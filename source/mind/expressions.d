module mind.expressions;

import mind.parser;
import mind.ast;
import mind.tokenizer;
import mind.keywords;
import mind.errors;
import mind.statements;
import mind.identifiers;
import mind.functions;

import std.exception : enforce;
import std.conv : to;

Expr parseExpression(ref Parser p) {
    auto expr = parseBinaryExpression(p, 0);

    auto last = p.peekLast();
    
    // Sanity check: ensure next token is not the start of another expression
    auto next = p.peek();

    // Valid tokens that may follow an expression in typical contexts:
    bool isValidContinuation =
        next.type == TokenType.Comma ||
        next.type == TokenType.RParen ||
        next.type == TokenType.RBracket ||
        next.type == TokenType.Semicolon ||
        next.type == TokenType.Newline ||
        next.type == TokenType.Colon;

    if (!isValidContinuation) {
        // If the next token looks like the start of a new expression, throw error
        if (next.type == TokenType.Identifier ||
            next.type == TokenType.NumberLiteral ||
            next.type == TokenType.StringLiteral ||
            next.type == TokenType.CharLiteral ||
            next.type == TokenType.LParen ||
            next.type == TokenType.LBracket ||
            next.type == TokenType.MultilineStringLiteral ||
            next.type == TokenType.InterpolatedStringStart) {
            throw new CompilerException("Unexpected token '" ~ next.lexeme ~ "' after expression: '" ~ last.lexeme ~ "'", next);
        }
    }

    return expr;
}

immutable string[][] precedences = [
    ["||"],                        // logical OR (lowest)
    ["&&"],                        // logical AND
    ["|"],                         // bitwise OR
    ["^"],                         // bitwise XOR
    ["&"],                         // bitwise AND
    ["==", "!="],                  // equality
    ["<", ">", "<=", ">="],        // comparisons
    ["<<", ">>"],                  // bitwise shifts
    ["+", "-"],                    // additive
    ["*", "/", "%"]               // multiplicative (highest)
];

int getPrecedence(string op) {
    foreach (i, level; precedences) {
        foreach (s; level) {
            if (op == s)
                return cast(int)i;
        }
    }
    return -1;
}

Expr parseBinaryExpression(ref Parser p, int parentPrecedence) {
    auto left = parseUnaryExpression(p);

    while (true) {
        auto tok = p.peek();

        int prec = getPrecedence(tok.lexeme);
        if (prec < 0 || prec < parentPrecedence)
            break;

        string op = tok.lexeme;
        p.next();

        auto right = parseBinaryExpression(p, prec + 1);

        left = new BinaryExpr(left, op, right);
    }
    return left;
}

Expr parseCallOrPrimary(ref Parser p) {
    // Handle cast first
    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.Cast) {
        p.next(); // consume 'cast'
        p.expect(TokenType.LParen);

        auto typeExpr = parseTemplatedType(p);

        p.expect(TokenType.RParen);
        auto exprToCast = parseCallOrPrimary(p);
        return new CastExpr(typeExpr, exprToCast);
    }

    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.Fn) {
        return parseLambdaExpression(p);
    }

    // Handle 'new' expressions:
    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.New) {
        p.next(); // consume 'new'

        auto typeExpr = parseTemplatedType(p);

        p.expect(TokenType.LParen);
        auto args = parseArgumentList(p);

        return new NewExpr(typeExpr, args);
    }

    auto expr = parsePrimary(p);

    // Now loop to parse any combination of:
    // 1) templated instantiations
    // 2) member accesses
    // 3) function calls
    // 4) array indexing
    while (true) {
        bool progressed = false;

        // templated instantiation
        if (p.match(TokenType.Exclamation)) {
            progressed = true;
            if (p.match(TokenType.LParen)) {
                Expr[] templateArgs;
                if (!p.match(TokenType.RParen)) {
                    while (true) {
                        templateArgs ~= parseExpression(p);
                        if (p.match(TokenType.RParen)) break;
                        p.expect(TokenType.Comma);
                    }
                }
                expr = new TemplatedExpr(expr, templateArgs);
            } else {
                auto tok = p.expect(TokenType.Identifier);
                auto identExpr = new IdentifierExpr(tok.lexeme);
                expr = new TemplatedExpr(expr, [identExpr]);
            }
        }
        // member access
        else if (p.match(TokenType.Dot)) {
            progressed = true;
            string qualifiedName = parseQualifiedIdentifier(p);
            auto memberExpr = new IdentifierExpr(qualifiedName);
            expr = new QualifiedAccessExpr(expr, memberExpr);
        }
        // function call
        else if (p.match(TokenType.LParen)) {
            progressed = true;
            auto args = parseArgumentList(p);
            expr = new CallExpr(expr, args);
        }
        // array indexing
        else if (p.match(TokenType.LBracket)) {
            progressed = true;
            auto indexExpr = parseExpression(p);
            p.expect(TokenType.RBracket);
            expr = new ArrayIndexExpr(expr, indexExpr);
        }

        if (!progressed) break;
    }

    return expr;
}

Expr parseLambdaExpression(ref Parser p) {
    auto fnToken = p.expect(TokenType.Identifier);
    enforce(fnToken.lexeme == Keywords.Fn);

    string[] paramNames;

    if (p.peek().type == TokenType.LParen) {
        p.expect(TokenType.LParen);
        if (p.peek().type != TokenType.RParen) {
            while (true) {
                auto param = p.expect(TokenType.Identifier);
                paramNames ~= param.lexeme;

                if (p.match(TokenType.RParen)) break;
                p.expect(TokenType.Comma);
            }
        } else {
            p.expect(TokenType.RParen);
        }
    }

    // Now parse the body: either => expr or { statements }
    if (p.match(TokenType.EqualsArrow)) {
        auto exprBody = parseExpression(p);
        return new LambdaExpr(fnToken, paramNames, null, exprBody);
    } else if (p.match(TokenType.LBrace)) {
        auto stmts = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
        return new LambdaExpr(fnToken, paramNames, stmts, null);
    } else {
        throw new CompilerException("Expected '=>' or '{' after lambda parameters or 'fn'.", p.peek());
    }
}

Expr parseTemplatedType(ref Parser p) {
    // Parse the base identifier or qualified identifier (like a.b.c)
    string baseQualified = parseQualifiedIdentifier(p);
    Expr expr = new IdentifierExpr(baseQualified);

    // Parse templated suffixes (!b or !(expr,...))
    while (p.match(TokenType.Exclamation)) {
        if (p.match(TokenType.LParen)) {
            Expr[] templateArgs;
            if (!p.match(TokenType.RParen)) {
                while (true) {
                    templateArgs ~= parseExpression(p);
                    if (p.match(TokenType.RParen))
                        break;
                    p.expect(TokenType.Comma);
                }
            }
            expr = new TemplatedExpr(expr, templateArgs);
        } else {
            auto tok = p.expect(TokenType.Identifier);
            auto identExpr = new IdentifierExpr(tok.lexeme);
            expr = new TemplatedExpr(expr, [identExpr]);
        }
    }

    // Parse trailing dotted qualified identifiers after templated expression
    while (p.match(TokenType.Dot)) {
        string nextQualified = parseQualifiedIdentifier(p);
        expr = new QualifiedAccessExpr(expr, new IdentifierExpr(nextQualified));
    }

    return expr;
}

Expr[] parseArgumentList(ref Parser p) {
    Expr[] args;

    if (p.match(TokenType.RParen)) {
        return args;
    }

    while (true) {
        args ~= parseExpression(p);

        if (p.match(TokenType.RParen)) {
            break;
        }
        p.expect(TokenType.Comma);
    }

    return args;
}

Expr parsePrimary(ref Parser p) {
    auto t = p.peek();

    // Handle interpolated strings like $"Hello {name}"
    if (t.type == TokenType.InterpolatedStringStart) {
        p.next(); // consume InterpolatedStringStart token

        Expr[] parts;

        while (true) {
            auto current = p.peek();

            // Plain text part inside the interpolated string
            if (current.type == TokenType.InterpolatedStringText) {
                p.next();
                parts ~= new LiteralExpr(current.lexeme);
            }
            // Start of embedded expression block
            else if (current.type == TokenType.LBrace) {
                p.next(); // consume '{'

                auto expr = parseExpression(p);

                p.expect(TokenType.RBrace); // consume '}'

                parts ~= expr;
            }
            // End of interpolated string
            else if (current.type == TokenType.InterpolatedStringEnd) {
                p.next(); // consume closing quote token
                break;
            }
            else {
                throw new CompilerException("Unexpected token inside interpolated string: '" ~ current.lexeme ~ "'", current);
            }
        }

        return new InterpolatedStringExpr(parts);
    }

    // Cast expression: cast(Type) expr
    if (t.type == TokenType.Identifier && t.lexeme == Keywords.Cast) {
        p.next(); // consume 'cast'
        p.expect(TokenType.LParen);
        auto typeExpr = parseTemplatedType(p);
        p.expect(TokenType.RParen);
        auto exprToCast = parsePrimary(p);
        return new CastExpr(typeExpr, exprToCast);
    }

    // New expression: new Type(args)
    if (t.type == TokenType.Identifier && t.lexeme == Keywords.New) {
        p.next(); // consume 'new'
        auto typeExpr = parseTemplatedType(p);
        p.expect(TokenType.LParen);
        auto args = parseArgumentList(p);
        return new NewExpr(typeExpr, args);
    }

    // **Switch expression support**
    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.Switch) {
        return parseSwitchExpr(p);
    }

    if (t.type == TokenType.NumberLiteral ||
        t.type == TokenType.StringLiteral ||
        t.type == TokenType.CharLiteral) {
        p.next();
        return new LiteralExpr(t.lexeme);
    }

    if (t.type == TokenType.Identifier) {
        // parse full qualified identifier (with dots)
        string qualifiedName = parseQualifiedIdentifier(p);
        return new IdentifierExpr(qualifiedName);
    }

    if (p.match(TokenType.LParen)) {
        auto expr = parseExpression(p);
        p.expect(TokenType.RParen);
        return new GroupingExpr(expr);
    }

    if (p.match(TokenType.LBracket)) {
        Expr[] elements;
        if (p.match(TokenType.RBracket)) {
            return new ListExpr(elements);
        }
        while (true) {
            elements ~= parseExpression(p);
            if (p.match(TokenType.RBracket)) {
                break;
            }
            p.expect(TokenType.Comma);
        }
        return new ListExpr(elements);
    }

    throw new CompilerException("Unexpected token.", t);
}

Expr parseSwitchExpr(ref Parser p) {
    auto switchToken = p.expect(TokenType.Identifier);
    if (switchToken.lexeme != Keywords.Switch)
        throw new CompilerException("Expected 'switch' keyword.", switchToken);

    p.expect(TokenType.LParen);
    auto condition = parseExpression(p);
    p.expect(TokenType.RParen);

    p.expect(TokenType.LBrace);

    CaseExpr[] cases;
    DefaultExpr defaultCase = null;

    while (p.peek().type != TokenType.RBrace) {
        auto current = p.peek();

        if (current.type == TokenType.Identifier && current.lexeme == Keywords.Case) {
            auto caseToken = p.next();
            auto value = parseExpression(p);

            if (!p.match(TokenType.Colon) && !p.match(TokenType.EqualsArrow))
                throw new CompilerException("Expected ':' or '=>' after case expression.", p.peek());

            auto body = parseExpression(p);

            // Optional semicolon after case expression body, consume if present
            if (p.peek().type == TokenType.Semicolon)
                p.next();

            cases ~= new CaseExpr(caseToken, value, body);
        }
        else if (current.type == TokenType.Identifier && current.lexeme == Keywords.Default) {
            auto defaultToken = p.next();

            if (!p.match(TokenType.Colon) && !p.match(TokenType.EqualsArrow))
                throw new CompilerException("Expected ':' or '=>' after 'default'.", p.peek());

            auto body = parseExpression(p);

            // Optional semicolon after default expression body
            if (p.peek().type == TokenType.Semicolon)
                p.next();

            defaultCase = new DefaultExpr(defaultToken, body);
        }
        else {
            throw new CompilerException("Expected 'case' or 'default' in switch expression.", current);
        }
    }

    p.expect(TokenType.RBrace);

    return new SwitchExpr(switchToken, condition, cases, defaultCase);
}

Expr parseUnaryExpression(ref Parser p) {
    auto tok = p.peek();

    // Handle prefix unary ops
    if (tok.lexeme == "+" || tok.lexeme == "-" ||
        tok.lexeme == "!" || tok.lexeme == "~" ||
        tok.lexeme == "++" || tok.lexeme == "--" ||
        tok.lexeme == "&" || tok.lexeme == "*") {
        
        p.next(); // consume the operator
        auto operand = parseUnaryExpression(p);
        return new UnaryExpr(tok.lexeme, operand, false); // prefix
    }

    // Otherwise, parse a postfix-capable expression
    auto expr = parseCallOrPrimary(p);

    // Handle postfix ++ / --
    auto post = p.peek();
    if (post.lexeme == "++" || post.lexeme == "--") {
        p.next(); // consume postfix operator
        return new UnaryExpr(post.lexeme, expr, true); // postfix
    }

    return expr;
}
