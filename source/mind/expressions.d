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
import std.algorithm : map;

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

        left = new BinaryExpr(left, op, right, tok);
    }
    return left;
}

Expr parseCallOrPrimary(ref Parser p) {
    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.Cast) {
        auto tok = p.next();
        p.expect(TokenType.LParen);
        auto typeExpr = parseTypeExpr(p);
        p.expect(TokenType.RParen);
        auto exprToCast = parseCallOrPrimary(p);
        return new CastExpr(typeExpr, exprToCast, tok);
    }

    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.Fn) {
        return parseLambdaExpression(p);
    }

    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.New) {
        auto tok = p.next();
        auto typeExpr = parseTemplatedType(p);
        p.expect(TokenType.LParen);
        auto args = parseArgumentList(p);
        return new NewExpr(typeExpr, args, tok);
    }

    Expr expr;

    auto peeked = p.peek();
    if (peeked.type == TokenType.Identifier) {
        auto identTok = p.next();
        expr = new IdentifierExpr(identTok.lexeme, identTok);

        // Check immediately for ! to handle cases like sizeof!char
        if (p.peek().type == TokenType.Exclamation) {
            bool forceType = (cast(IdentifierExpr) expr).name == Keywords.Sizeof;

            import std.stdio : writefln;

            while (p.match(TokenType.Exclamation)) {
                auto bangTok = p.peekLast();
                if (p.match(TokenType.LParen)) {
                    Expr[] templateArgs;
                    if (!p.match(TokenType.RParen)) {
                        while (true) {
                            if (forceType) {
                                templateArgs ~= parseTypeExpr(p);
                            } else {
                                auto checkpoint = p.save();
                                try {
                                    templateArgs ~= parseTypeExpr(p);
                                } catch (Exception) {
                                    p.restore(checkpoint);
                                    templateArgs ~= parseExpression(p);
                                }
                            }
                            if (p.match(TokenType.RParen)) break;
                            p.expect(TokenType.Comma);
                        }
                    }
                    expr = new TemplatedExpr(expr, templateArgs, bangTok);
                } else {
                    auto checkpoint = p.save();
                    try {
                        if (forceType) {
                            auto typeArg = parseTypeExpr(p); // it gets here and typeArg is actually TypeExpr
                            expr = new TemplatedExpr(expr, [typeArg], bangTok);
                        } else {
                            auto fallback = parseExpression(p);
                            expr = new TemplatedExpr(expr, [fallback], bangTok);
                        }
                    } catch (Exception) {
                        p.restore(checkpoint);
                        auto argTok = p.expect(TokenType.Identifier);
                        auto argExpr = new IdentifierExpr(argTok.lexeme, argTok);
                        expr = new TemplatedExpr(expr, [argExpr], argTok);
                    }
                }
            }
        }
    } else {
        expr = parsePrimary(p);
    }

    while (true) {
        bool progressed = false;
        auto tok = p.peek();

        if (p.match(TokenType.Exclamation)) {
            progressed = true;
            bool forceType = false;
            if (auto ident = cast(IdentifierExpr) expr) {
                forceType = ident.name == Keywords.Sizeof;
            }

            if (p.match(TokenType.LParen)) {
                Expr[] templateArgs;
                if (!p.match(TokenType.RParen)) {
                    while (true) {
                        if (forceType) {
                            templateArgs ~= parseTypeExpr(p);
                        } else {
                            auto checkpoint = p.save();
                            try {
                                templateArgs ~= parseTypeExpr(p);
                            } catch (Exception) {
                                p.restore(checkpoint);
                                templateArgs ~= parseExpression(p);
                            }
                        }
                        if (p.match(TokenType.RParen)) break;
                        p.expect(TokenType.Comma);
                    }
                }
                expr = new TemplatedExpr(expr, templateArgs, tok);
            } else {
                auto checkpoint = p.save();
                try {
                    if (forceType) {
                        auto typeArg = parseTypeExpr(p);
                        expr = new TemplatedExpr(expr, [typeArg], tok);
                    } else {
                        auto fallback = parseExpression(p);
                        expr = new TemplatedExpr(expr, [fallback], tok);
                    }
                } catch (Exception) {
                    p.restore(checkpoint);
                    auto identTok = p.expect(TokenType.Identifier);
                    auto identExpr = new IdentifierExpr(identTok.lexeme, identTok);
                    expr = new TemplatedExpr(expr, [identExpr], identTok);
                }
            }
        }
        else if (p.match(TokenType.Dot)) {
            progressed = true;
            string qualifiedName = parseQualifiedIdentifier(p);
            auto memberExpr = new IdentifierExpr(qualifiedName, tok);
            expr = new QualifiedAccessExpr(expr, memberExpr, tok);
        }
        else if (p.match(TokenType.LParen)) {
            progressed = true;
            auto args = parseArgumentList(p);
            expr = new CallExpr(expr, args, tok);
        }
        else if (p.match(TokenType.LBracket)) {
            progressed = true;
            auto indexExpr = parseExpression(p);
            p.expect(TokenType.RBracket);
            expr = new ArrayIndexExpr(expr, indexExpr, tok);
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
    auto baseQualifiedToken = p.peek();
    string baseQualified = parseQualifiedIdentifier(p);
    Expr expr = new IdentifierExpr(baseQualified, baseQualifiedToken);

    bool forceType = false;
    if (auto ident = cast(IdentifierExpr) expr) {
        forceType = ident.name == Keywords.Sizeof;
    }

    while (p.match(TokenType.Exclamation)) {
        if (p.match(TokenType.LParen)) {
            Expr[] templateArgs;
            if (!p.match(TokenType.RParen)) {
                while (true) {
                    if (forceType) {
                        templateArgs ~= parseTypeExpr(p);
                    } else {
                        auto checkpoint = p.save();
                        try {
                            templateArgs ~= parseTypeExpr(p);
                        } catch (Exception) {
                            p.restore(checkpoint);
                            templateArgs ~= parseExpression(p);
                        }
                    }
                    if (p.match(TokenType.RParen))
                        break;
                    p.expect(TokenType.Comma);
                }
            }
            expr = new TemplatedExpr(expr, templateArgs, baseQualifiedToken);
        } else {
            auto checkpoint = p.save();
            try {
                if (forceType) {
                    auto typeArg = parseTypeExpr(p);
                    expr = new TemplatedExpr(expr, [typeArg], baseQualifiedToken);
                } else {
                    auto fallback = parseExpression(p);
                    expr = new TemplatedExpr(expr, [fallback], baseQualifiedToken);
                }
            } catch (Exception) {
                p.restore(checkpoint);
                auto identTok = p.expect(TokenType.Identifier);
                auto identExpr = new IdentifierExpr(identTok.lexeme, identTok);
                expr = new TemplatedExpr(expr, [identExpr], identTok);
            }
        }
    }

    while (p.match(TokenType.Dot)) {
        string nextQualified = parseQualifiedIdentifier(p);
        expr = new QualifiedAccessExpr(expr, new IdentifierExpr(nextQualified, baseQualifiedToken), baseQualifiedToken);
    }

    // HERE: Wrap the entire expr as a TypeExpr before returning!
    if (cast(TypeExpr) expr is null) {
        expr = new TypeExpr(expr, baseQualifiedToken);
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
        auto tok = p.next(); // consume InterpolatedStringStart token

        Expr[] parts;

        while (true) {
            auto current = p.peek();

            // Plain text part inside the interpolated string
            if (current.type == TokenType.InterpolatedStringText) {
                p.next();
                parts ~= new LiteralExpr(current.lexeme, current);
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

        return new InterpolatedStringExpr(parts, tok);
    }

    // Cast expression: cast(Type) expr
    if (t.type == TokenType.Identifier && t.lexeme == Keywords.Cast) {
        p.next(); // consume 'cast'
        p.expect(TokenType.LParen);
        auto typeExpr = parseTemplatedType(p);
        p.expect(TokenType.RParen);
        auto exprToCast = parsePrimary(p);
        return new CastExpr(typeExpr, exprToCast, t);
    }

    // New expression: new Type(args)
    if (t.type == TokenType.Identifier && t.lexeme == Keywords.New) {
        p.next(); // consume 'new'
        auto typeExpr = parseTemplatedType(p);
        p.expect(TokenType.LParen);
        auto args = parseArgumentList(p);
        return new NewExpr(typeExpr, args, t);
    }

    // **Switch expression support**
    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.Switch) {
        return parseSwitchExpr(p);
    }

    if (t.type == TokenType.NumberLiteral ||
        t.type == TokenType.StringLiteral ||
        t.type == TokenType.CharLiteral) {
        p.next();
        return new LiteralExpr(t.lexeme, t);
    }

    if (t.type == TokenType.Identifier) {
        auto tok = p.next();
        return new IdentifierExpr(tok.lexeme, tok);
    }

    if (p.match(TokenType.LParen)) {
        auto expr = parseExpression(p);
        p.expect(TokenType.RParen);
        return new GroupingExpr(expr, t);
    }

    if (p.match(TokenType.LBracket)) {
        Expr[] elements;
        if (p.match(TokenType.RBracket)) {
            return new ListExpr(elements, t);
        }
        while (true) {
            elements ~= parseExpression(p);
            if (p.match(TokenType.RBracket)) {
                break;
            }
            p.expect(TokenType.Comma);
        }
        return new ListExpr(elements, t);
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
        return new UnaryExpr(tok.lexeme, operand, tok, false); // prefix
    }

    // Otherwise, parse a postfix-capable expression
    auto expr = parseCallOrPrimary(p);

    // Handle postfix ++ / --
    auto post = p.peek();
    if (post.lexeme == "++" || post.lexeme == "--") {
        p.next(); // consume postfix operator
        return new UnaryExpr(post.lexeme, expr, tok, true); // postfix
    }

    return expr;
}

Expr parseTypeExpr(ref Parser p) {
    auto expr = parseTemplatedType(p);

    // If we already wrapped it as TypeExpr somewhere else, avoid double wrapping
    if (cast(TypeExpr) expr !is null)
        return expr;

    return new TypeExpr(expr, expr.token);
}

string flattenExpression(Expr e) {
    import std.array : join;

    if (e is null)
        return "<null>";

    auto id = cast(IdentifierExpr) e;
    if (id !is null)
        return id.name;

    auto lit = cast(LiteralExpr) e;
    if (lit !is null)
        return lit.value;

    auto bin = cast(BinaryExpr) e;
    if (bin !is null)
        return flattenExpression(bin.left) ~ bin.op ~ flattenExpression(bin.right);

    auto un = cast(UnaryExpr) e;
    if (un !is null) {
        if (un.isPostfix)
            return flattenExpression(un.operand) ~ un.op;
        else
            return un.op ~ flattenExpression(un.operand);
    }

    auto call = cast(CallExpr) e;
    if (call !is null) {
        auto args = call.arguments.map!(a => flattenExpression(a)).join(",");
        return flattenExpression(call.callee) ~ "(" ~ args ~ ")";
    }

    auto list = cast(ListExpr) e;
    if (list !is null) {
        auto elements = list.elements.map!(a => flattenExpression(a)).join(",");
        return "[" ~ elements ~ "]";
    }

    auto group = cast(GroupingExpr) e;
    if (group !is null)
        return "(" ~ flattenExpression(group.expression) ~ ")";

    auto castExpr = cast(CastExpr) e;
    if (castExpr !is null) {
        // C-style cast: (Type)expr
        return "(" ~ flattenExpression(castExpr.targetType) ~ ")" ~ flattenExpression(castExpr.expr);
    }

    auto templ = cast(TemplatedExpr) e;
    if (templ !is null) {
        auto tmplArgs = templ.templateArgs.map!(a => flattenExpression(a)).join(",");
        return flattenExpression(templ.target) ~ "!(" ~ tmplArgs ~ ")";
    }

    auto n = cast(NewExpr) e;
    if (n !is null) {
        auto newArgs = n.arguments.map!(a => flattenExpression(a)).join(",");
        return "new " ~ flattenExpression(n.typeExpr) ~ "(" ~ newArgs ~ ")";
    }

    auto interp = cast(InterpolatedStringExpr) e;
    if (interp !is null) {
        string result = `"`;
        foreach (part; interp.parts) {
            auto litPart = cast(LiteralExpr) part;
            if (litPart !is null)
                result ~= litPart.value;
            else
                result ~= "{" ~ flattenExpression(part) ~ "}";
        }
        return result ~ `"`;
    }

    auto sw = cast(SwitchExpr) e;
    if (sw !is null) {
        string casesStr;
        foreach (caseExpr; sw.cases) {
            casesStr ~= "case " ~ flattenExpression(caseExpr.value) ~ ": " ~ flattenExpression(caseExpr.body) ~ "; ";
        }
        if (sw.defaultCase !is null) {
            casesStr ~= "default: " ~ flattenExpression(sw.defaultCase.body) ~ "; ";
        }
        return "switch(" ~ flattenExpression(sw.condition) ~ ") { " ~ casesStr ~ "}";
    }

    auto qual = cast(QualifiedAccessExpr) e;
    if (qual !is null)
        return flattenExpression(qual.target) ~ "." ~ flattenExpression(qual.member);

    auto lambda = cast(LambdaExpr) e;
    if (lambda !is null) {
        string params = lambda.paramNames.join(", ");
        string bodyStr;
        if (lambda.bodyStatements.length > 0) {
            bodyStr = "{ ... }"; // Simplified for now
        } else if (lambda.bodyExpression !is null) {
            bodyStr = flattenExpression(lambda.bodyExpression);
        } else {
            bodyStr = "{}";
        }
        return "fn(" ~ params ~ ") " ~ bodyStr;
    }

    auto arr = cast(ArrayIndexExpr) e;
    if (arr !is null)
        return flattenExpression(arr.arrayExpr) ~ "[" ~ flattenExpression(arr.indexExpr) ~ "]";

    auto typeExpr = cast(TypeExpr) e;
    if (typeExpr !is null)
        return flattenExpression(typeExpr.innerType);

    // Unknown expression type
    return "<unknown_expr>";
}