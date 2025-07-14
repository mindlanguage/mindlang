module mind.properties;

import mind.statements;
import mind.tokenizer;
import mind.types;
import mind.functions;
import mind.expressions;
import mind.ast;
import mind.parser;
import mind.errors;
import mind.access;
import mind.keywords;
import mind.attributes;

class PropStatement {
    Attribute[] attributes;
    AccessModifier access;
    Token token;
    string name;
    TypeReference type;
    Expr getExpr;
    LRStatement setLR;
    Statement[] getBody;
    Statement[] setBody;
    string setterParam;
    bool hasEmptyGetter;
    bool hasEmptySetter;

    this(Attribute[] attributes, AccessModifier access, Token token, string name, TypeReference type,
         Expr getExpr = null,
         LRStatement setLR = null,
         Statement[] getBody = null,
         Statement[] setBody = null,
         string setterParam = null,
         bool hasEmptyGetter = false,
         bool hasEmptySetter = false) {
        this.attributes = attributes;
        this.access = access;
        this.token = token;
        this.name = name;
        this.type = type;
        this.getExpr = getExpr;
        this.setLR = setLR;
        this.getBody = getBody;
        this.setBody = setBody;
        this.setterParam = setterParam;
        this.hasEmptyGetter = hasEmptyGetter;
        this.hasEmptySetter = hasEmptySetter;
    }
}

PropStatement parsePropertyStatement(Attribute[] attributes, AccessModifier access, ref Parser p) {
    auto propToken = p.expect(TokenType.Identifier);
    if (propToken.lexeme != Keywords.Prop)
        throw new CompilerException("Expected 'prop' keyword.", propToken);

    auto nameToken = p.expect(TokenType.Identifier);
    string name = nameToken.lexeme;

    TypeReference type = null;

    // Optional type
    if (p.match(TokenType.Colon)) {
        type = parseTypeReference(p);
    }

    // Shorthand syntax: prop NAME => EXPR;
    if (p.match(TokenType.EqualsArrow)) {
        auto expr = parseExpression(p);
        p.expect(TokenType.Semicolon);
        return new PropStatement(attributes, access, nameToken, name, type, expr);
    }

    // Auto-property: prop NAME: TYPE;
    if (p.match(TokenType.Semicolon)) {
        return new PropStatement(attributes, access, nameToken, name, type);
    }

    p.expect(TokenType.LBrace);

    Expr getExpr = null;
    LRStatement setLR = null;
    Statement[] getBody = null;
    Statement[] setBody = null;
    string setterParam = null;

    bool foundGet = false;
    bool foundSet = false;
    bool hasEmptyGetter = false;
    bool hasEmptySetter = false;

    while (!p.check(TokenType.RBrace)) {
        auto nextToken = p.peek();
        if (nextToken.type != TokenType.Identifier) {
            throw new CompilerException("Expected 'get' or 'set' inside property, but got: " ~ nextToken.lexeme, nextToken);
        }

        auto keyword = p.expect(TokenType.Identifier);

        if (keyword.lexeme == Keywords.Get) {
            if (foundGet)
                throw new CompilerException("Duplicate 'get' definition.", keyword);
            foundGet = true;

            if (p.match(TokenType.EqualsArrow)) {
                getExpr = parseExpression(p);
                p.expect(TokenType.Semicolon);
            }
            else if (p.match(TokenType.LBrace)) {
                getBody = parseStatements(TokenType.RBrace, p);
                p.expect(TokenType.RBrace);
            }
            else if (p.match(TokenType.Semicolon)) {
                // Empty get;
                hasEmptyGetter = true;
            }
            else {
                throw new CompilerException("Expected '=>', '{', or ';' after 'get'.", keyword);
            }
        }
        else if (keyword.lexeme == Keywords.Set) {
            if (foundSet)
                throw new CompilerException("Duplicate 'set' definition.", keyword);
            foundSet = true;

            if (p.match(TokenType.EqualsArrow)) {
                auto token = p.peek();
                auto leftExpr = parseExpression(p);

                auto op = p.next();

                bool isLROp =
                    op.type == TokenType.Equal || op.type == TokenType.PlusEqual ||
                    op.type == TokenType.MinusEqual || op.type == TokenType.MulEqual ||
                    op.type == TokenType.DivEqual || op.type == TokenType.ModEqual ||
                    op.type == TokenType.OrEqual || op.type == TokenType.AndEqual ||
                    op.type == TokenType.XorEqual || op.type == TokenType.LeftShiftAssign ||
                    op.type == TokenType.DoubleShiftAssign;

                if (!isLROp)
                    throw new CompilerException("Expected assignment operator after expression in 'set =>'.", op);

                auto rhs = parseExpression(p);
                p.expect(TokenType.Semicolon);

                setLR = new LRStatement(token, leftExpr, op, rhs);
            }
            else if (p.match(TokenType.LParen)) {
                auto paramName = p.expect(TokenType.Identifier);
                setterParam = paramName.lexeme;
                p.expect(TokenType.RParen);
                p.expect(TokenType.LBrace);
                setBody = parseStatements(TokenType.RBrace, p);
                p.expect(TokenType.RBrace);
            }
            else if (p.match(TokenType.LBrace)) {
                setBody = parseStatements(TokenType.RBrace, p);
                p.expect(TokenType.RBrace);
            }
            else if (p.match(TokenType.Semicolon)) {
                // Empty set;
                hasEmptySetter = true;
            }
            else {
                throw new CompilerException("Expected '=>', '(', '{', or ';' after 'set'.", keyword);
            }
        }
        else {
            throw new CompilerException("Expected 'get' or 'set' inside property.", keyword);
        }
    }

    p.expect(TokenType.RBrace);

    return new PropStatement(attributes, access, propToken, name, type, getExpr, setLR, getBody, setBody, setterParam, hasEmptyGetter, hasEmptySetter);
}
