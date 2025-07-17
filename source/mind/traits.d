module mind.traits;

import mind.parser;
import mind.tokenizer;
import mind.errors;
import mind.access;
import mind.attributes;
import mind.statements;
import mind.ast;
import mind.expressions;
import mind.functions;
import mind.keywords;

class TraitEntry {
    string[] entries;

    this(string[] entries) {
        this.entries = entries ? entries : [];
    }
}

class TraitDecl {
    AccessModifier access;
    Attribute[] attributes;
    Token token;
    string name;
    string[] typeParameters;
    ReturnStatement statement;

    this(AccessModifier access, Attribute[] attributes, Token token, string name, string[] typeParameters, ReturnStatement statement) {
        this.access = access;
        this.attributes = attributes;
        this.token = token;
        this.name = name;
        this.typeParameters = typeParameters;
        this.statement = statement;
    }
}

TraitDecl parseTraitDeclaration(Attribute[] attributes, AccessModifier access, ref Parser p) {
    auto traitToken = p.expect(TokenType.Identifier);

    if (traitToken.lexeme != Keywords.Trait)
        throw new CompilerException("Expected 'trait' keyword.", traitToken);

    auto nameToken = p.expect(TokenType.Identifier);
    string name = nameToken.lexeme;

    if (isKeyword(nameToken.lexeme)) {
        throw new CompilerException("Cannot use keyword as identifier.", nameToken);
    }

    string[] typeParameters;
    bool[string] typeMap;
    if (p.peek().type == TokenType.LParen) {
        p.next(); // consume '('
        while (true) {
            auto t = p.expect(TokenType.Identifier);
            typeParameters ~= t.lexeme;
            
            if (t.lexeme in typeMap) {
                throw new CompilerException("Duplicate type name.", t);
            }
            
            typeMap[t.lexeme] = true;

            if (p.peek().type == TokenType.RParen) {
                p.next(); // consume ')'
                break;
            }
            p.expect(TokenType.Comma);
        }
    }

    ReturnStatement statement;
    
    if (p.peek().type == TokenType.EqualsArrow) {
        p.next(); // consume =>
        statement = parseReturnStatement(false, p);
        p.expect(TokenType.Semicolon);
    } else if (p.peek().type == TokenType.LBrace) {
        p.next(); // consume {
        auto statementToken = p.peek();
        auto statements = parseStatements(TokenType.RBrace, p);

        enforceCompilerException(statements && statements.length == 1,
            "Traits can only have a single statement", statementToken);

        statement = cast(ReturnStatement)statements[0];

        p.expect(TokenType.RBrace);
    }

    enforceCompilerException(statement !is null,
        "Missing return statement in trait.", traitToken);

    return new TraitDecl(access, attributes, traitToken, name, typeParameters, statement);
}
