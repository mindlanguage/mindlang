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

class TraitArgument {
    string name;
    string argument;

    this(string name, string argument) {
        this.name = name;
        this.argument = argument;
    }
}

class TraitEntry {
    TraitArgument[] entries;

    this(TraitArgument[] entries) {
        this.entries = entries ? entries : [];
    }
}

class TraitDecl {
    AccessModifier access;
    Attribute[] attributes;
    Token token;
    string name;
    string typeParameter;
    ReturnStatement statement;

    this(AccessModifier access, Attribute[] attributes, Token token, string name, string typeParameter, ReturnStatement statement) {
        this.access = access;
        this.attributes = attributes;
        this.token = token;
        this.name = name;
        this.typeParameter = typeParameter;
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

    string typeParameter;
    
    if (p.peek().type == TokenType.LParen) {
        p.next(); // consume '('
        
        auto t = p.expect(TokenType.Identifier);
        typeParameter = t.lexeme;

        p.expect(TokenType.RParen);
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

    return new TraitDecl(access, attributes, traitToken, name, typeParameter, statement);
}
