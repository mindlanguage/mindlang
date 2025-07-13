module mind.enums;

import mind.keywords;
import mind.expressions;
import mind.errors;
import mind.access;
import mind.tokenizer;
import mind.ast;
import mind.parser;
import mind.types;

class EnumValue {
    Token nameToken;     // Token for the enum name
    string name;         // Name of the enum value
    Expr value;          // Optional initializer (can be null)

    this(Token nameToken, string name, Expr value = null) {
        this.nameToken = nameToken;
        this.name = name;
        this.value = value;
    }
}

class EnumDecl {
    AccessModifier access;
    Token token;
    string name;
    TypeReference backingType;
    EnumValue[] values;

    this(AccessModifier access, Token token, string name, TypeReference backingType, EnumValue[] values) {
        this.access = access;
        this.token = token;
        this.name = name;
        this.backingType = backingType;
        this.values = values;
    }
}

EnumDecl parseEnumDeclaration(AccessModifier access, ref Parser parser) {
    // Consume 'enum' keyword
    auto enumToken = parser.expect(TokenType.Identifier);
    enforceCompilerException(enumToken.lexeme == "enum", "Expected 'enum'", enumToken);

    // Parse name
    auto nameToken = parser.expect(TokenType.Identifier);
    string enumName = nameToken.lexeme;

    // If next token is '=' it means it's a variable declaration, not an enum
    if (parser.peek().type == TokenType.Equal) {
        parser.pos -= 2;
        return null;
    }

    // Optional backing type
    TypeReference backingType = null;
    if (parser.match(TokenType.Colon)) {
        backingType = parseTypeReference(parser);
    }

    // Opening brace
    parser.expect(TokenType.LBrace);

    // Parse enum values
    EnumValue[] values;
    bool expectComma = false;

    // Track used names to prevent duplicates
    string[string] usedNames;

    while (!parser.check(TokenType.RBrace) && !parser.isEOF()) {
        if (expectComma) {
            parser.expect(TokenType.Comma);
        }

        // Enum value name
        auto valueToken = parser.expect(TokenType.Identifier);
        string valueName = valueToken.lexeme;

        // Check for duplicates
        if (usedNames.get(valueName, null) !is null) {
            throw new CompilerException("Duplicate enum value name '" ~ valueName ~ "' in enum '" ~ enumName ~ "'.", valueToken);
        }
        usedNames[valueName] = "";

        // Optional assignment
        Expr expr = null;
        if (parser.match(TokenType.Equal)) {
            expr = parseExpression(parser);
        }

        values ~= new EnumValue(valueToken, valueName, expr);

        expectComma = true;

        // Allow trailing comma before closing brace
        if (parser.check(TokenType.RBrace))
            break;
    }

    parser.expect(TokenType.RBrace);

    return new EnumDecl(access, enumToken, enumName, backingType, values);
}