module mind.aliases;

import mind.parser;
import mind.tokenizer;
import mind.access;
import mind.keywords;
import mind.types;
import mind.errors;

class AliasStatement {
    AccessModifier access;
    Token token;
    string name;
    TypeReference type;

    this(AccessModifier access, Token token, string name, TypeReference type) {
        this.access = access;
        this.token = token;
        this.name = name;
        this.type = type;
    }
}

AliasStatement parseAliasStatement(AccessModifier access, ref Parser parser) {
    auto aliasToken = parser.expect(TokenType.Identifier);

    if (aliasToken.lexeme != Keywords.Alias) {
        throw new CompilerException("Expected 'alias' keyword.", aliasToken);
    }

    auto nameToken = parser.expect(TokenType.Identifier);
    string aliasName = nameToken.lexeme;

    parser.expect(TokenType.Equal);

    auto type = parseTypeReference(parser); // Now supports D-style templates via !

    return new AliasStatement(access, aliasToken, aliasName, type);
}