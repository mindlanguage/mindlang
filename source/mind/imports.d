module mind.imports;

import mind.keywords;
import mind.identifiers;
import mind.parser;
import mind.tokenizer;
import mind.access;

class ImportStatement {
    Token token;
    AccessModifier access;
    string aliasName;
    string moduleName;
    string[] members;

    this(AccessModifier access, Token token, string aliasName, string moduleName) {
        this.access = access;
        this.token = token;
        this.aliasName = aliasName;
        this.moduleName = moduleName;
        this.members = [];
    }
}

ImportStatement parseImportStatement(AccessModifier access, ref Parser parser) {
    auto token = parser.expect(TokenType.Identifier);
    auto firstToken = token;

    if (token.lexeme != Keywords.Import) {
        throw new Exception("Expected import statement.");
    }

    string aliasIdentifier;
    auto identifier = parseQualifiedIdentifier(parser);

    if (parser.peek().type == TokenType.Equal) {
        parser.next(); // skip =

        aliasIdentifier = identifier;
        identifier = parseQualifiedIdentifier(parser);
    }

    auto imp = new ImportStatement(access, firstToken, aliasIdentifier, identifier);

    if (parser.peek().type == TokenType.Colon) {
        parser.next(); // skip :

        while (true)
        {
            auto member = parseQualifiedIdentifier(parser);

            imp.members ~= member;

            if (parser.peek().type != TokenType.Comma) {
                break;
            }

            parser.next(); // skip ,
        }
    }

    return imp;
}
