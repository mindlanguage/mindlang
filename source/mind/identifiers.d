module mind.identifiers;

import mind.tokenizer;
import mind.parser;

string parseQualifiedIdentifier(ref Parser p) {
    import std.array : appender;

    auto builder = appender!string();
    auto first = p.expect(TokenType.Identifier);
    builder.put(first.lexeme);

    while (p.match(TokenType.Dot)) {
        auto nextPart = p.expect(TokenType.Identifier);
        builder.put(".");
        builder.put(nextPart.lexeme);
    }

    return builder.data;
}

string[] parseQualifiedIdentifierParts(ref Parser parser) {
    string[] parts;

    auto token = parser.expect(TokenType.Identifier);
    parts ~= token.lexeme;

    while (parser.peek().type == TokenType.Dot) {
        parser.next(); // skip dot
        auto next = parser.expect(TokenType.Identifier);
        parts ~= next.lexeme;
    }

    return parts;
}