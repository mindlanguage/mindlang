module mind.includes;

import std.string : strip, replace;
import std.conv : to;
import std.exception : enforce;

import mind.tokenizer;
import mind.parser;
import mind.keywords;
import mind.errors;

class IncludeStatement {
    Token token;
    string path;

    this(Token token, string path) {
        this.token = token;
        this.path = path;
    }
}

IncludeStatement parseIncludeStatement(ref Parser parser) {
    auto token = parser.expect(TokenType.Identifier);

    if (token.lexeme != Keywords.Include) {
        throw new CompilerException("Expected 'include' keyword.", token);
    }

    auto stringToken = parser.expect(TokenType.StringLiteral);

    if (stringToken.lexeme.length < 2 || stringToken.lexeme[0] != '"' || stringToken.lexeme[$ - 1] != '"') {
        throw new CompilerException("Include path must be a normal string literal.", stringToken);
    }
    
    string path = stringToken.lexeme[1 .. $ - 1];

    return new IncludeStatement(token, path);
}
