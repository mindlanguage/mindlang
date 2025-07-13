module mind.attributes;

import mind.tokenizer;
import mind.ast;
import mind.expressions;
import mind.parser;

class Attribute {
    Token atToken;       // '@' token
    string name;         // attribute name
    Expr[] args;         // optional arguments, empty if none

    this(Token atToken, string name, Expr[] args) {
        this.atToken = atToken;
        this.name = name;
        this.args = args;
    }
}

Attribute[] parseAttributes(ref Parser p) {
    Attribute[] attrs;

    while (p.peek().type == TokenType.At) {
        auto atToken = p.next(); // consume '@'
        auto nameToken = p.expect(TokenType.Identifier);
        Expr[] args;

        if (p.match(TokenType.LParen)) {
            if (!p.match(TokenType.RParen)) {
                while (true) {
                    args ~= parseExpression(p);
                    if (p.match(TokenType.RParen))
                        break;
                    p.expect(TokenType.Comma);
                }
            }
        }

        attrs ~= new Attribute(atToken, nameToken.lexeme, args);
    }

    return attrs;
}