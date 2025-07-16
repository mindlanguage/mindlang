module mind.attributes;

import mind.tokenizer;
import mind.ast;
import mind.expressions;
import mind.parser;
import mind.keywords;
import mind.errors;

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

        // Expect the opening bracket for @[...]
        p.expect(TokenType.LBracket);

        // Parse one or more attributes inside the brackets
        while (true) {
            auto nameToken = p.expect(TokenType.Identifier);
            if (isKeyword(nameToken.lexeme)) {
                throw new CompilerException("Cannot use keyword as identifier.", nameToken);
            }

            Expr[] args;

            // Optional parentheses for arguments: @[name(...)]
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

            // Break if there are no more attributes in this list
            if (p.match(TokenType.Comma)) {
                continue;
            } else {
                break;
            }
        }

        // Expect closing bracket
        p.expect(TokenType.RBracket);
    }

    return attrs;
}