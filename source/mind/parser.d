module mind.parser;

import std.conv : to;

import mind.tokenizer;
import mind.errors;

struct Parser {
    Token[] tokens;
    size_t pos = 0;

    this(Token[] tokens) {
        this.tokens = tokens;
    }

    bool isEOF() {
        return pos >= tokens.length;
    }

    Token peekLast() {
        auto lastPos = pos - 1;
        if (lastPos < 0 || lastPos >= tokens.length) return Token(TokenType.Unknown, "", "", 0, 0);
        return tokens[lastPos];
    }

    Token peek(size_t amount = 0) {
        size_t ahead = pos + amount;
        if (ahead >= tokens.length) return Token(TokenType.Unknown, "", "", 0, 0);
        return tokens[ahead];
    }

    Token next() {
        if (pos >= tokens.length) return Token(TokenType.Unknown, "", "", 0, 0);
        return tokens[pos++];
    }

    bool match(TokenType expected) {
        if (peek().type == expected) {
            next();
            return true;
        }
        return false;
    }

    Token expect(TokenType expected) {
        if (isEOF) {
            auto last = peekLast();

            throw new CompilerException(
                "Expected token " ~ expected.to!string ~
                ", but reached end of input after.",
                last
            );
        }

        auto t = peek();
        enforceCompilerException(t.type == expected, "Expected token " ~ expected.to!string ~
            ", found " ~ t.type.to!string ~
            " ('" ~ t.lexeme ~ "')", t);

        return next();
    }

    bool check(TokenType expected) {
        if (isEOF()) return false;
        return peek().type == expected;
    }

    size_t save() {
        return pos;
    }

    void restore(size_t saved) {
        pos = saved;
    }
}