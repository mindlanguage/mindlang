module mind.unittests;

import mind.tokenizer;
import mind.statements;
import mind.functions;
import mind.parser;
import mind.errors;
import mind.keywords;

class UnittestBlock : Statement {
    Token unittestToken;
    string name;       // empty string if no identifier given
    Statement[] body;

    this(Token unittestToken, string name, Statement[] body) {
        this.unittestToken = unittestToken;
        this.name = name;
        this.body = body;
    }
}

UnittestBlock parseUnittestBlock(ref Parser p) {
    auto unittestToken = p.expect(TokenType.Identifier);
    if (unittestToken.lexeme != Keywords.Unittest)
        throw new CompilerException("Expected 'unittest' keyword.", unittestToken);

    string name = "";

    // Optionally parse an identifier after 'unittest'
    if (p.peek().type == TokenType.Identifier) {
        auto nameToken = p.next();
        name = nameToken.lexeme;

        if (isKeyword(nameToken.lexeme)) {
            throw new CompilerException("Cannot use keyword as identifier.", nameToken);
        }
    }

    // Expect block start '{'
    p.expect(TokenType.LBrace);

    // Parse statements inside block
    auto body = parseStatements(TokenType.RBrace, p);

    p.expect(TokenType.RBrace);

    return new UnittestBlock(unittestToken, name, body);
}