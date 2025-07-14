module mind.errors;

import std.conv : to;

import mind.tokenizer;
import mind.symbols;

public class CompilerException : Exception {
    this(string message, Token token) {
        super(message ~ " in " ~ token.sourceFile ~" at line " ~ token.line.to!string ~ " column " ~ token.column.to!string ~ " '" ~ token.lexeme ~"'");
    }

    this(string message, Symbol symbol) {
        super(message ~ " in " ~ symbol.token.sourceFile ~" at line " ~ symbol.token.line.to!string ~ " column " ~ symbol.token.column.to!string ~ " '" ~ symbol.token.lexeme ~"' '" ~ symbol.name ~ "'");
    }
}

void enforceCompilerException(bool expression, string message, Token token) {
    if (!expression) {
        throw new CompilerException(message, token);
    }
}

void enforceCompilerException(bool expression, string message, Symbol symbol) {
    if (!expression) {
        throw new CompilerException(message, symbol);
    }
}
