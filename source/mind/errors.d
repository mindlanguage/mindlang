module mind.errors;

import std.conv : to;

import mind.tokenizer;

public class CompilerException : Exception {
    this(string message, Token token) {
        super(message ~ " in " ~ token.sourceFile ~" at line " ~ token.line.to!string ~ " column " ~ token.column.to!string ~ " '" ~ token.lexeme ~"'");
    }
}

void enforceCompilerException(bool expression, string message, Token token) {
    if (!expression) {
        throw new CompilerException(message, token);
    }
}

