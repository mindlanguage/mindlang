module mind.tokenizer;

import std.stdio;
import std.string;
import std.algorithm;
import std.array;
import std.conv;

enum TokenType {
    // Special/general
    Unknown,
    Whitespace,          // no longer emitted outside strings/comments
    Newline,             // no longer emitted outside strings/comments
    Identifier,
    NumberLiteral,
    StringLiteral,
    CharLiteral,
    MultilineStringLiteral,

    InterpolatedStringText,   // Plain text parts inside interpolated strings
    InterpolatedStringStart,  // Token for start of interpolated string ($" or $@")
    InterpolatedStringEnd,    // Token for closing quote

    LineComment,
    BlockComment,

    // Symbols (ordered by descending length)
    Semicolon,         // ;
    Comma,             // ,
    TildeArrow,        // ~>
    DashArrow,         // ->
    EqualsArrow,       // =>
    TripleShiftAssign, // >>>=
    TripleShift,       // >>>
    DoubleShiftAssign, // >>=
    LeftShiftAssign,   // <<=
    
    LeftShift,         // <<
    RightShift,        // >>
    
    EqualEqual,        // ==
    NotEqual,          // !=
    GreaterEqual,      // >=
    LessEqual,         // <=
    TildeEqual,        // ~=
    XorEqual,          // ^=
    MulEqual,          // *=
    PlusEqual,         // +=
    MinusEqual,        // -=
    DivEqual,          // /=
    QuestionEqual,     // ?=
    NullCoalesce,      // ??
    Question,          // ?
    Colon,             // :
    Xor,               // ^
    Asterisk,          // *
    Plus,              // +
    PlusPlus,          // ++
    Minus,             // -
    MinusMinus,        // --
    LBrace,            // {
    RBrace,            // }
    LBracket,          // [
    RBracket,          // ]
    LParen,            // (
    RParen,            // )
    AndAnd,            // &&
    AndEqual,          // &=
    And,               // &
    Slash,             // /
    LTGT,              // <>
    LessThan,          // <
    GreaterThan,       // >
    OrOr,              // ||
    OrEqual,           // |=
    Or,                // |
    At,                // @
    Dollar,            // $
    ModEqual,          // %=
    Mod,               // %
    PowEqual,          // ^^=
    Pow,               // ^^
    DotDotDot,         // ...
    DotDot,            // ..
    Dot,               // .
    Exclamation,       // !
    Equal,              // =
    Tilde              // ~
}

struct Token {
    TokenType type;
    string lexeme;
    string sourceFile;
    size_t line;
    size_t column;

    static Token dummy(TokenType type) {
        return Token(type, "", "", 0, 0);
    }
}

public enum UnknownToken = Token(TokenType.Unknown, "", "", 0, 0);

private TokenType[string] tokenMap;

private void initializeTokenMap() {
    tokenMap = [
        ";"  : TokenType.Semicolon,
        ","  : TokenType.Comma,
        "~>" : TokenType.TildeArrow,
        "->" : TokenType.DashArrow,
        "=>" : TokenType.EqualsArrow,
        ">>>=": TokenType.TripleShiftAssign,
        ">>>": TokenType.TripleShift,
        ">>=": TokenType.DoubleShiftAssign,
        "<<=": TokenType.LeftShiftAssign,
        "<<" : TokenType.LeftShift,
        ">>" : TokenType.RightShift,
        "==": TokenType.EqualEqual,
        "!=": TokenType.NotEqual,
        ">=": TokenType.GreaterEqual,
        "<=": TokenType.LessEqual,
        "~=": TokenType.TildeEqual,
        "^=": TokenType.XorEqual,
        "*=": TokenType.MulEqual,
        "+=": TokenType.PlusEqual,
        "-=": TokenType.MinusEqual,
        "/=": TokenType.DivEqual,
        "?=": TokenType.QuestionEqual,
        "??": TokenType.NullCoalesce,
        "?": TokenType.Question,
        ":": TokenType.Colon,
        "^": TokenType.Xor,
        "*": TokenType.Asterisk,
        "+": TokenType.Plus,
        "++": TokenType.PlusPlus,
        "-": TokenType.Minus,
        "--": TokenType.MinusMinus,
        "{": TokenType.LBrace,
        "}": TokenType.RBrace,
        "[": TokenType.LBracket,
        "]": TokenType.RBracket,
        "(": TokenType.LParen,
        ")": TokenType.RParen,
        "&&": TokenType.AndAnd,
        "&=": TokenType.AndEqual,
        "&": TokenType.And,
        "/": TokenType.Slash,
        "<>": TokenType.LTGT,
        "<": TokenType.LessThan,
        ">": TokenType.GreaterThan,
        "||": TokenType.OrOr,
        "|=": TokenType.OrEqual,
        "|": TokenType.Or,
        "@": TokenType.At,
        "$": TokenType.Dollar,
        "%=": TokenType.ModEqual,
        "%": TokenType.Mod,
        "^^=": TokenType.PowEqual,
        "^^": TokenType.Pow,
        "...": TokenType.DotDotDot,
        "..": TokenType.DotDot,
        ".": TokenType.Dot,
        "!": TokenType.Exclamation,
        "=": TokenType.Equal,
        "~": TokenType.Tilde
    ];
}

/// Recursively tokenizes an interpolated string content
/// starting after the opening quote (pos points right after $" or $@" sequence)
/// Returns tokens and updates i, line, col accordingly
Token[] tokenizeInterpolatedString(string source, ref size_t i, ref size_t line, ref size_t col, bool isVerbatim, bool removeComments, string sourceFile) {
    Token[] tokens;
    size_t textStart = i;
    size_t textColStart = col;

    while (i < source.length) {
        char c = source[i];

        if (!isVerbatim && c == '\\') {
            // Escape sequence in normal string — consume two chars
            i += 2;
            col += 2;
            continue;
        }

        if (c == '{') {
            // Emit pending text before '{'
            if (textStart < i) {
                tokens ~= Token(TokenType.InterpolatedStringText, source[textStart .. i], sourceFile, line, textColStart);
            }

            // Emit '{' token
            tokens ~= Token(TokenType.LBrace, "{", sourceFile, line, col);
            i++; col++;

            // Tokenize expression inside braces until matching '}'
            int innerBraceDepth = 1;
            size_t exprStart = i;
            size_t exprStartLine = line;
            size_t exprStartCol = col;

            while (i < source.length && innerBraceDepth > 0) {
                c = source[i];
                if (c == '\n') {
                    i++; line++; col = 1;
                    continue;
                }
                if (c == '{') {
                    tokens ~= Token(TokenType.LBrace, "{", sourceFile, line, col);
                    i++; col++;
                    innerBraceDepth++;
                }
                else if (c == '}') {
                    innerBraceDepth--;
                    if (innerBraceDepth == 0) {
                        // Recursively tokenize inner expression
                        auto exprStr = source[exprStart .. i];
                        size_t exprI = 0;
                        size_t exprLine = exprStartLine;
                        size_t exprCol = exprStartCol;
                        tokens ~= tokenize(exprStr, exprI, exprLine, exprCol, removeComments, sourceFile);

                        tokens ~= Token(TokenType.RBrace, "}", sourceFile, line, col);
                        i++; col++;
                        break;
                    }
                    else {
                        tokens ~= Token(TokenType.RBrace, "}", sourceFile, line, col);
                        i++; col++;
                    }
                }
                else if (c == '"' && i > 0 && source[i-1] == '$') {
                    tokens ~= Token(TokenType.InterpolatedStringStart, "$\"", sourceFile, line, col);
                    i++; col++;
                    tokens ~= tokenizeInterpolatedString(source, i, line, col, false, removeComments, sourceFile);
                    tokens ~= Token(TokenType.InterpolatedStringEnd, "\"", sourceFile, line, col);
                }
                else {
                    // *** JUST ADVANCE without emitting Unknown token ***
                    i++; col++;
                }
            }
            textStart = i;
            textColStart = col;
            continue;
        }
        else if (c == '"') {
            if (textStart < i) {
                tokens ~= Token(TokenType.InterpolatedStringText, source[textStart .. i], sourceFile, line, textColStart);
            }
            tokens ~= Token(TokenType.InterpolatedStringEnd, "\"", sourceFile, line, col);
            i++; col++;
            break;
        }
        else if (c == '\n') {
            if (isVerbatim) {
                i++; line++; col = 1;
                continue;
            }
            else {
                break;
            }
        }
        else {
            i++; col++;
        }
    }
    return tokens;
}

Token[] tokenize(string source, ref size_t i, ref size_t line, ref size_t col, bool removeComments, string sourceFile) {
    initializeTokenMap();

    Token[] tokens;
    auto keys = tokenMap.keys.sort!((a, b) => b.length < a.length);

    while (i < source.length) {
        char c = source[i];

        // Skip whitespace outside strings/comments (no tokens emitted)
        if (c == ' ' || c == '\t' || c == '\r') {
            ++i;
            ++col;
            continue;
        }

        if (c == '\n') {
            i++;
            line++;
            col = 1;
            continue;
        }

        // Line comment
        if (i + 2 <= source.length && source[i .. i + 2] == "//") {
            size_t start = i;
            size_t startCol = col;
            i += 2;
            col += 2;
            while (i < source.length && source[i] != '\n') {
                i++;
                col++;
            }
            if (!removeComments)
                tokens ~= Token(TokenType.LineComment, source[start .. i], sourceFile, line, startCol);
            continue;
        }

        // Block comment
        if (i + 2 <= source.length && source[i .. i + 2] == "/*") {
            size_t start = i;
            size_t startCol = col;
            i += 2;
            col += 2;
            while (i + 1 < source.length && source[i .. i + 2] != "*/") {
                if (source[i] == '\n') {
                    i++;
                    line++;
                    col = 1;
                } else {
                    i++;
                    col++;
                }
            }
            if (i + 2 <= source.length) {
                i += 2;
                col += 2;
            }
            if (!removeComments)
                tokens ~= Token(TokenType.BlockComment, source[start .. i], sourceFile, line, startCol);
            continue;
        }

        // Interpolated strings start
        if (i + 3 <= source.length && source[i] == '$' && source[i+1] == '@' && source[i+2] == '"') {
            tokens ~= Token(TokenType.InterpolatedStringStart, "$@\"", sourceFile, line, col);
            i += 3; col += 3;
            tokens ~= tokenizeInterpolatedString(source, i, line, col, true, removeComments, sourceFile);
            continue;
        }

        if (i + 2 <= source.length && source[i] == '$' && source[i+1] == '"') {
            tokens ~= Token(TokenType.InterpolatedStringStart, "$\"", sourceFile, line, col);
            i += 2; col += 2;
            tokens ~= tokenizeInterpolatedString(source, i, line, col, false, removeComments, sourceFile);
            continue;
        }

        // Regular string literal (handles escaped characters correctly)
        if (c == '"') {
            size_t start = i;
            size_t startCol = col;
            i++; col++; // skip initial "

            while (i < source.length) {
                if (source[i] == '\\') {
                    if (i + 1 < source.length) {
                        i += 2;
                        col += 2;
                    } else {
                        // lone backslash at end
                        i++; col++;
                        break;
                    }
                } else if (source[i] == '"') {
                    i++; col++;
                    break;
                } else if (source[i] == '\n') {
                    // Unterminated string
                    break;
                } else {
                    i++; col++;
                }
            }

            tokens ~= Token(TokenType.StringLiteral, source[start .. i], sourceFile, line, startCol);
            continue;
        }

        // Char literal
        if (c == '\'') {
            size_t start = i;
            size_t startCol = col;
            i++; col++;
            while (i < source.length) {
                if (source[i] == '\\') {
                    i += 2;
                    col += 2;
                    if (i > source.length) break;
                } else if (source[i] == '\'') {
                    i++;
                    col++;
                    break;
                } else if (source[i] == '\n') {
                    break;
                } else {
                    i++;
                    col++;
                }
            }
            tokens ~= Token(TokenType.CharLiteral, source[start .. i], sourceFile, line, startCol);
            continue;
        }

        // Verbatim multiline string
        if (i + 2 <= source.length && source[i .. i + 2] == "@\"") {
            size_t start = i;
            size_t startCol = col;
            i += 2;
            col += 2;
            while (i < source.length) {
                if (source[i] == '"') {
                    i++;
                    col++;
                    break;
                }
                if (source[i] == '\n') {
                    i++;
                    line++;
                    col = 1;
                } else {
                    i++;
                    col++;
                }
            }
            tokens ~= Token(TokenType.MultilineStringLiteral, source[start .. i], sourceFile, line, startCol);
            continue;
        }

        // Try to match tokens from tokenMap (longest first)
        bool matched = false;

        foreach (key; keys) {
            if (i + key.length <= source.length && source[i .. i + key.length] == key) {
                tokens ~= Token(tokenMap[key], key, sourceFile, line, col);
                i += key.length;
                col += key.length;
                matched = true;
                break;
            }
        }
        if (matched) continue;

        // Identifiers
        if (isAlpha(c) || c == '_') {
            size_t start = i;
            size_t startCol = col;
            while (i < source.length && (isAlphaNum(source[i]) || source[i] == '_')) {
                i++;
                col++;
            }
            tokens ~= Token(TokenType.Identifier, source[start .. i], sourceFile, line, startCol);
            continue;
        }

        // Number literals (supports decimals and suffixes like 'f')
        if (isDigit(c)) {
            size_t start = i;
            size_t startCol = col;

            // Integer part
            while (i < source.length && isDigit(source[i])) {
                i++;
                col++;
            }

            // Optional decimal part
            if (i < source.length && source[i] == '.' && i + 1 < source.length && isDigit(source[i + 1])) {
                i++; col++; // consume '.'
                while (i < source.length && isDigit(source[i])) {
                    i++;
                    col++;
                }
            }

            // Optional suffix (e.g. 'f', 'd', etc.)
            if (i < source.length && isAlpha(source[i])) {
                i++;
                col++;
            }

            tokens ~= Token(TokenType.NumberLiteral, source[start .. i], sourceFile, line, startCol);
            continue;
        }

        // Unknown char
        tokens ~= Token(TokenType.Unknown, source[i .. i + 1], sourceFile, line, col);
        i++;
        col++;
    }

    return tokens;
}

// Convenience wrapper
Token[] tokenize(string source, bool removeComments, string sourceFile) {
    size_t i = 0;
    size_t line = 1;
    size_t col = 1;
    return tokenize(source, i, line, col, removeComments, sourceFile);
}

// Character helpers
bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

bool isAlphaNum(char c) {
    return isAlpha(c) || isDigit(c);
}