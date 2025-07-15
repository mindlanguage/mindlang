module mind.access;

import mind.keywords;
import mind.parser;
import mind.tokenizer;
import mind.errors;

enum AccessLevel {
    Public,
    Private,
    Protected,
    Package,
    Default
}

struct AccessModifier {
    AccessLevel level = AccessLevel.Default;
    bool isStatic = false;
    bool isFinal = false;
    bool isRef = false;
    bool isOut = false;
}

public enum DefaultAccessModifier = AccessModifier(AccessLevel.Default, false, false, false, false);

AccessModifier parseAccessModifier(bool allowOut, ref Parser parser) {
    AccessModifier modifier;
    bool seenModifier = true;

    while (!parser.isEOF && parser.peek().type == TokenType.Identifier && seenModifier) {
        auto token = parser.peek();
        seenModifier = false;

        switch (token.lexeme) {
            case Keywords.Public:
                if (modifier.level != AccessLevel.Default)
                    throw new CompilerException("Multiple access modifiers specified.", token);
                modifier.level = AccessLevel.Public;
                parser.next();
                seenModifier = true;
                break;

            case Keywords.Private:
                if (modifier.level != AccessLevel.Default)
                    throw new CompilerException("Multiple access modifiers specified.", token);
                modifier.level = AccessLevel.Private;
                parser.next();
                seenModifier = true;
                break;

            case Keywords.Protected:
                if (modifier.level != AccessLevel.Default)
                    throw new CompilerException("Multiple access modifiers specified.", token);
                modifier.level = AccessLevel.Protected;
                parser.next();
                seenModifier = true;
                break;

            case Keywords.Package:
                if (modifier.level != AccessLevel.Default)
                    throw new CompilerException("Multiple access modifiers specified.", token);
                modifier.level = AccessLevel.Package;
                parser.next();
                seenModifier = true;
                break;

            case Keywords.Static:
                if (modifier.isStatic)
                    throw new CompilerException("Duplicate 'static' modifier.", token);
                modifier.isStatic = true;
                parser.next();
                seenModifier = true;
                break;

            case Keywords.Final:
                if (modifier.isFinal)
                    throw new CompilerException("Duplicate 'final' modifier.", token);
                modifier.isFinal = true;
                parser.next();
                seenModifier = true;
                break;

            case Keywords.Ref:
                if (modifier.isRef)
                    throw new CompilerException("Duplicate 'ref' modifier.", token);
                if (modifier.isOut)
                    throw new CompilerException("'ref' and 'out' cannot be combined.", token);
                modifier.isRef = true;
                parser.next();
                seenModifier = true;
                break;

            case Keywords.Out:
                if (!allowOut)
                    throw new CompilerException("'out' not allowed in current scope.", token);
                if (modifier.isOut)
                    throw new CompilerException("Duplicate 'out' modifier.", token);
                if (modifier.isRef)
                    throw new CompilerException("'out' and 'ref' cannot be combined.", token);
                modifier.isOut = true;
                parser.next();
                seenModifier = true;
                break;

            default:
                break; // Not a modifier — exit loop
        }
    }

    return modifier;
}