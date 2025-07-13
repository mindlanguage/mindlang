module mind.variables;

import mind.parser;
import mind.tokenizer;
import mind.access;
import mind.expressions;
import mind.errors;
import mind.keywords;
import mind.ast;
import mind.types;
import mind.attributes;

enum VarKind {
    Let,    // let (immutable)
    Mut,    // mut (mutable)
    Const,  // const (immutable with const semantics)
    Enum    // enum (special constant or enum value)
}

class VariableDecl {
    AccessModifier access;
    Attribute[] attributes;
    Token token;
    VarKind kind;       // Kind of variable (let, mut, const, enum)
    string name;
    TypeReference type;
    Expr initializer;   // the parsed expression after =

    this(AccessModifier access, Attribute[] attributes, Token token, VarKind kind, string name, TypeReference type, Expr initializer) {
        this.access = access;
        this.attributes = attributes;
        this.token = token;
        this.kind = kind;
        this.name = name;
        this.type = type;
        this.initializer = initializer;
    }
}

VariableDecl parseVariableDeclaration(Attribute[] attributes, bool isParameter, VarKind defaultKind, AccessModifier access, ref Parser parser) {
    VarKind kind = defaultKind;
    Token variableToken;

    auto nextToken = parser.peek();

    bool kindParsed = false;
    if (nextToken.type == TokenType.Identifier) {
        switch (nextToken.lexeme) {
            case Keywords.Let, Keywords.Mut, Keywords.Const, Keywords.Enum:
                kindParsed = true;
                break;
            default:
                kindParsed = false;
        }
    }

    if (kindParsed) {
        auto token = parser.next();
        variableToken = token;

        switch (token.lexeme) {
            case Keywords.Let:   kind = VarKind.Let; break;
            case Keywords.Mut:   kind = VarKind.Mut; break;
            case Keywords.Const: kind = VarKind.Const; break;
            case Keywords.Enum:  kind = VarKind.Enum; break;
            default:
                throw new CompilerException("Unexpected variable kind.", token);
        }
    } else if (!isParameter) {
        throw new CompilerException("Expected 'let', 'mut', 'const', or 'enum'.", nextToken);
    }

    auto identToken = parser.expect(TokenType.Identifier);
    if (!kindParsed) {
        variableToken = identToken;
    }
    string varName = identToken.lexeme;

    TypeReference typeAnnotation = null;
    if (parser.match(TokenType.Colon)) {
        typeAnnotation = parseTypeReference(parser);
    }

    Expr initializer = null;
    if (parser.match(TokenType.Equal)) {
        initializer = parseExpression(parser);
    }

    return new VariableDecl(access, attributes, variableToken, kind, varName, typeAnnotation, initializer);
}