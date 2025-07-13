module mind.types;

import mind.parser;
import mind.tokenizer;
import mind.identifiers;
import mind.expressions;
import mind.ast;

class TypeReference {
    string baseName;
    string[] qualifiers;
    TypeReference[] typeArguments;

    TypeReference arrayElementType;  // for array types like int[] or int[5]
    Expr arraySizeExpr;              // for fixed size arrays int[5] (null if dynamic)
    TypeReference keyType;           // for map types like ValueType[KeyType]

    override string toString() {
        import std.array : join;
        import std.algorithm : map;

        string s = (qualifiers.length > 0 ? qualifiers.join(".") ~ "." : "") ~ baseName;

        if (typeArguments.length) {
            s ~= "<" ~ typeArguments.map!(a => a.toString).join(",") ~ ">";
        }

        if (arrayElementType !is null) {
            s = arrayElementType.toString;
            s ~= "[";
            if (arraySizeExpr !is null)
                s ~= arraySizeExpr.toString();
            s ~= "]";
        } else if (keyType !is null) {
            s = this.toString(); // base type string
            s ~= "[" ~ keyType.toString ~ "]";
        }

        return s;
    }
}

TypeReference parseTypeReference(ref Parser parser) {
    // Parse qualified identifier parts: e.g. foo.bar.Baz
    auto parts = parseQualifiedIdentifierParts(parser);

    auto typeRef = new TypeReference;
    typeRef.baseName = parts[$ - 1];
    typeRef.qualifiers = parts[0 .. $ - 1];

    // Parse template arguments like Foo!(Bar, Baz) or Foo!Bar
    if (parser.peek().type == TokenType.Exclamation) {
        parser.next(); // consume '!'

        if (parser.peek().type == TokenType.LParen) {
            parser.next(); // consume '('
            while (true) {
                typeRef.typeArguments ~= parseTypeReference(parser);
                if (parser.peek().type == TokenType.Comma) {
                    parser.next();
                    continue;
                } else {
                    break;
                }
            }
            parser.expect(TokenType.RParen);
        } else {
            // Single template argument
            typeRef.typeArguments ~= parseTypeReference(parser);
        }
    }

    // Handle array and map types with brackets, including empty brackets for dynamic arrays
    while (parser.peek().type == TokenType.LBracket) {
        parser.next(); // consume '['

        Expr exprOrType = null;

        // If next token is not closing bracket, parse expression or type inside brackets
        if (parser.peek().type != TokenType.RBracket) {
            exprOrType = parseExpression(parser);
        }
        parser.expect(TokenType.RBracket);

        bool isTypeRef = false;
        if (exprOrType !is null) {
            if (cast(IdentifierExpr) exprOrType !is null ||
                cast(TemplatedExpr) exprOrType !is null ||
                cast(CallExpr) exprOrType !is null) {
                isTypeRef = true;
            }
        }

        if (isTypeRef) {
            // Map type: TYPE[KEYTYPE]
            auto mapType = new TypeReference;
            mapType.baseName = typeRef.baseName;
            mapType.qualifiers = typeRef.qualifiers.dup;
            mapType.typeArguments = typeRef.typeArguments.dup;
            mapType.keyType = exprToTypeReference(exprOrType);
            typeRef = mapType;
        } else {
            // Array type: dynamic (int[]) or fixed size (int[5])
            auto arrType = new TypeReference;
            arrType.arrayElementType = typeRef;
            arrType.arraySizeExpr = exprOrType; // null for dynamic array
            typeRef = arrType;
        }
    }

    return typeRef;
}

// Helper function to convert an Expr to a TypeReference
TypeReference exprToTypeReference(Expr expr) {
    // Handle simple identifier expressions
    auto id = cast(IdentifierExpr) expr;
    if (id !is null) {
        auto tr = new TypeReference;
        tr.baseName = id.name;
        tr.qualifiers = [];
        tr.typeArguments = [];
        return tr;
    }

    // Handle templated expressions like Foo!Bar or Foo!(Bar, Baz)
    auto templ = cast(TemplatedExpr) expr;
    if (templ !is null) {
        auto baseTR = exprToTypeReference(templ.target);
        foreach (arg; templ.templateArgs) {
            baseTR.typeArguments ~= exprToTypeReference(arg);
        }
        return baseTR;
    }

    // Handle call expressions (e.g., templated calls)
    auto call = cast(CallExpr) expr;
    if (call !is null) {
        auto baseTR = exprToTypeReference(call.callee);
        foreach (arg; call.arguments) {
            baseTR.typeArguments ~= exprToTypeReference(arg);
        }
        return baseTR;
    }

    // Fallback for unsupported exprs
    throw new Exception("Cannot convert expression to type reference: " ~ expr.toString());
}
