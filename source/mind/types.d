module mind.types;

import mind.parser;
import mind.tokenizer;
import mind.identifiers;
import mind.expressions;
import mind.ast;
import mind.errors;

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
                } else {
                    break;
                }
            }
            parser.expect(TokenType.RParen);
        } else {
            typeRef.typeArguments ~= parseTypeReference(parser);
        }
    }

    // Parse pointer suffix: '*'
    typeRef = applyPointerSugar(typeRef, parser);

    // Handle array or map types
    while (parser.peek().type == TokenType.LBracket) {
        parser.next(); // consume '['
        Expr exprOrType = null;
        auto restorePoint = parser.save();

        // Try parsing the content inside brackets
        if (parser.peek().type != TokenType.RBracket) {
            // Try to parse as type first
            bool parsedAsType = false;
            size_t checkpoint = parser.save();
            try {
                auto trialType = parseTypeReference(parser);
                parser.expect(TokenType.RBracket);
                auto mapType = new TypeReference;
                mapType.baseName = typeRef.baseName;
                mapType.qualifiers = typeRef.qualifiers.dup;
                mapType.typeArguments = typeRef.typeArguments.dup;
                mapType.keyType = trialType;
                typeRef = mapType;
                parsedAsType = true;
            } catch (Exception e) {
                parser.restore(checkpoint);
            }

            if (!parsedAsType) {
                exprOrType = parseExpression(parser);
                parser.expect(TokenType.RBracket);
                auto arrType = new TypeReference;
                arrType.arrayElementType = typeRef;
                arrType.arraySizeExpr = exprOrType;
                typeRef = arrType;
            }
        } else {
            parser.expect(TokenType.RBracket); // closing ']'
            auto dynArrayType = new TypeReference;
            dynArrayType.arrayElementType = typeRef;
            typeRef = dynArrayType;
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

TypeReference applyPointerSugar(TypeReference typeRef, ref Parser parser) {
    while (parser.peek().type == TokenType.Asterisk) {
        parser.next();
        auto ptrType = new TypeReference;
        ptrType.baseName = "ptr";
        ptrType.typeArguments = [typeRef];
        typeRef = ptrType;
    }
    return typeRef;
}