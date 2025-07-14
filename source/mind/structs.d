module mind.structs;

import mind.access;
import mind.variables;
import mind.tokenizer;
import mind.parser;
import mind.types;
import mind.errors;
import mind.identifiers;
import mind.keywords;
import mind.functions;
import mind.properties;
import mind.attributes;
import mind.unittests;

class StructMember {
    Attribute[] attributes;
    AccessModifier access;
    VariableDecl variable;
    StructDecl unionDecl;
    FunctionDecl fnDecl;
    bool isDestructor;
    PropStatement propStatement;
    UnittestBlock unittestBlock;

    this(Attribute[] attributes, AccessModifier access, VariableDecl variable) {
        this.attributes = attributes;
        this.access = access;
        this.variable = variable;
    }

    this(Attribute[] attributes, AccessModifier access, StructDecl unionDecl) {
        this.attributes = attributes;
        this.access = access;
        this.unionDecl = unionDecl;
    }

    this(Attribute[] attributes, AccessModifier access, FunctionDecl fnDecl, bool isDestructor) {
        this.attributes = attributes;
        this.access = access;
        this.fnDecl = fnDecl;
        this.isDestructor = isDestructor;
    }

    this(Attribute[] attributes, AccessModifier access, PropStatement propStatement) {
        this.attributes = attributes;
        this.access = access;
        this.propStatement = propStatement;
    }

    this(Attribute[] attributes, AccessModifier access, UnittestBlock unittestBlock) {
        this.attributes = attributes;
        this.access = access;
        this.unittestBlock = unittestBlock;
    }
}

class StructDecl {
    AccessModifier access;
    Token token;
    string name;
    string[] genericParams;
    TypeReference[] baseTypes;
    StructMember[] members;
    bool isUnion;
    bool isExtern;
    bool isInternal;

    this(AccessModifier access, Token token, string name, string[] genericParams, TypeReference[] baseTypes, StructMember[] members, bool isUnion) {
        this.access = access;
        this.token = token;
        this.name = name;
        this.genericParams = genericParams;
        this.baseTypes = baseTypes;
        this.members = members;
    }
}

StructDecl parseStructDeclaration(AccessModifier access, bool excludeName, ref Parser parser) {
    // Parse 'struct' keyword
    auto structToken = parser.expect(TokenType.Identifier);
    bool isUnion = structToken.lexeme == Keywords.Union;
    enforceCompilerException(
        structToken.lexeme == Keywords.Struct || isUnion,
        "Expected 'struct' or 'union'", structToken);

    bool isExtern = false;
    bool isInternal = false;

    if (parser.match(TokenType.LParen)) {
        auto modeToken = parser.expect(TokenType.Identifier);
        if (modeToken.lexeme == Keywords.Extern) {
            isExtern = true;
        } else if (modeToken.lexeme == Keywords.Internal) {
            isInternal = true;
        } else {
            throw new CompilerException("Expected 'extern' or 'internal' in struct { ... }", modeToken);
        }

        parser.expect(TokenType.RParen);
    }

    // Parse struct name
    string structName = "";
    if (!excludeName) {
        auto nameParts = parseQualifiedIdentifierParts(parser);
        structName = nameParts[$ - 1];
    }

    // Optional generic parameters: (T, U, ...)
    string[] genericParams;
    if (parser.match(TokenType.LParen)) {
        while (!parser.match(TokenType.RParen)) {
            auto paramToken = parser.expect(TokenType.Identifier);
            genericParams ~= paramToken.lexeme;
            if (!parser.match(TokenType.Comma))
                break;
        }
        parser.expect(TokenType.RParen);
    }

    // Optional base type (inheritance)
    TypeReference[] baseTypes;
    if (parser.match(TokenType.Colon)) {
        while (true) {
            auto baseType = parseTypeReference(parser);
            baseTypes ~= baseType;

            if (parser.peek().type == TokenType.Comma) {
                parser.next(); // consume ','
                continue;
            }
            break;
        }
    }

    // Opening brace
    parser.expect(TokenType.LBrace);

    // Struct members
    StructMember[] members;

    while (!parser.check(TokenType.RBrace) && !parser.isEOF()) {
        bool parseMember = true;
        auto memberAttributes = parseAttributes(parser);
        auto memberAccess = parseAccessModifier(false, parser);

        if (parser.peek().type == TokenType.Identifier) {
            auto token = parser.peek();

            switch (token.lexeme) {
                case Keywords.Union:
                    auto u = parseStructDeclaration(memberAccess, true, parser);

                    members ~= new StructMember(memberAttributes, memberAccess, u);

                    parseMember = false;
                    break;

                case Keywords.Fn:
                    auto fn = parseFunction(memberAttributes, access, true, parser);

                    members ~= new StructMember(memberAttributes, memberAccess, fn, false);

                    parseMember = false;
                    break;

                case Keywords.Prop:
                    auto prop = parsePropertyStatement(memberAttributes, memberAccess, parser);

                    members ~= new StructMember(memberAttributes, memberAccess, prop);

                    parseMember = false;
                    break;

                case Keywords.Unittest:
                    auto unit = parseUnittestBlock(parser);

                    members ~= new StructMember(memberAttributes, memberAccess, unit);

                    parseMember = false;
                    break;

                default: break;
            }
        }
        
        if (parseMember) {
            if (parser.peek().type == TokenType.Identifier &&
                parser.peek().lexeme == Keywords.This) {
                    // Constructors: this();
                    auto fn = parseFunction(memberAttributes, access, false, parser);

                    members ~= new StructMember(memberAttributes, memberAccess, fn, false);

                    parseMember = false;
            } else if (parser.peek().type == TokenType.Exclamation &&
                parser.peek(1).type == TokenType.Identifier &&
                parser.peek(1).lexeme == Keywords.This) {
                    // Destructors: !this();
                    parser.next(); // Consume !
                    auto fn = parseFunction(memberAttributes, access, false, parser);

                    members ~= new StructMember(memberAttributes, memberAccess, fn, true);

                    parseMember = false;
            } else {
                auto decl = parseVariableDeclaration(memberAttributes, true, VarKind.Let, memberAccess, parser);
                members ~= new StructMember(memberAttributes, memberAccess, decl);
                parser.expect(TokenType.Semicolon); // Struct members require semicolon
            }
        }
    }

    parser.expect(TokenType.RBrace);

    auto s = new StructDecl(access, structToken, structName, genericParams, baseTypes, members, isUnion);
    s.isExtern = isExtern;
    s.isInternal = isInternal;

    return s;
}