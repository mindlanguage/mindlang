module mind.interfaces;

import mind.tokenizer;
import mind.properties;
import mind.functions;
import mind.attributes;
import mind.access;
import mind.parser;
import mind.errors;
import mind.keywords;
import mind.ast;
import mind.expressions;
import mind.types;

class InterfaceDecl {
    Token token;
    Attribute[] attributes;
    AccessModifier access;
    string name;
    string[] templateParams;
    TypeReference[] baseInterfaces;
    PropStatement[] properties;
    FunctionDecl[] functions;

    this(Attribute[] attributes, AccessModifier access, Token token, string name, string[] templateParams,
         TypeReference[] baseInterfaces, PropStatement[] properties, FunctionDecl[] functions) {
        this.attributes = attributes;
        this.access = access;
        this.token = token;
        this.name = name;
        this.templateParams = templateParams;
        this.baseInterfaces = baseInterfaces;
        this.properties = properties;
        this.functions = functions;
    }
}

InterfaceDecl parseInterface(Attribute[] attributes, AccessModifier access, ref Parser p) {
    auto interfaceToken = p.expect(TokenType.Identifier);
    if (interfaceToken.lexeme != Keywords.Interface)
        throw new CompilerException("Expected 'interface' keyword", interfaceToken);

    auto nameToken = p.expect(TokenType.Identifier);
    string name = nameToken.lexeme;

    if (isKeyword(nameToken.lexeme)) {
        throw new CompilerException("Cannot use keyword as identifier.", nameToken);
    }

    // Optional template parameters
    string[] templateParams;
    if (p.peek().type == TokenType.LParen) {
        p.next(); // consume '('
        while (true) {
            auto t = p.expect(TokenType.Identifier);
            templateParams ~= t.lexeme;

            if (p.peek().type == TokenType.RParen) {
                p.next(); // consume ')'
                break;
            }
            p.expect(TokenType.Comma);
        }
    }

    // Optional base interface after colon
    // Optional base interfaces after colon (multiple allowed, comma separated)
    TypeReference[] baseInterfaces;

    if (p.peek().type == TokenType.Colon) {
        p.next(); // consume ':'

        while (true) {
            auto baseIntf = parseTypeReference(p);
            baseInterfaces ~= baseIntf;

            if (p.peek().type == TokenType.Comma) {
                p.next(); // consume ','
                continue;
            }
            break;
        }
    }

    p.expect(TokenType.LBrace);

    PropStatement[] properties;
    FunctionDecl[] functions;

    while (!p.match(TokenType.RBrace)) {
        auto attrs = parseAttributes(p);
        auto accessModifier = parseAccessModifier(false, p);

        auto tok = p.peek();
        if (tok.lexeme == Keywords.Fn) {
            auto fn = parseFunction(attrs, accessModifier, true, p);
            if (fn.statements.length > 0)
                throw new CompilerException("Interface functions cannot have bodies.", fn.token);
            fn.attributes = attrs;
            functions ~= fn;
        }
        else if (tok.lexeme == Keywords.Prop) {
            auto prop = parsePropertyStatement(attrs, accessModifier, p);
            properties ~= prop;
        }
        else {
            throw new CompilerException("Only 'fn' and 'prop' are allowed inside interfaces.", tok);
        }
    }

    return new InterfaceDecl(attributes, access, interfaceToken, name, templateParams, baseInterfaces, properties, functions);
}
