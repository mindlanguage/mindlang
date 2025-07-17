module mind.templates;

import mind.tokenizer;
import mind.functions;
import mind.variables;
import mind.attributes;
import mind.access;
import mind.parser;
import mind.errors;
import mind.keywords;
import mind.ast;
import mind.expressions;
import mind.types;
import mind.traits;

class TemplateDecl {
    Token token;
    Attribute[] attributes;
    AccessModifier access;
    string name;
    string[] templateParams;
    TraitEntry[] templateTraits;
    FunctionDecl[] functions;
    VariableDecl[] variables;

    this(Attribute[] attributes, AccessModifier access, Token token, string name, string[] templateParams,
         VariableDecl[] variables, FunctionDecl[] functions) {
        this.attributes = attributes;
        this.access = access;
        this.token = token;
        this.name = name;
        this.templateParams = templateParams;
        this.variables = variables;
        this.functions = functions;
    }
}

TemplateDecl parseTemplate(Attribute[] attributes, AccessModifier access, ref Parser p) {
    auto templateToken = p.expect(TokenType.Identifier);
    if (templateToken.lexeme != Keywords.Template)
        throw new CompilerException("Expected 'template' keyword", templateToken);

    auto nameToken = p.expect(TokenType.Identifier);
    string name = nameToken.lexeme;

    if (isKeyword(nameToken.lexeme)) {
        throw new CompilerException("Cannot use keyword as identifier.", nameToken);
    }

    // Optional template parameters
    string[] templateParams;
    bool[string] typeMap;
    TraitEntry[] templateTraits;
    if (p.peek().type == TokenType.LParen) {
        p.next(); // consume '('
        while (true) {
            auto t = p.expect(TokenType.Identifier);
            templateParams ~= t.lexeme;
            
            if (t.lexeme in typeMap) {
                throw new CompilerException("Duplicate type name.", t);
            }
            
            typeMap[t.lexeme] = true;

            if (p.peek().type == TokenType.Colon) {
                p.next(); // consume :
                
                string[] traitEntries;
                while (true) {
                    auto trait = p.expect(TokenType.Identifier);

                    traitEntries ~= trait.lexeme;

                    if (p.peek().type == TokenType.RParen)
                        break;
                    p.expect(TokenType.Comma);
                }

                templateTraits ~= new TraitEntry(traitEntries);
            } else {
                templateTraits ~= new TraitEntry([]);
            }

            if (p.peek().type == TokenType.RParen) {
                p.next(); // consume ')'
                break;
            }
            p.expect(TokenType.Comma);
        }
    }

    p.expect(TokenType.LBrace);

    VariableDecl[] variables;
    FunctionDecl[] functions;

    while (!p.match(TokenType.RBrace)) {
        auto attrs = parseAttributes(p);
        auto accessModifier = parseAccessModifier(false, p);

        auto tok = p.peek();

        if (tok.lexeme == Keywords.Fn) {
            auto fn = parseFunction(attrs, accessModifier, true, p);
            // Templates allow function bodies
            fn.attributes = attrs;
            functions ~= fn;
        }
        else if (tok.type == TokenType.Identifier && 
                 (tok.lexeme == Keywords.Let || tok.lexeme == Keywords.Mut || tok.lexeme == Keywords.Const || tok.lexeme == Keywords.Enum)) {
            auto variable = parseVariableDeclaration(attrs, false, VarKind.Let, accessModifier, p);
            variables ~= variable;
            p.expect(TokenType.Semicolon);
        }
        else {
            throw new CompilerException("Only 'fn' and variable declarations ('let', 'mut', 'const', 'enum') are allowed inside templates.", tok);
        }
    }

    auto temp = new TemplateDecl(attributes, access, templateToken, name, templateParams, variables, functions);
    temp.templateTraits = templateTraits;
    return temp;
}