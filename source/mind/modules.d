module mind.modules;

import std.conv : to;

import mind.parser;
import mind.tokenizer;
import mind.keywords;
import mind.identifiers;
import mind.imports;
import mind.errors;
import mind.access;
import mind.aliases;
import mind.variables;
import mind.includes;
import mind.enums;
import mind.structs;
import mind.functions;
import mind.properties;
import mind.attributes;
import mind.interfaces;
import mind.templates;
import mind.unittests;

class ModuleInfo {
    Token token;
    string name;
    ImportStatement[] imports;
    AliasStatement[] aliases;
    VariableDecl[] variables;
    IncludeStatement[] includes;
    EnumDecl[] enums;
    StructDecl[] structs;
    FunctionDecl[] functions;
    PropStatement[] properties;
    InterfaceDecl[] interfaces;
    TemplateDecl[] templates;
    UnittestBlock[] unittests;

    this(Token token, string name) {
        this.token = token;
        this.name = name;
        imports = [];
        aliases = [];
        variables = [];
        includes = [];
        enums = [];
        structs = [];
        functions = [];
        properties = [];
        interfaces = [];
        templates = [];
        unittests = [];
    }
}

ModuleInfo parseModule(string sourceName, ref Parser parser) {
    ModuleInfo mod;
    void createDefaultModule() {
        if (!mod) {
            mod = new ModuleInfo(UnknownToken, sourceName);
        }
    }

    while (!parser.isEOF) {
        auto attributes = parseAttributes(parser);
        auto access = parseAccessModifier(false, parser);

        auto token = parser.peek();
        auto statementToken = token;

        if (token.type != TokenType.Identifier) {
            throw new CompilerException("Expected an identifier.", token);
        }

        switch (token.lexeme) {
            case Keywords.Module:
                if (mod) {
                    throw new CompilerException("Module already declared.", mod.token);
                }

                parser.expect(TokenType.Identifier);
                auto identifier = parseQualifiedIdentifier(parser);
                mod = new ModuleInfo(statementToken, identifier);

                parser.expect(TokenType.Semicolon);
                break;

            case Keywords.Import:
                createDefaultModule();
                
                auto imp = parseImportStatement(access, parser);
                mod.imports ~= imp;

                parser.expect(TokenType.Semicolon);
                break;

            case Keywords.Alias:
                createDefaultModule();

                auto a = parseAliasStatement(access, parser);
                mod.aliases ~= a;

                parser.expect(TokenType.Semicolon);
                break;

            case Keywords.Enum:
                createDefaultModule();

                auto e = parseEnumDeclaration(access, parser);

                if (!e) {
                    goto case Keywords.Let;
                }

                mod.enums ~= e;
                break;

            case Keywords.Let:
            case Keywords.Mut:
            case Keywords.Const:
                createDefaultModule();

                auto v = parseVariableDeclaration(attributes, false, VarKind.Let, access, parser);
                mod.variables ~= v;

                parser.expect(TokenType.Semicolon);
                break;

            case Keywords.Include:
                createDefaultModule();

                auto include = parseIncludeStatement(parser);
                mod.includes ~= include;

                parser.expect(TokenType.Semicolon);
                break;

            case Keywords.Struct:
                auto s = parseStructDeclaration(access, false, parser);

                mod.structs ~= s;
                break;

            case Keywords.Fn:
                auto fn = parseFunction(attributes, access, true, parser);

                mod.functions ~= fn;
                break;

            case Keywords.Prop:
                auto prop = parsePropertyStatement(attributes, access, parser);

                mod.properties ~= prop;
                break;

            case Keywords.Interface:
                auto intf = parseInterface(attributes, access, parser);

                mod.interfaces ~= intf;
                break;

            case Keywords.Template:
                auto temp = parseTemplate(attributes, access, parser);

                mod.templates ~= temp;
                break;

            case Keywords.Unittest:
                auto unit = parseUnittestBlock(parser);

                mod.unittests ~= unit;
                break;

            default:
                throw new CompilerException("Invalid identifier for module scope.", token);
        }
    }

    return mod;
}