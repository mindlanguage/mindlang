module mind.symbols;

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
import mind.modules;
import mind.tokenizer;
import mind.types;

enum SymbolKind {
    Variable,
    Alias,
    Function,
    Struct,
    Enum,
    EnumValue,
    Property,
    Interface,
    Template,
    TypeParameter
}

class Symbol {
    string name;
    SymbolKind kind;
    Token token;
    AccessModifier access;
    Module mod;

    this(string name, SymbolKind kind, Token token, AccessModifier access, Module mod) {
        this.name = name;
        this.kind = kind;
        this.token = token;
        this.access = access;
        this.mod = mod;
    }
}

class VariableSymbol : Symbol {
    VariableDecl decl;

    this(VariableDecl decl, Module mod) {
        super(decl.name, SymbolKind.Variable, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class AliasSymbol : Symbol {
    AliasStatement decl;
    Symbol resolvedTarget;

    this(AliasStatement decl, Module mod) {
        super(decl.name, SymbolKind.Alias, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class FunctionSymbol : Symbol {
    FunctionDecl decl;

    this(FunctionDecl decl, Module mod) {
        super(decl.name, SymbolKind.Function, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class StructSymbol : Symbol {
    StructDecl decl;
    SymbolTable symbols;

    this(StructDecl decl, Module mod) {
        super(decl.name, SymbolKind.Struct, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class EnumSymbol : Symbol {
    EnumDecl decl;

    this(EnumDecl decl, Module mod) {
        super(decl.name, SymbolKind.Enum, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class EnumValueSymbol : Symbol {
    EnumDecl parentEnum;
    this(EnumDecl parentEnum, string name, Token token, Module mod) {
        super(name, SymbolKind.EnumValue, token, parentEnum.access, mod);
        this.parentEnum = parentEnum;
    }
}

class PropertySymbol : Symbol {
    PropStatement decl;

    this(PropStatement decl, Module mod) {
        super(decl.name, SymbolKind.Property, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class InterfaceSymbol : Symbol {
    InterfaceDecl decl;

    this(InterfaceDecl decl, Module mod) {
        super(decl.name, SymbolKind.Interface, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class TemplateSymbol : Symbol {
    TemplateDecl decl;

    this(TemplateDecl decl, Module mod) {
        super(decl.name, SymbolKind.Template, decl.token, decl.access, mod);
        this.decl = decl;
    }
}

class BuiltinSymbol : Symbol {
    this(string name, Module mod) {
        super(name, SymbolKind.Variable, Token.init, DefaultAccessModifier, mod); // Token is unused
    }
}

struct ImportInfo {
    string moduleName;
    string[] members;      // empty if whole module imported
}

class SymbolTable {
    Module mod;
    Symbol[string] symbols;
    ImportInfo[string] imports;
    SymbolTable parent;

    this(Module mod, SymbolTable parent = null) {
        this.mod = mod;
        this.parent = parent;
    }

    void addSymbol(Symbol sym) {
        if (sym.name in symbols) {
            throw new CompilerException("Duplicate symbol declaration: " ~ sym.name, sym);
        }

        auto current = parent;
        while (current !is null) {
            // Allow shadowing if current scope is module-level (parent is null)
            if (current.parent is null) {
                // current is module scope, so allow shadowing here
                break;
            }

            if (sym.name in current.symbols) {
                throw new CompilerException("Symbol '" ~ sym.name ~ "' shadows a declaration in an outer scope.", sym);
            }

            current = current.parent;
        }

        symbols[sym.name] = sym;
    }

    Symbol getSymbol(string name) {
        if (name in symbols)
            return symbols[name];
        if (parent !is null)
            return parent.getSymbol(name);
        return null;
    }

    VariableSymbol getVariable(string name) {
        auto sym = getSymbol(name);
        return cast(VariableSymbol) sym;
    }

    AliasSymbol getAlias(string name) {
        auto sym = getSymbol(name);
        return cast(AliasSymbol) sym;
    }

    FunctionSymbol getFunction(string name) {
        auto sym = getSymbol(name);
        return cast(FunctionSymbol) sym;
    }

    StructSymbol getStruct(string name) {
        auto sym = getSymbol(name);
        return cast(StructSymbol) sym;
    }

    EnumSymbol getEnum(string name) {
        auto sym = getSymbol(name);
        return cast(EnumSymbol) sym;
    }

    PropertySymbol getProperty(string name) {
        auto sym = getSymbol(name);
        return cast(PropertySymbol) sym;
    }

    InterfaceSymbol getInterface(string name) {
        auto sym = getSymbol(name);
        return cast(InterfaceSymbol) sym;
    }

    TemplateSymbol getTemplate(string name) {
        auto sym = getSymbol(name);
        return cast(TemplateSymbol) sym;
    }

    void addImport(ImportStatement imp) {
        auto key = imp.aliasName.length > 0 ? imp.aliasName : imp.moduleName;
        imports[key] = ImportInfo(imp.moduleName, imp.members);
    }

    ImportInfo* getImport(string name) {
        if (name in imports)
            return name in imports ? &imports[name] : null;
        if (parent !is null)
            return parent.getImport(name);
        return null;
    }
}
