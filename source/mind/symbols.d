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
    Property,
    Interface,
    Template,
}

abstract class Symbol {
    string name;
    SymbolKind kind;
    Token token;

    this(string name, SymbolKind kind, Token token) {
        this.name = name;
        this.kind = kind;
        this.token = token;
    }
}

class VariableSymbol : Symbol {
    VariableDecl decl;

    this(VariableDecl decl) {
        super(decl.name, SymbolKind.Variable, decl.token);
        this.decl = decl;
    }
}

class AliasSymbol : Symbol {
    AliasStatement decl;
    Symbol resolvedTarget;

    this(AliasStatement decl) {
        super(decl.name, SymbolKind.Alias, decl.token);
        this.decl = decl;
    }
}

class FunctionSymbol : Symbol {
    FunctionDecl decl;

    this(FunctionDecl decl) {
        super(decl.name, SymbolKind.Function, decl.token);
        this.decl = decl;
    }
}

class StructSymbol : Symbol {
    StructDecl decl;

    this(StructDecl decl) {
        super(decl.name, SymbolKind.Struct, decl.token);
        this.decl = decl;
    }
}

class EnumSymbol : Symbol {
    EnumDecl decl;

    this(EnumDecl decl) {
        super(decl.name, SymbolKind.Enum, decl.token);
        this.decl = decl;
    }
}

class PropertySymbol : Symbol {
    PropStatement decl;

    this(PropStatement decl) {
        super(decl.name, SymbolKind.Property, decl.token);
        this.decl = decl;
    }
}

class InterfaceSymbol : Symbol {
    InterfaceDecl decl;

    this(InterfaceDecl decl) {
        super(decl.name, SymbolKind.Interface, decl.token);
        this.decl = decl;
    }
}

class TemplateSymbol : Symbol {
    TemplateDecl decl;

    this(TemplateDecl decl) {
        super(decl.name, SymbolKind.Template, decl.token);
        this.decl = decl;
    }
}

class BuiltinSymbol : Symbol {
    this(string name) {
        super(name, SymbolKind.Variable, Token.init); // Token is unused
    }
}

struct ImportInfo {
    string moduleName;
    string[] members;      // empty if whole module imported
}

class SymbolTable {
    Symbol[string] symbols;
    ImportInfo[string] imports;

    void addSymbol(Symbol sym) {
        if (sym.name in symbols) {
            throw new CompilerException("Duplicate symbol declaration.", sym);
        } else {
            symbols[sym.name] = sym;
        }
    }

    Symbol getSymbol(string name) {
        if (name in symbols) {
            return symbols[name];
        }
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
        return name in imports ? &imports[name] : null;
    }
}
