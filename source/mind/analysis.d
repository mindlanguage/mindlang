module mind.analysis;

import std.string : indexOf;
import std.algorithm : canFind;
import std.traits : isSomeString;

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
import mind.symbols;
import mind.expressions;
import mind.ast;
import mind.statements;
import mind.semantic;

void analyzeTables(Module[string] modules, SymbolTable[string] allTables) {
    // Resolving aliases for modules
    foreach (k, mod; modules) {
        auto table = allTables[mod.name];

        // Validate aliases after building the table
        foreach (a; mod.aliases) {
            if (!validateTypeReference(a.type, table, allTables)) {
                throw new CompilerException("Unresolved type in alias: " ~ a.name, a.token);
            }
        }
    }
    
    validateAllImportsAndMembers(modules, allTables);
    resolveAliases(allTables);

    // Resolving modules
    foreach (k, mod; modules) {
        auto table = allTables[mod.name];

        // Variables
        foreach (var; mod.variables) {
            analyzeVariable(false, var, table, allTables);
        }

        // Properties
        foreach (prop; mod.properties) {
            analyzeProperty(prop, table, allTables);
        }

        // Enums
        foreach (e; mod.enums) {
            analyzeEnum(e, table, allTables);
        }

        // Functions
        foreach (fn; mod.functions) {
            analyzeFunction(fn, table, allTables);
        }

        // Interfaces
        foreach (i; mod.interfaces) {
            analyzeInterface(i, table, allTables);
        }
    }
}

void analyzeFunction(FunctionDecl fn, SymbolTable moduleScope, SymbolTable[string] allModules) {
    auto functionScope = new SymbolTable(moduleScope.mod, moduleScope);

    // Add function parameters to local scope
    foreach (param; fn.params) {
        analyzeVariable(true, param, functionScope, allModules);

        functionScope.addSymbol(new VariableSymbol(param, moduleScope.mod));
    }


    foreach (ret; fn.returnTypes) {
        if (!validateTypeReference(ret, functionScope, allModules)) {
            throw new CompilerException("Unresolved return type in function: " ~ fn.name, fn.token);
        }
    }

    foreach (stmt; fn.statements) {
        resolveStatement(stmt, functionScope, allModules);
    }
}

void analyzeVariable(bool isParam, VariableDecl variable, SymbolTable local, SymbolTable[string] allModules) {
    if (!validateTypeReference(variable.type, local, allModules)) {
        if (isParam) {
            throw new CompilerException("Unresolved type in parameter: " ~ variable.name, variable.token);
        } else {
            throw new CompilerException("Unresolved type for variable: " ~ variable.name, variable.token);
        }
    }

    if (variable.initializer !is null)
        resolveExpression(variable.initializer, local, allTables);
}

void analyzeProperty(PropStatement prop, SymbolTable local, SymbolTable[string] allModules) {
    VariableDecl createSetterParam(string name) {
        return new VariableDecl(DefaultAccessModifier, [], prop.token, VarKind.Const, name, null, null);
    }

    VariableSymbol thisSymbol;

    void injectThisIfNeeded(SymbolTable s) {
        if (thisSymbol !is null)
            s.addSymbol(thisSymbol);
    }

    // Validate property type
    if (!validateTypeReference(prop.type, local, allModules)) {
        throw new CompilerException("Unresolved type in property: " ~ prop.name, prop.token);
    }

    // Determine if we're in a struct (i.e. property belongs to a struct)
    Symbol thisTypeSym = local.getEnclosingStructSymbol();
    if (thisTypeSym !is null) {
        auto thisVar = new VariableDecl(DefaultAccessModifier, [], prop.token, VarKind.Const, "this", new TypeReference(thisTypeSym.name), null);
        thisSymbol = new VariableSymbol(thisVar, local.mod);
    }

    // Simple getter
    if (prop.getExpr !is null) {
        auto getterScope = new SymbolTable(local.mod, local);
        injectThisIfNeeded(getterScope);
        resolveExpression(prop.getExpr, getterScope, allModules);
    }

    // Simple setter
    if (prop.setLR !is null) {
        auto setterScope = new SymbolTable(local.mod, local);
        injectThisIfNeeded(setterScope);

        setterScope.addSymbol(new VariableSymbol(createSetterParam("value"), local.mod));
        resolveExpression(prop.setLR.leftExpression, setterScope, allModules);
        resolveExpression(prop.setLR.rightExpression, setterScope, allModules);
    }

    // Full getter
    if (prop.getBody !is null) {
        auto getterScope = new SymbolTable(local.mod, local);
        injectThisIfNeeded(getterScope);
        foreach (s; prop.getBody)
            resolveStatement(s, getterScope, allModules);
    }

    // Full setter
    if (prop.setBody !is null) {
        auto setterScope = new SymbolTable(local.mod, local);
        injectThisIfNeeded(setterScope);

        if (prop.setterParam !is null) {
            setterScope.addSymbol(new VariableSymbol(createSetterParam(prop.setterParam), local.mod));
        }

        foreach (s; prop.setBody)
            resolveStatement(s, setterScope, allModules);
    }
}

void analyzeEnum(EnumDecl e, SymbolTable local, SymbolTable[string] allModules) {
    // Validate backing type (optional, like `enum Color: int`)
    if (e.backingType !is null) {
        if (!validateTypeReference(e.backingType, local, allModules)) {
            throw new CompilerException("Unresolved enum backing type: " ~ e.name, e.token);
        }
    }

    // Create a scope for the enum values (shadowed inside the enum)
    auto enumScope = new SymbolTable(local.mod, local);

    // Analyze enum values
    foreach (value; e.values) {
        // Add the value as a symbol so it can be accessed (e.g., Color.red)
        auto valueSymbol = new EnumValueSymbol(e, value.name, value.nameToken, local.mod);
        enumScope.addSymbol(valueSymbol);

        // Analyze the initializer expression if it exists
        if (value.value !is null) {
            resolveExpression(value.value, enumScope, allModules);
        }
    }
}

void analyzeInterface(InterfaceDecl i, SymbolTable local, SymbolTable[string] allModules) {
    // Validate base interfaces (if any)
    foreach (baseInterface; i.baseInterfaces) {
        if (!validateTypeReference(baseInterface, local, allModules)) {
            throw new CompilerException("Unresolved base interface in: " ~ i.name, i.token);
        }
    }

    // Validate property types
    foreach (prop; i.properties) {
        if (!validateTypeReference(prop.type, local, allModules)) {
            throw new CompilerException("Unresolved type in interface property: " ~ prop.name, prop.token);
        }
    }

    // Validate function parameter and return types
    foreach (fn; i.functions) {
        foreach (param; fn.params) {
            if (!validateTypeReference(param.type, local, allModules)) {
                throw new CompilerException("Unresolved parameter type in interface method: " ~ fn.name, param.token);
            }
        }

        foreach (ret; fn.returnTypes) {
            if (!validateTypeReference(ret, local, allModules)) {
                throw new CompilerException("Unresolved return type in interface method: " ~ fn.name, fn.token);
            }
        }
    }
}