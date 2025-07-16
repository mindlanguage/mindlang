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

        // Structs
        foreach (s; mod.structs) {
            analyzeStruct(s, table, allTables);
        }

        // Unittests
        foreach (u; mod.unittests) {
            analyzeUnittest(u, table, allTables);
        }

        // Templates
        foreach (t; mod.templates) {
            analyzeTemplate(t, table, allTables);
        }
    }
}

void analyzeFunction(FunctionDecl fn, SymbolTable local, SymbolTable[string] allModules, bool includeFunction = false) {
    auto functionScope = new SymbolTable(local.mod, local);

    // Handle template parameters
    foreach (paramName; fn.templateParams) {
        auto typeParamSymbol = new Symbol(paramName, SymbolKind.TypeParameter, fn.token, fn.access, functionScope.mod);
        functionScope.addSymbol(typeParamSymbol);
    }

    // Add function parameters to local scope
    foreach (param; fn.params) {
        analyzeVariable(true, param, functionScope, allModules);

        functionScope.addSymbol(new VariableSymbol(param, local.mod));
    }


    foreach (ret; fn.returnTypes) {
        if (!validateTypeReference(ret, functionScope, allModules)) {
            throw new CompilerException("Unresolved return type in function: " ~ fn.name, fn.token);
        }
    }

    foreach (stmt; fn.statements) {
        resolveStatement(stmt, functionScope, allModules, includeFunction ? fn : null);
    }
}

void analyzeVariable(bool isParam, VariableDecl variable, SymbolTable local, SymbolTable[string] allModules, FunctionDecl currentMethod = null) {
    if (!validateTypeReference(variable.type, local, allModules)) {
        if (isParam) {
            throw new CompilerException("Unresolved type in parameter: " ~ variable.name, variable.token);
        } else {
            throw new CompilerException("Unresolved type for variable: " ~ variable.name, variable.token);
        }
    }

    if (variable.initializer !is null)
        resolveExpression(variable.initializer, local, allTables, currentMethod);
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

void analyzeStruct(StructDecl s, SymbolTable local, SymbolTable[string] allModules) {
    InterfaceDecl[] implementedInterfaces;
    StructDecl baseStruct = null;

    auto structScope = new SymbolTable(local.mod, local);

    // Step 1: Process generic params (templates)
    foreach (paramName; s.genericParams) {
        auto typeParamSymbol = new Symbol(paramName, SymbolKind.TypeParameter, s.token, s.access, structScope.mod);
        structScope.addSymbol(typeParamSymbol);
    }

    // Step 2: Process and validate base types
    foreach (baseType; s.baseTypes) {
        if (!validateTypeReference(baseType, structScope, allModules)) {
            throw new CompilerException("Unresolved base type in struct: " ~ s.name, s.token);
        }

        auto baseSym = resolveTypeReference(baseType, structScope, allModules);
        auto unwrapped = unwrapAlias(baseSym, structScope, allModules);

        if (auto iface = cast(InterfaceSymbol) unwrapped) {
            implementedInterfaces ~= iface.decl;
            implementedInterfaces ~= collectAllBaseInterfaces(iface.decl, structScope, allModules);
        } else if (auto baseStructSym = cast(StructSymbol) unwrapped) {
            if (baseStruct !is null) {
                throw new CompilerException("Multiple base structs not allowed: " ~ s.name, s.token);
            }
            baseStruct = baseStructSym.decl;
        } else {
            throw new CompilerException("Invalid base type (must be struct or interface): " ~ baseType.baseName, s.token);
        }
    }

    // Step 3: Register current struct's own members
    Symbol[string] structMembers;

    foreach (member; s.members) {
        if (member.variable !is null) {
            if (!validateTypeReference(member.variable.type, structScope, allModules)) {
                throw new CompilerException("Unresolved type for struct variable: " ~ member.name, member.variable.token);
            }
            structMembers[member.name] = new VariableSymbol(member.variable, structScope.mod);
        } else if (member.fnDecl !is null) {
            analyzeFunction(member.fnDecl, structScope, allModules, true);
            structMembers[member.name] = new FunctionSymbol(member.fnDecl, structScope.mod);
        } else if (member.propStatement !is null) {
            analyzeProperty(member.propStatement, structScope, allModules);
            structMembers[member.name] = new PropertySymbol(member.propStatement, structScope.mod);
        } else if (member.unionDecl !is null) {
            analyzeStruct(member.unionDecl, structScope, allModules);
        }
        else if (member.unittestBlock !is null) {
            analyzeUnittest(member.unittestBlock, structScope, allModules);
        }
    }

    // Step 3: Validate that interface members are implemented
    foreach (iface; implementedInterfaces) {
        foreach (ifaceProp; iface.properties) {
            if (!isMemberImplemented(ifaceProp.name, ifaceProp.type, structMembers, baseStruct, structScope, allModules, true))
                throw new CompilerException("Missing property from interface: " ~ ifaceProp.name, s.token);
        }

        foreach (ifaceFn; iface.functions) {
            if (!isFunctionImplemented(ifaceFn, structMembers, baseStruct, structScope, allModules))
                throw new CompilerException("Missing function from interface: " ~ ifaceFn.name, s.token);
        }
    }
}

void analyzeUnittest(UnittestBlock unit, SymbolTable local, SymbolTable[string] allModules) {
    auto unittestScope = new SymbolTable(local.mod, local);

    // Analyze all statements inside the unittest body
    foreach (stmt; unit.body) {
        resolveStatement(stmt, unittestScope, allModules);
    }
}

void analyzeTemplate(TemplateDecl t, SymbolTable local, SymbolTable[string] allModules) {
    auto templateScope = new SymbolTable(local.mod, local);

    // Register template type parameters as symbols in the scope
    foreach (paramName; t.templateParams) {
        auto typeParamSymbol = new Symbol(paramName, SymbolKind.TypeParameter, t.token, t.access, local.mod);
        templateScope.addSymbol(typeParamSymbol);
    }

    // Analyze functions in the template scope
    foreach (fn; t.functions) {
        analyzeFunction(fn, templateScope, allModules);
    }

    // Analyze variables in the template scope
    foreach (var; t.variables) {
        analyzeVariable(false, var, templateScope, allModules);
    }
}