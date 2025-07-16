module mind.semantic;

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

Symbol[string] builtinSymbols;

void initBuiltinSymbols() {
    string[] builtinTypeNames = [
        Keywords.Int8,
        Keywords.Int16,
        Keywords.Int32,
        Keywords.Int64,
        Keywords.UInt8,
        Keywords.UInt16,
        Keywords.UInt32,
        Keywords.UInt64,
        Keywords.Float,
        Keywords.Double,
        Keywords.Real,
        Keywords.Char,
        Keywords.Void,
        Keywords.Bool,
        Keywords.Ptr,
        Keywords.Size_T,
        Keywords.Ptrdiff_T
    ];

    foreach (typeName; builtinTypeNames) {
        builtinSymbols[typeName] = new BuiltinSymbol(typeName, new Module(UnknownToken, "__builtin"));
    }
}

SymbolTable[string] allTables;

SymbolTable[string] createTables(Module[string] modules) {
    // Building symbol tables for modules
    foreach (k, mod; modules) {
        auto table = buildSymbolTable(mod);
        allTables[mod.name] = table;
    }

    return allTables;
}

SymbolTable buildSymbolTable(Module mod) {
    auto table = new SymbolTable(mod);

    foreach (imp; mod.imports)
        table.addImport(imp);

    foreach (v; mod.variables)
        table.addSymbol(new VariableSymbol(v, mod));

    foreach (f; mod.functions)
        table.addSymbol(new FunctionSymbol(f, mod));

    foreach (s; mod.structs)
        table.addSymbol(new StructSymbol(s, mod));

    foreach (e; mod.enums)
        table.addSymbol(new EnumSymbol(e, mod));

    foreach (p; mod.properties)
        table.addSymbol(new PropertySymbol(p, mod));

    foreach (i; mod.interfaces)
        table.addSymbol(new InterfaceSymbol(i, mod));

    foreach (t; mod.templates)
        table.addSymbol(new TemplateSymbol(t, mod));

    foreach (a; mod.aliases) {
        auto aliasSymbol = new AliasSymbol(a, mod);
        table.addSymbol(aliasSymbol);
    }

    return table;
}

void resolveAliases(SymbolTable[string] tables) {
    foreach (moduleName, table; tables) {
        foreach (name, sym; table.symbols) {
            auto a = cast(AliasSymbol) sym;
            if (a is null)
                continue;

            auto resolved = resolveTypeReference(a.decl.type, table, tables);

            if (resolved is null) {
                throw new CompilerException("Could not resolve alias target: " ~ a.decl.type.baseName, a);
            }

            a.resolvedTarget = resolved;
        }
    }
}

Symbol resolveTypeReference(TypeReference typeRef, SymbolTable local, SymbolTable[string] allModules) {
    auto base = typeRef.baseName;

    // 1. Check builtins
    if (auto built = base in builtinSymbols)
        return *built;

    // 2. Qualified name check (moduleAlias.symbol)
    auto dotIndex = indexOf(base, ".");
    if (dotIndex != -1) {
        auto prefix = base[0 .. dotIndex];         // e.g. 'AX'
        auto remainder = base[dotIndex + 1 .. $];  // e.g. 'Foo' or 'Foo.Bar'

        auto impInfo = local.getImport(prefix);
        if (impInfo !is null) {
            // Check explicit member import
            if (impInfo.members.length > 0) {
                auto firstDot = indexOf(remainder, ".");
                string firstMember = (firstDot == -1) ? remainder : remainder[0 .. firstDot];
                if (!impInfo.members.canFind(firstMember))
                    return null;
            }

            if (impInfo.moduleName in allModules) {
                auto modTable = allModules[impInfo.moduleName];
                auto subTypeRef = new TypeReference();
                subTypeRef.baseName = remainder;

                auto resolved = resolveTypeReference(subTypeRef, modTable, allModules);
                if (resolved !is null && isAccessible(resolved.access, local.mod, modTable.mod))
                    return unwrapAlias(resolved, local, allModules);

                return null;
            }

            return null;
        }

        return null;
    }

    // 3. Unqualified name resolution

    // a) Check local table
    SymbolTable current = local;
    while (current !is null) {
        if (auto sym = getSymbolWithImports(base, local, allModules)) {
            return unwrapAlias(sym, local, allModules);
        }
        current = current.parent;
    }

    // b) Explicit member imports
    foreach (aliasName, impInfo; local.imports) {
        if (impInfo.members.length > 0 && impInfo.members.canFind(base)) {
            if (impInfo.moduleName in allModules) {
                auto modTable = allModules[impInfo.moduleName];
                if (auto sym = modTable.getSymbol(base)) {
                    if (isAccessible(sym.access, local.mod, modTable.mod))
                        return unwrapAlias(sym, local, allModules); // unwrap
                }
            }
        }
    }

    // c) Wildcard imports
    foreach (aliasName, impInfo; local.imports) {
        if (impInfo.members.length == 0 && impInfo.moduleName in allModules) {
            auto modTable = allModules[impInfo.moduleName];
            if (auto sym = modTable.getSymbol(base)) {
                if (isAccessible(sym.access, local.mod, modTable.mod))
                    return unwrapAlias(sym, local, allModules); // unwrap
            }
        }
    }

    return null;
}

bool validateTypeReference(TypeReference typeRef, SymbolTable local, SymbolTable[string] allModules) {
    if (typeRef is null)
        return true;

    auto baseSymbol = resolveTypeReference(typeRef, local, allModules);

    if (baseSymbol is null)
        return false; // Base type not found

    // Recursively validate generic type arguments
    foreach (arg; typeRef.typeArguments) {
        if (!validateTypeReference(arg, local, allModules))
            return false;
    }

    // Array type element
    if (typeRef.arrayElementType !is null) {
        if (!validateTypeReference(typeRef.arrayElementType, local, allModules))
            return false;
    }

    // Map key type
    if (typeRef.keyType !is null) {
        if (!validateTypeReference(typeRef.keyType, local, allModules))
            return false;
    }

    return true;
}

void validateImportMembers(Module mod, SymbolTable[string] allModules) {
    foreach (imp; mod.imports) {
        auto moduleName = imp.moduleName;
        if (!(moduleName in allModules)) {
            throw new CompilerException(
                "Imported module '" ~ moduleName ~ "' does not exist.",
                imp.token
            );
        }

        auto moduleTable = allModules[moduleName];

        // If members are specified explicitly, check each exists in the module
        if (imp.members.length > 0) {
            foreach (memberName; imp.members) {
                auto member = moduleTable.symbols.get(memberName, null);

                if (!member) {
                    throw new CompilerException(
                        "Imported member '" ~ memberName ~ "' does not exist in module '" ~ moduleName ~ "'.",
                        imp.token
                    );
                }
                
                if (!isAccessible(member.access, mod, moduleTable.mod)) {
                    throw new CompilerException("Cannot access the import member. '"~ memberName ~"'", imp.token);
                }
            }
        }
    }
}

void validateAllImportsAndMembers(Module[string] modules, SymbolTable[string] allTables) {
    foreach (modName, mod; modules) {
        validateImportMembers(mod, allTables);
    }
}

bool isAccessible(AccessModifier access, Module fromModule, Module declaringModule) {
    final switch (access.level) {
        case AccessLevel.Public:
        case AccessLevel.Default:
            return true;
        case AccessLevel.Private:
            return fromModule.id == declaringModule.id;
        case AccessLevel.Package:
            return getPackageName(fromModule) == getPackageName(declaringModule);
        case AccessLevel.Protected:
            // Extend this for subclass logic
            return fromModule.id == declaringModule.id;
    }
}

string getPackageName(Module mod) {
    auto moduleName = mod.name;

    auto firstDot = moduleName.indexOf(".");
    return (firstDot == -1) ? moduleName : moduleName[0 .. firstDot];
}

Symbol resolveExpression(Expr expr, SymbolTable local, SymbolTable[string] allTables) {
    if (auto idExpr = cast(IdentifierExpr) expr) {
        auto sym = getSymbolWithImports(idExpr.name, local, allTables);

        if (sym !is null)
            return unwrapAlias(sym, local, allTables);

        // If not found, and inside a method of a struct, check `this`
        auto methodStruct = findEnclosingStructFromMethod(local);
        if (methodStruct !is null) {
            foreach (member; methodStruct.decl.members) {
                if (member.name == idExpr.name) {
                    // Convert `a` to `this.a` and resolve again
                    auto thisExpr = new IdentifierExpr("this", idExpr.token);
                    auto qualified = new QualifiedAccessExpr(thisExpr, idExpr, idExpr.token);
                    return resolveExpression(qualified, local, allTables);
                }
            }
        }

        throw new CompilerException("Unresolved identifier: " ~ idExpr.name, idExpr.token);
    }

    if (auto qualified = cast(QualifiedAccessExpr) expr) {
        auto targetSym = resolveExpression(qualified.target, local, allTables);
        targetSym = unwrapAlias(targetSym, local, allTables);

        if (targetSym is null) {
            throw new CompilerException("Unresolved target in qualified access.", qualified.target.token);
        }

        // If the target is a variable (like msg), resolve its type
        Symbol typeSym = null;

        if (auto varSym = cast(VariableSymbol) targetSym) {
            auto typeRef = varSym.decl.type;
            if (typeRef is null)
                throw new CompilerException("Variable has no type", varSym.decl.token);
            auto resolvedType = resolveTypeReference(typeRef, local, allTables);
            if (resolvedType is null)
                throw new CompilerException("Failed to resolve variable type: " ~ typeRef.baseName, varSym.decl.token);
            typeSym = unwrapAlias(resolvedType, local, allTables);
            if (typeSym is null)
                throw new CompilerException("Failed to unwrap alias for variable type: " ~ typeRef.baseName, varSym.decl.token);
        } else {
            // Maybe the symbol is already a type (like a struct instance)
            typeSym = targetSym;
        }

        // First check if it's a struct
        if (auto structSym = cast(StructSymbol) typeSym) {
            foreach (memberDecl; structSym.decl.members) {
                if (memberDecl.name == qualified.member.name) {
                    if (!isAccessible(memberDecl.access, local.mod, structSym.mod)) {
                        throw new CompilerException("Member is not accessible: " ~ memberDecl.name, qualified.member.token);
                    }
                    return findSymbolForMember(memberDecl, structSym.mod);
                }
            }

            throw new CompilerException("No such member: " ~ qualified.member.name, qualified.member.token);
        }

        // If not a struct, maybe it's an enum
        if (auto enumSym = cast(EnumSymbol) typeSym) {
            foreach (value; enumSym.decl.values) {
                if (value.name == qualified.member.name) {
                    // We return the enum value name as a synthetic symbol.
                    // Later on, for type checking, you'll want to associate these with their enum type.
                    return new VariableSymbol(
                        new VariableDecl(
                            DefaultAccessModifier, [], value.nameToken, VarKind.Const, value.name, null, value.value
                        ),
                        enumSym.mod
                    );
                }
            }

            throw new CompilerException("No such enum value: " ~ qualified.member.name, qualified.member.token);
        }

        // Not a type that can have members
        throw new CompilerException("Target expression does not have members (not a struct or enum).", qualified.target.token);
    }

    if (auto call = cast(CallExpr) expr) {
        resolveExpression(call.callee, local, allTables);
        foreach (arg; call.arguments)
            resolveExpression(arg, local, allTables);
        return null;
    }

    if (auto bin = cast(BinaryExpr) expr) {
        resolveExpression(bin.left, local, allTables);
        resolveExpression(bin.right, local, allTables);
        return null;
    }

    if (auto group = cast(GroupingExpr) expr) {
        return resolveExpression(group.expression, local, allTables);
    }

    if (auto unary = cast(UnaryExpr) expr) {
        return resolveExpression(unary.operand, local, allTables);
    }

    if (auto arrayIdx = cast(ArrayIndexExpr) expr) {
        resolveExpression(arrayIdx.arrayExpr, local, allTables);
        resolveExpression(arrayIdx.indexExpr, local, allTables);
        return null;
    }

    if (auto castExpr = cast(CastExpr) expr) {
        resolveExpression(castExpr.expr, local, allTables);
        return null;
    }

    if (auto templ = cast(TemplatedExpr) expr) {
        return resolveExpression(templ.target, local, allTables);
    }

    if (auto newExpr = cast(NewExpr) expr) {
        return resolveExpression(newExpr.typeExpr, local, allTables);
    }

    if (auto switchExpr = cast(SwitchExpr) expr) {
        resolveExpression(switchExpr.condition, local, allTables);
        foreach (c; switchExpr.cases) {
            resolveExpression(c.value, local, allTables);
            resolveExpression(c.body, local, allTables);
        }
        if (switchExpr.defaultCase !is null) {
            resolveExpression(switchExpr.defaultCase.body, local, allTables);
        }
        return null;
    }

    if (auto lambda = cast(LambdaExpr) expr) {
        foreach (s; lambda.bodyStatements)
            resolveStatement(s, local, allTables);
        resolveExpression(lambda.bodyExpression, local, allTables);
        return null;
    }

    if (auto interp = cast(InterpolatedStringExpr) expr) {
        foreach (part; interp.parts)
            resolveExpression(part, local, allTables);
        return null;
    }

    return null; // Includes literals
}

Symbol findSymbolForMember(StructMember memberDecl, Module structModule) {
    // If member is a VariableDecl
    auto varDecl = memberDecl.variable;
    if (varDecl !is null) {
        return new VariableSymbol(varDecl, structModule);
    }

    // If member is a PropStatement (property)
    auto propDecl = memberDecl.propStatement;
    if (propDecl !is null) {
        return new PropertySymbol(propDecl, structModule);
    }

    // If member is a FunctionDecl
    auto funcDecl = memberDecl.fnDecl;
    if (funcDecl !is null) {
        return new FunctionSymbol(funcDecl, structModule);
    }

    // Add other member declaration types as needed
    // e.g., enums, nested structs, etc.

    // If type unknown or unsupported, return null
    return null;
}

void resolveStatement(Statement stmt, SymbolTable local, SymbolTable[string] allTables) {
    if (auto lr = cast(LRStatement) stmt) {
        resolveExpression(lr.leftExpression, local, allTables);
        resolveExpression(lr.rightExpression, local, allTables);
        return;
    }

    if (auto ret = cast(ReturnStatement) stmt) {
        if (ret.returnExpression !is null)
            resolveExpression(ret.returnExpression, local, allTables);
        return;
    }

    if (auto exprStmt = cast(ExprStatement) stmt) {
        resolveExpression(exprStmt.expression, local, allTables);
        return;
    }

    if (auto ifStmt = cast(IfStatement) stmt) {
        resolveExpression(ifStmt.condition, local, allTables);

        // New scope for if body
        auto thenScope = new SymbolTable(local.mod, local);
        foreach (s; ifStmt.body)
            resolveStatement(s, thenScope, allTables);

        if (ifStmt.elseBranch !is null) {
            auto elseScope = new SymbolTable(local.mod, local);
            resolveStatement(ifStmt.elseBranch, elseScope, allTables);
        }
        return;
    }

    if (auto blockStmt = cast(BlockStatement) stmt) {
        auto blockScope = new SymbolTable(local.mod, local);
        foreach (s; blockStmt.statements)
            resolveStatement(s, blockScope, allTables);
        return;
    }

    if (auto switchStmt = cast(SwitchStatement) stmt) {
        resolveExpression(switchStmt.condition, local, allTables);

        foreach (c; switchStmt.cases) {
            resolveExpression(c.value, local, allTables);
            auto caseScope = new SymbolTable(local.mod, local);
            foreach (s; c.body)
                resolveStatement(s, caseScope, allTables);
        }

        if (switchStmt.defaultClause !is null) {
            auto defScope = new SymbolTable(local.mod, local);
            foreach (s; switchStmt.defaultClause.body)
                resolveStatement(s, defScope, allTables);
        }
        return;
    }

    if (auto guardStmt = cast(GuardStatement) stmt) {
        resolveExpression(guardStmt.condition, local, allTables);
        if (guardStmt.elseExpression !is null)
            resolveExpression(guardStmt.elseExpression, local, allTables);
        return;
    }

    if (auto fnDeclStmt = cast(FunctionDeclStatement) stmt) {
        // Function symbols are already in the outer scope,
        // but we can analyze the body with new local scope if needed
        auto fnScope = new SymbolTable(local.mod, local);
        foreach (param; fnDeclStmt.fn.params)
            fnScope.addSymbol(new VariableSymbol(param, local.mod));

        foreach (s; fnDeclStmt.fn.statements)
            resolveStatement(s, fnScope, allTables);
        return;
    }

    if (auto whileStmt = cast(WhileStatement) stmt) {
        resolveExpression(whileStmt.condition, local, allTables);
        auto loopScope = new SymbolTable(local.mod, local);
        resolveStatement(whileStmt.body, loopScope, allTables);
        return;
    }

    if (auto doWhileStmt = cast(DoWhileStatement) stmt) {
        auto loopScope = new SymbolTable(local.mod, local);
        resolveStatement(doWhileStmt.body, loopScope, allTables);
        resolveExpression(doWhileStmt.condition, local, allTables);
        return;
    }

    if (auto forStmt = cast(ForStatement) stmt) {
        auto forScope = new SymbolTable(local.mod, local);
        if (forStmt.initializer !is null)
            resolveStatement(new VariableStatement(forStmt.initializer.token, forStmt.initializer), forScope, allTables);
        if (forStmt.condition !is null)
            resolveExpression(forStmt.condition, forScope, allTables);
        if (forStmt.update !is null)
            resolveStatement(forStmt.update, forScope, allTables);
        resolveStatement(forStmt.body, forScope, allTables);
        return;
    }

    if (auto foreachStmt = cast(ForeachStatement) stmt) {
        auto foreachScope = new SymbolTable(local.mod, local);

        // Register each loop variable (1 or 2 identifiers)
        foreach (name; foreachStmt.identifiers) {
            auto syntheticVar = new VariableDecl(DefaultAccessModifier, [], foreachStmt.foreachToken, VarKind.Const, name, null, null);
            foreachScope.addSymbol(new VariableSymbol(syntheticVar, local.mod));
        }

        // Resolve iterable or range start
        resolveExpression(foreachStmt.iterableOrStart, foreachScope, allTables);

        // If it’s a range-based loop, resolve end expression
        if (foreachStmt.endRange !is null)
            resolveExpression(foreachStmt.endRange, foreachScope, allTables);

        // Resolve body in loop scope
        resolveStatement(foreachStmt.body, foreachScope, allTables);
        return;
    }

    if (auto varStmt = cast(VariableStatement) stmt) {
        analyzeVariable(false, varStmt.variable, local, allTables);

        local.addSymbol(new VariableSymbol(varStmt.variable, local.mod));
        return;
    }

    if (auto assertStmt = cast(AssertStatement) stmt) {
        resolveExpression(assertStmt.condition, local, allTables);
        if (assertStmt.message !is null)
            resolveExpression(assertStmt.message, local, allTables);
        return;
    }

    if (cast(BreakStatement) stmt || cast(ContinueStatement) stmt) {
        return; // Nothing to resolve
    }

    // TODO: Add more statement types as your language grows
}

Symbol unwrapAlias(Symbol sym, SymbolTable local, SymbolTable[string] allModules) {
    while (true) {
        auto a = cast(AliasSymbol) sym;
        if (a is null)
            break;

        // Use already-resolved target if available
        if (a.resolvedTarget is null) {
            if (a.decl.type is null)
                return null;

            a.resolvedTarget = resolveTypeReference(a.decl.type, local, allModules);
        }

        sym = a.resolvedTarget;
        if (sym is null)
            return null;
    }

    return sym;
}

Symbol getSymbolWithImports(string name, SymbolTable startTable, SymbolTable[string] allModules) {
    SymbolTable current = startTable;

    while (current !is null) {
        // First: try local symbol
        if (auto sym = current.getSymbol(name))
            return sym;

        // Check imports only at the module level (where parent is null)
        if (current.parent is null) {
            // Explicit imports
            foreach (aliasName, imp; current.imports) {
                if (imp.members.length > 0 && imp.members.canFind(name)) {
                    if (imp.moduleName in allModules) {
                        auto modTable = allModules[imp.moduleName];
                        if (auto sym = modTable.getSymbol(name)) {
                            if (isAccessible(sym.access, startTable.mod, modTable.mod))
                                return sym;
                        }
                    }
                }
            }

            // Wildcard imports
            foreach (aliasName, imp; current.imports) {
                if (imp.members.length == 0 && imp.moduleName in allModules) {
                    auto modTable = allModules[imp.moduleName];
                    if (auto sym = modTable.getSymbol(name)) {
                        if (isAccessible(sym.access, startTable.mod, modTable.mod))
                            return sym;
                    }
                }
            }
        }

        current = current.parent;
    }

    return null;
}

StructSymbol findEnclosingStructFromMethod(SymbolTable sc) {
    while (sc !is null) {
        foreach (s; sc.symbols) {
            if (auto fn = cast(FunctionSymbol) s) {
                if (fn.decl.isMethod) { // Add a flag if needed
                    // Assume method is declared within a struct
                    auto structMod = fn.mod;
                    foreach (sym; allTables[structMod.name].symbols) {
                        if (auto st = cast(StructSymbol) sym)
                            return st;
                    }
                }
            }
        }

        sc = sc.parent;
    }

    return null;
}

Symbol getEnclosingStructSymbol(SymbolTable table) {
    auto current = table;
    while (current !is null) {
        foreach (sym; current.symbols.values) {
            if (auto structSym = cast(StructSymbol) sym)
                return structSym;
        }
        current = current.parent;
    }
    return null;
}

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
            analyzeFunctionBody(fn, table, allTables);
        }
    }
}

void analyzeFunctionBody(FunctionDecl fn, SymbolTable moduleScope, SymbolTable[string] allModules) {
    auto functionScope = new SymbolTable(moduleScope.mod, moduleScope);

    // Add function parameters to local scope
    foreach (param; fn.params) {
        analyzeVariable(true, param, functionScope, allModules);

        functionScope.addSymbol(new VariableSymbol(param, moduleScope.mod));
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