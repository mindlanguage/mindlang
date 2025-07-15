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
        Keywords.Ptr
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

    foreach (k, mod; modules) {
        auto table = allTables[mod.name];

        foreach (fn; mod.functions) {
            analyzeFunctionBody(fn, table, allTables);
        }
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

    foreach (a; mod.aliases)
        table.addSymbol(new AliasSymbol(a, mod));

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
                if (!impInfo.members.canFind(firstMember)) {
                    return null;
                }
            }

            if (impInfo.moduleName in allModules) {
                auto modTable = allModules[impInfo.moduleName];

                auto subTypeRef = new TypeReference();
                subTypeRef.baseName = remainder;

                auto resolved = resolveTypeReference(subTypeRef, modTable, allModules);
                if (resolved !is null) {
                    if (!isAccessible(resolved.access, local.mod, modTable.mod))
                        return null;
                    return resolved;
                }
            }

            return null;
        }

        return null; // prefix not an import alias
    }

    // 3. Unqualified name resolution

    // a) Check local table
    if (auto sym = local.getSymbol(base))
        return sym;

    // b) Explicit member imports
    foreach (aliasName, impInfo; local.imports) {
        if (impInfo.members.length > 0 && impInfo.members.canFind(base)) {
            if (impInfo.moduleName in allModules) {
                auto modTable = allModules[impInfo.moduleName];
                if (auto sym = modTable.getSymbol(base)) {
                    if (!isAccessible(sym.access, local.mod, modTable.mod))
                        return null;
                    return sym;
                }
            }
        }
    }

    // c) Wildcard imports
    foreach (aliasName, impInfo; local.imports) {
        if (impInfo.members.length == 0 && impInfo.moduleName in allModules) {
            auto modTable = allModules[impInfo.moduleName];
            if (auto sym = modTable.getSymbol(base)) {
                if (!isAccessible(sym.access, local.mod, modTable.mod))
                    return null;
                return sym;
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
        if (sym is null)
            throw new CompilerException("Unresolved identifier: " ~ idExpr.name, idExpr.token);
        return unwrapAlias(sym);
    }

    if (auto qualified = cast(QualifiedAccessExpr) expr) {
        auto targetSym = resolveExpression(qualified.target, local, allTables);
        targetSym = unwrapAlias(targetSym);
        if (targetSym is null) {
            throw new CompilerException("Unresolved target in qualified access.", qualified.target.token);
        }

        auto structSym = cast(StructSymbol) targetSym;
        if (structSym is null) {
            throw new CompilerException("Target is not a struct and has no members.", qualified.target.token);
        }

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
        if (varStmt.variable.initializer !is null)
            resolveExpression(varStmt.variable.initializer, local, allTables);

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

void analyzeFunctionBody(FunctionDecl fn, SymbolTable moduleScope, SymbolTable[string] allModules) {
    auto functionScope = new SymbolTable(moduleScope.mod, moduleScope);

    // Add function parameters to local scope
    foreach (param; fn.params) {
        functionScope.addSymbol(new VariableSymbol(param, moduleScope.mod));
    }

    foreach (stmt; fn.statements) {
        resolveStatement(stmt, functionScope, allModules);
    }
}

Symbol unwrapAlias(Symbol sym) {
    if (auto a = cast(AliasSymbol) sym) {
        sym = a.resolvedTarget;
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