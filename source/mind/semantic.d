module mind.semantic;

import std.string : indexOf;
import std.algorithm : canFind;
import std.traits : isSomeString;
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
import mind.modules;
import mind.tokenizer;
import mind.types;
import mind.symbols;
import mind.expressions;
import mind.ast;
import mind.statements;
import mind.analysis;
import mind.settings;

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

    foreach (t; mod.traits)
        table.addSymbol(new TraitSymbol(t, mod));

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

    // 2. Qualified name check (e.g. AX.Foo or AX.Foo.Bar)
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
                if (resolved !is null && isAccessible(resolved.access, local.mod, modTable.mod)) {
                    auto unwrapped = unwrapAlias(resolved, local, allModules);
                    if (isValidTypeSymbol(unwrapped))
                        return unwrapped;
                }
                return null;
            }

            return null;
        }

        return null;
    }

    // 3. Unqualified name resolution
    SymbolTable current = local;
    while (current !is null) {
        if (auto sym = getSymbolWithImports(base, local, allModules)) {
            auto unwrapped = unwrapAlias(sym, local, allModules);
            if (isValidTypeSymbol(unwrapped))
                return unwrapped;
            return null; // Found, but not a valid type
        }
        current = current.parent;
    }

    // 4. Explicit member imports
    foreach (aliasName, impInfo; local.imports) {
        if (impInfo.members.length > 0 && impInfo.members.canFind(base)) {
            if (impInfo.moduleName in allModules) {
                auto modTable = allModules[impInfo.moduleName];
                if (auto sym = modTable.getSymbol(base)) {
                    if (isAccessible(sym.access, local.mod, modTable.mod)) {
                        auto unwrapped = unwrapAlias(sym, local, allModules);
                        if (isValidTypeSymbol(unwrapped))
                            return unwrapped;
                    }
                }
            }
        }
    }

    // 5. Wildcard imports
    foreach (aliasName, impInfo; local.imports) {
        if (impInfo.members.length == 0 && impInfo.moduleName in allModules) {
            auto modTable = allModules[impInfo.moduleName];
            if (auto sym = modTable.getSymbol(base)) {
                if (isAccessible(sym.access, local.mod, modTable.mod)) {
                    auto unwrapped = unwrapAlias(sym, local, allModules);
                    if (isValidTypeSymbol(unwrapped))
                        return unwrapped;
                }
            }
        }
    }

    return null;
}

bool isValidTypeSymbol(Symbol sym) {
    switch (sym.kind) {
        case SymbolKind.Struct,
             SymbolKind.Enum,
             SymbolKind.Interface,
             SymbolKind.Alias,
             SymbolKind.TypeParameter:
            return true;
        default:
            return false;
    }
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

Symbol resolveExpression(Expr expr, SymbolTable local, SymbolTable[string] allTables, FunctionDecl currentMethod = null) {
    if (auto idExpr = cast(IdentifierExpr) expr) {
        auto sym = getSymbolWithImports(idExpr.name, local, allTables);

        if (sym !is null)
            return unwrapAlias(sym, local, allTables);

        if (currentMethod !is null) {
            auto methodStruct = findEnclosingStructFromMethod(local, currentMethod);
            if (methodStruct !is null) {
                foreach (member; methodStruct.decl.members) {
                    if (member.name == idExpr.name) {
                        auto thisExpr = new IdentifierExpr("this", idExpr.token);
                        auto qualified = new QualifiedAccessExpr(thisExpr, idExpr, idExpr.token);
                        return resolveExpression(qualified, local, allTables, currentMethod);
                    }
                }
            }
        }

        throw new CompilerException("Unresolved identifier: " ~ idExpr.name, idExpr.token);
    }

    if (auto qualified = cast(QualifiedAccessExpr) expr) {
        auto targetSym = resolveExpression(qualified.target, local, allTables, currentMethod);
        targetSym = unwrapAlias(targetSym, local, allTables);

        if (targetSym is null) {
            throw new CompilerException("Unresolved target in qualified access.", qualified.target.token);
        }

        Symbol typeSym = null;

        if (auto varSym = cast(VariableSymbol) targetSym) {
            auto typeRef = varSym.decl.type;
            if (typeRef is null)
                throw new CompilerException("Variable has no type", varSym.decl.token);

            // --- POINTER UNWRAP LOGIC START ---
            // Check if the variable's type is ptr!T
            if (typeRef.baseName == "ptr" && typeRef.typeArguments.length == 1) {
                // Resolve pointee type T instead of ptr!T
                auto pointeeTypeRef = typeRef.typeArguments[0];
                auto resolvedPointeeType = resolveTypeReference(pointeeTypeRef, local, allTables);
                if (resolvedPointeeType is null)
                    throw new CompilerException("Failed to resolve pointee type: " ~ pointeeTypeRef.baseName, varSym.decl.token);
                typeSym = unwrapAlias(resolvedPointeeType, local, allTables);
            } else {
                // Normal variable type resolution
                auto resolvedType = resolveTypeReference(typeRef, local, allTables);
                if (resolvedType is null)
                    throw new CompilerException("Failed to resolve variable type: " ~ typeRef.baseName, varSym.decl.token);
                typeSym = unwrapAlias(resolvedType, local, allTables);
            }
            // --- POINTER UNWRAP LOGIC END ---

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

    // --- The rest remains unchanged ---

    if (auto call = cast(CallExpr) expr) {
        resolveExpression(call.callee, local, allTables, currentMethod);
        foreach (arg; call.arguments)
            resolveExpression(arg, local, allTables, currentMethod);
        return null;
    }

    if (auto bin = cast(BinaryExpr) expr) {
        resolveExpression(bin.left, local, allTables, currentMethod);
        resolveExpression(bin.right, local, allTables, currentMethod);
        return null;
    }

    if (auto group = cast(GroupingExpr) expr) {
        return resolveExpression(group.expression, local, allTables, currentMethod);
    }

    if (auto unary = cast(UnaryExpr) expr) {
        return resolveExpression(unary.operand, local, allTables, currentMethod);
    }

    if (auto arrayIdx = cast(ArrayIndexExpr) expr) {
        resolveExpression(arrayIdx.arrayExpr, local, allTables, currentMethod);
        resolveExpression(arrayIdx.indexExpr, local, allTables, currentMethod);
        return null;
    }

    if (auto castExpr = cast(CastExpr) expr) {
        resolveExpression(castExpr.expr, local, allTables, currentMethod);
        return null;
    }

    if (auto templ = cast(TemplatedExpr) expr) {
        if (auto id = cast(IdentifierExpr) templ.target) {
            if (id.name == Keywords.Sizeof) {
                if (templ.templateArgs.length != 1)
                    throw new CompilerException("sizeof expects 1 argument", templ.token);
                
                if (auto typeExpr = cast(TypeExpr) templ.templateArgs[0]) {
                    auto resolved = resolveTypeReference(exprToTypeReference(typeExpr.innerType), local, allTables);
                    return resolveSizeof(resolved, local, allTables);
                }
                throw new CompilerException("Expected a type in sizeof!", templ.token);
            }
        }
    }

    if (auto newExpr = cast(NewExpr) expr) {
        return resolveExpression(newExpr.typeExpr, local, allTables, currentMethod);
    }

    if (auto switchExpr = cast(SwitchExpr) expr) {
        resolveExpression(switchExpr.condition, local, allTables, currentMethod);
        foreach (c; switchExpr.cases) {
            resolveExpression(c.value, local, allTables, currentMethod);
            resolveExpression(c.body, local, allTables, currentMethod);
        }
        if (switchExpr.defaultCase !is null) {
            resolveExpression(switchExpr.defaultCase.body, local, allTables, currentMethod);
        }
        return null;
    }

    if (auto lambda = cast(LambdaExpr) expr) {
        foreach (s; lambda.bodyStatements)
            resolveStatement(s, local, allTables, currentMethod);
        resolveExpression(lambda.bodyExpression, local, allTables, currentMethod);
        return null;
    }

    if (auto interp = cast(InterpolatedStringExpr) expr) {
        foreach (part; interp.parts)
            resolveExpression(part, local, allTables, currentMethod);
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

void resolveStatement(Statement stmt, SymbolTable local, SymbolTable[string] allTables, FunctionDecl currentMethod = null) {
    if (auto lr = cast(LRStatement) stmt) {
        resolveExpression(lr.leftExpression, local, allTables, currentMethod);
        resolveExpression(lr.rightExpression, local, allTables, currentMethod);
        return;
    }

    if (auto ret = cast(ReturnStatement) stmt) {
        if (ret.returnExpression !is null)
            resolveExpression(ret.returnExpression, local, allTables, currentMethod);
        return;
    }

    if (auto exprStmt = cast(ExprStatement) stmt) {
        resolveExpression(exprStmt.expression, local, allTables, currentMethod);
        return;
    }

    if (auto ifStmt = cast(IfStatement) stmt) {
        resolveExpression(ifStmt.condition, local, allTables, currentMethod);

        // New scope for if body
        auto thenScope = new SymbolTable(local.mod, local);
        foreach (s; ifStmt.body)
            resolveStatement(s, thenScope, allTables, currentMethod);

        if (ifStmt.elseBranch !is null) {
            auto elseScope = new SymbolTable(local.mod, local);
            resolveStatement(ifStmt.elseBranch, elseScope, allTables, currentMethod);
        }
        return;
    }

    if (auto blockStmt = cast(BlockStatement) stmt) {
        auto blockScope = new SymbolTable(local.mod, local);
        foreach (s; blockStmt.statements)
            resolveStatement(s, blockScope, allTables, currentMethod);
        return;
    }

    if (auto switchStmt = cast(SwitchStatement) stmt) {
        resolveExpression(switchStmt.condition, local, allTables, currentMethod);

        foreach (c; switchStmt.cases) {
            resolveExpression(c.value, local, allTables, currentMethod);
            auto caseScope = new SymbolTable(local.mod, local);
            foreach (s; c.body)
                resolveStatement(s, caseScope, allTables, currentMethod);
        }

        if (switchStmt.defaultClause !is null) {
            auto defScope = new SymbolTable(local.mod, local);
            foreach (s; switchStmt.defaultClause.body)
                resolveStatement(s, defScope, allTables, currentMethod);
        }
        return;
    }

    if (auto guardStmt = cast(GuardStatement) stmt) {
        resolveExpression(guardStmt.condition, local, allTables, currentMethod);
        if (guardStmt.elseExpression !is null)
            resolveExpression(guardStmt.elseExpression, local, allTables, currentMethod);
        return;
    }

    if (auto fnDeclStmt = cast(FunctionDeclStatement) stmt) {
        analyzeFunction(fnDeclStmt.fn, local, allTables);
        return;
    }

    if (auto whileStmt = cast(WhileStatement) stmt) {
        resolveExpression(whileStmt.condition, local, allTables, currentMethod);
        auto loopScope = new SymbolTable(local.mod, local);
        resolveStatement(whileStmt.body, loopScope, allTables, currentMethod);
        return;
    }

    if (auto doWhileStmt = cast(DoWhileStatement) stmt) {
        auto loopScope = new SymbolTable(local.mod, local);
        resolveStatement(doWhileStmt.body, loopScope, allTables, currentMethod);
        resolveExpression(doWhileStmt.condition, local, allTables, currentMethod);
        return;
    }

    if (auto forStmt = cast(ForStatement) stmt) {
        auto forScope = new SymbolTable(local.mod, local);
        if (forStmt.initializer !is null)
            resolveStatement(new VariableStatement(forStmt.initializer.token, forStmt.initializer), forScope, allTables, currentMethod);
        if (forStmt.condition !is null)
            resolveExpression(forStmt.condition, forScope, allTables, currentMethod);
        if (forStmt.update !is null)
            resolveStatement(forStmt.update, forScope, allTables, currentMethod);
        resolveStatement(forStmt.body, forScope, allTables, currentMethod);
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
        resolveExpression(foreachStmt.iterableOrStart, foreachScope, allTables, currentMethod);

        // If it’s a range-based loop, resolve end expression
        if (foreachStmt.endRange !is null)
            resolveExpression(foreachStmt.endRange, foreachScope, allTables, currentMethod);

        // Resolve body in loop scope
        resolveStatement(foreachStmt.body, foreachScope, allTables, currentMethod);
        return;
    }

    if (auto varStmt = cast(VariableStatement) stmt) {
        analyzeVariable(false, varStmt.variable, local, allTables, currentMethod);

        local.addSymbol(new VariableSymbol(varStmt.variable, local.mod));
        return;
    }

    if (auto assertStmt = cast(AssertStatement) stmt) {
        resolveExpression(assertStmt.condition, local, allTables, currentMethod);
        if (assertStmt.message !is null)
            resolveExpression(assertStmt.message, local, allTables, currentMethod);
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

StructSymbol findEnclosingStructFromMethod(SymbolTable sc, FunctionDecl currentMethod) {
    while (sc !is null) {
        foreach (sym; sc.symbols) {
            auto structSym = cast(StructSymbol) sym;
            if (structSym !is null) {
                foreach (member; structSym.decl.members) {
                    if (member.fnDecl is currentMethod) {
                        return structSym;
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

bool isMemberImplemented(
    string name,
    TypeReference expectedType,
    Symbol[string] structMembers,
    StructDecl baseStruct,
    SymbolTable local,
    SymbolTable[string] allModules,
    bool isProperty
) {
    Symbol sym = name in structMembers ? structMembers[name] : null;

    // Try resolving from base struct
    if (sym is null && baseStruct !is null) {
        foreach (baseMember; baseStruct.members) {
            if (baseMember.name == name) {
                if (isProperty && baseMember.propStatement !is null)
                    return areTypesEqual(expectedType, baseMember.propStatement.type);
                if (!isProperty && baseMember.variable !is null)
                    return areTypesEqual(expectedType, baseMember.variable.type);
            }
        }
        return false;
    }

    if (isProperty) {
        auto prop = cast(PropertySymbol) sym;
        return prop !is null && areTypesEqual(expectedType, prop.decl.type);
    } else {
        auto var = cast(VariableSymbol) sym;
        return var !is null && areTypesEqual(expectedType, var.decl.type);
    }
}

bool isFunctionImplemented(
    FunctionDecl expectedFn,
    Symbol[string] structMembers,
    StructDecl baseStruct,
    SymbolTable local,
    SymbolTable[string] allModules
) {
    Symbol sym = expectedFn.name in structMembers ? structMembers[expectedFn.name] : null;

    // Try resolving from base struct
    if (sym is null && baseStruct !is null) {
        foreach (baseMember; baseStruct.members) {
            if (baseMember.fnDecl !is null && baseMember.name == expectedFn.name) {
                return areFunctionSignaturesEqual(baseMember.fnDecl, expectedFn);
            }
        }
        return false;
    }

    auto fn = cast(FunctionSymbol) sym;
    return fn !is null && areFunctionSignaturesEqual(fn.decl, expectedFn);
}

bool areTypesEqual(TypeReference a, TypeReference b) {
    return a.baseName == b.baseName; // Can expand to more complex matching if needed
}

InterfaceDecl[] collectAllBaseInterfaces(InterfaceDecl iface, SymbolTable local, SymbolTable[string] allModules) {
    InterfaceDecl[] result;

    foreach (baseRef; iface.baseInterfaces) {
        if (!validateTypeReference(baseRef, local, allModules))
            continue;

        auto baseSym = resolveTypeReference(baseRef, local, allModules);
        auto unwrapped = unwrapAlias(baseSym, local, allModules);
        auto baseIface = cast(InterfaceSymbol) unwrapped;
        if (baseIface !is null) {
            result ~= baseIface.decl;
            result ~= collectAllBaseInterfaces(baseIface.decl, local, allModules); // Recursive
        }
    }

    return result;
}

bool areFunctionSignaturesEqual(FunctionDecl a, FunctionDecl b) {
    if (a.params.length != b.params.length || a.returnTypes.length != b.returnTypes.length)
        return false;

    foreach (i, param; a.params) {
        if (param.type.baseName != b.params[i].type.baseName)
            return false;
    }

    foreach (i, ret; a.returnTypes) {
        if (ret.baseName != b.returnTypes[i].baseName)
            return false;
    }

    return true;
}

Symbol resolveSizeof(Symbol typeSym, SymbolTable local, SymbolTable[string] allTables) {
    // Unwrap any aliases (e.g., type aliases)
    typeSym = unwrapAlias(typeSym, local, allTables);

    // Handle primitive types
    if (auto prim = cast(BuiltinSymbol) typeSym) {
        size_t size = getPrimitiveSize(prim.name);
        return makeConstSizeSymbol(size, prim.mod);
    }

    // Handle structs
    if (auto strct = cast(StructSymbol) typeSym) {
        size_t size = computeStructSize(strct);
        return makeConstSizeSymbol(size, strct.mod);
    }

    // Optionally handle enums (usually same as underlying type, e.g., int)
    if (auto enm = cast(EnumSymbol) typeSym) {
        if (enm.decl.backingType is null)
            throw new CompilerException("Enum does not declare an underlying type", enm.token);

        auto backingSym = resolveTypeReference(enm.decl.backingType, local, allTables);
        if (backingSym is null)
            throw new CompilerException("Failed to resolve enum backing type", enm.token);

        size_t size = getPrimitiveSize(backingSym.name); // use name of the resolved type
        return makeConstSizeSymbol(size, enm.mod);
    }

    throw new CompilerException("sizeof cannot be used on non-type or unsupported type", typeSym.token);
}

size_t getPrimitiveSize(string name) {
    auto settings = getSettings();

    switch (name) {
        case Keywords.Bool:   return 1;
        case Keywords.Char:   return 1;

        case Keywords.Int8: return 1;
        case Keywords.Int16: return 2;
        case Keywords.Int32: return 4;
        case Keywords.Int64: return 8;

        case Keywords.UInt8: return 1;
        case Keywords.UInt16: return 2;
        case Keywords.UInt32: return 4;
        case Keywords.UInt64: return 8;

        case Keywords.Float:  return 4;
        case Keywords.Double: return 8;
        case Keywords.Real: return 16;

        case Keywords.Ptr:    return settings.is64Bit ? 8 : 4;
        case Keywords.Size_T: return settings.is64Bit ? 8 : 4;
        case Keywords.Ptrdiff_T: return settings.is64Bit ? 8 : 4;
        default:
            throw new CompilerException("Unknown primitive type: " ~ name, UnknownToken);
    }
}

Symbol makeConstSizeSymbol(size_t size, Module mod) {
    auto val = new LiteralExpr(size.to!string, Token.dummy(TokenType.NumberLiteral));
    auto decl = new VariableDecl(
        DefaultAccessModifier, [], Token.dummy(TokenType.Identifier), VarKind.Const, "sizeof_result", null, val
    );
    return new VariableSymbol(decl, mod);
}

size_t computeStructSize(StructSymbol sym) {
    size_t total = 0;
    foreach (member; sym.decl.members) {
        if (auto var = cast(VariableDecl) member) {
            auto memberTypeSym = resolveTypeReference(var.type, null, null); // provide local + allTables if needed
            total += getPrimitiveSize(memberTypeSym.name); // or recursive if needed
        }
    }
    return total;
}

bool isVoidFunction(FunctionDecl fn) {
    return fn.returnTypes.length == 0 || (fn.returnTypes.length == 1 && fn.returnTypes[0].baseName == Keywords.Void);
}

bool doesStatementAlwaysReturn(Statement stmt) {
    import mind.statements;

    if (auto blockStmt = cast(BlockStatement) stmt) {
        // For a block to always return, the last statement in the block must always return
        if (blockStmt.statements.length == 0)
            return false;
        return doesStatementAlwaysReturn(blockStmt.statements[$-1]);
    }

    if (auto ifStmt = cast(IfStatement) stmt) {
        // If no else branch, it may not always return
        if (ifStmt.elseBranch is null)
            return false;

        // Both if and else branch must always return
        bool thenReturns = false;
        foreach (s; ifStmt.body) {
            thenReturns = doesStatementAlwaysReturn(s);
            if (thenReturns) break;
        }
        bool elseReturns = doesStatementAlwaysReturn(ifStmt.elseBranch);
        return thenReturns && elseReturns;
    }

    if (auto retStmt = cast(ReturnStatement) stmt) {
        // Return statement with expression counts as always returns a value
        return retStmt.returnExpression !is null;
    }

    if (auto switchStmt = cast(SwitchStatement) stmt) {
        // All cases and default must always return
        foreach (c; switchStmt.cases) {
            if (!doesStatementsAlwaysReturn(c.body))
                return false;
        }
        if (switchStmt.defaultClause is null)
            return false;
        if (!doesStatementsAlwaysReturn(switchStmt.defaultClause.body))
            return false;
        return true;
    }

    if (auto whileStmt = cast(WhileStatement) stmt) {
        // Conservative: assume while loop might not always return
        return false;
    }

    if (auto doWhileStmt = cast(DoWhileStatement) stmt) {
        // Conservative: assume do-while might not always return
        return false;
    }

    if (auto forStmt = cast(ForStatement) stmt) {
        // Conservative: assume for loop might not always return
        return false;
    }

    if (auto foreachStmt = cast(ForeachStatement) stmt) {
        // Conservative: assume foreach loop might not always return
        return false;
    }

    if (auto exprStmt = cast(ExprStatement) stmt) {
        return false; // Expressions don’t cause returns
    }

    if (auto lrStmt = cast(LRStatement) stmt) {
        return false; // Assignment statements don’t cause returns
    }

    if (auto breakStmt = cast(BreakStatement) stmt) {
        return false; // break does not imply function return
    }

    if (auto continueStmt = cast(ContinueStatement) stmt) {
        return false; // continue does not imply function return
    }

    if (auto guardStmt = cast(GuardStatement) stmt) {
        // guard does not guarantee return
        return false;
    }

    if (auto fnDeclStmt = cast(FunctionDeclStatement) stmt) {
        // function declaration inside another function: no direct return here
        return false;
    }

    if (auto varStmt = cast(VariableStatement) stmt) {
        return false; // Variable declarations don’t cause returns
    }

    if (auto assertStmt = cast(AssertStatement) stmt) {
        return false; // Assertions don’t cause returns
    }

    // Default conservative fallback
    return false;
}

bool doesStatementsAlwaysReturn(Statement[] stmts) {
    if (stmts.length == 0)
        return false;
    // Only the last statement in the block matters for return checking
    return doesStatementAlwaysReturn(stmts[$-1]);
}

// Infer the type of an expression, returning TypeReference or throws on error
TypeReference inferExpressionType(Expr expr, SymbolTable local, SymbolTable[string] allModules) {
    import std.exception : enforce;

    Symbol sym;
    VariableSymbol varSym;
    AliasSymbol aliasSym;
    FunctionSymbol fnSym;
    PropertySymbol propSym;
    StructSymbol structSym;
    EnumSymbol enumSym;
    EnumValueSymbol enumValSym;
    InterfaceSymbol interfaceSym;

    TypeReference getSymbolType(Symbol sym, Token token) {
        varSym = cast(VariableSymbol) sym;
        if (varSym !is null) {
            return varSym.decl.type;
        }

        aliasSym = cast(AliasSymbol) sym;
        if (aliasSym !is null) {
            enforceCompilerException(aliasSym.resolvedTarget !is null, 
                "Alias symbol's resolved target is null", token);
            return getSymbolType(aliasSym.resolvedTarget, token);
        }

        fnSym = cast(FunctionSymbol) sym;
        if (fnSym !is null) {
            enforceCompilerException(fnSym.decl.returnTypes && fnSym.decl.returnTypes.length,
                "Function does not return a value.", token);
            return fnSym.decl.returnTypes[0];
        }

        propSym = cast(PropertySymbol) sym;
        if (propSym !is null) {
            return propSym.decl.type;
        }

        structSym = cast(StructSymbol) sym;
        if (structSym !is null) {
            return new TypeReference(structSym.name);
        }

        enumSym = cast(EnumSymbol) sym;
        if (enumSym !is null) {
            return new TypeReference(enumSym.name);
        }

        enumValSym = cast(EnumValueSymbol) sym;
        if (enumValSym !is null) {
            return new TypeReference(enumValSym.parentEnum.name);
        }

        interfaceSym = cast(InterfaceSymbol) sym;
        if (interfaceSym !is null) {
            return new TypeReference(interfaceSym.name);
        }

        throw new CompilerException("Cannot infer type from symbol kind: " ~ sym.kind.to!string, token);
    }

    // Try each expression type with casts:
    auto idExpr = cast(IdentifierExpr) expr;
    if (idExpr !is null) {
        sym = local.getSymbol(idExpr.name);
        if (sym is null) {
            throw new CompilerException("Undefined symbol: " ~ idExpr.name, idExpr.token);
        }
        return getSymbolType(sym, idExpr.token);
    }

    auto litExpr = cast(LiteralExpr) expr;
    if (litExpr !is null) {
        // Infer literal type (expand if needed)
        if (litExpr.value.length > 0 && litExpr.value[0] == '"') {
            return new TypeReference(Keywords.String);
        }
        if (litExpr.value.length > 0 && litExpr.value[0] == '\'') {
            return new TypeReference(Keywords.Char);
        }
        else if (litExpr.value == "true" || litExpr.value == "false") {
            return new TypeReference(Keywords.Bool);
        }
        else {
            return new TypeReference(Keywords.Int32);
        }
    }

    auto binExpr = cast(BinaryExpr) expr;
    if (binExpr !is null) {
        auto leftType = inferExpressionType(binExpr.left, local, allModules);
        auto rightType = inferExpressionType(binExpr.right, local, allModules);

        if (leftType.baseName != rightType.baseName) {
            throw new CompilerException("Type mismatch in binary expression: " ~ 
                leftType.baseName ~ " vs " ~ rightType.baseName, binExpr.token);
        }
        return leftType;
    }

    auto unExpr = cast(UnaryExpr) expr;
    if (unExpr !is null) {
        auto operandType = inferExpressionType(unExpr.operand, local, allModules);
        if (unExpr.op == "!") {
            return new TypeReference(Keywords.Bool);
        }
        return operandType;
    }

    auto callExpr = cast(CallExpr) expr;
    if (callExpr !is null) {
        auto calleeId = cast(IdentifierExpr) callExpr.callee;
        if (calleeId is null) {
            throw new CompilerException("Unsupported callee expression in function call", callExpr.token);
        }

        sym = local.getSymbol(calleeId.name);
        if (sym is null) {
            throw new CompilerException("Undefined function: " ~ calleeId.name, callExpr.token);
        }

        fnSym = cast(FunctionSymbol) sym;
        if (fnSym is null) {
            throw new CompilerException("Symbol is not a function: " ~ calleeId.name, callExpr.token);
        }

        enforceCompilerException(fnSym.decl.returnTypes && fnSym.decl.returnTypes.length,
            "Function does not return any values.", callExpr.token);

        return fnSym.decl.returnTypes[0];
    }

    auto grpExpr = cast(GroupingExpr) expr;
    if (grpExpr !is null) {
        return inferExpressionType(grpExpr.expression, local, allModules);
    }

    auto castExpr = cast(CastExpr) expr;
    if (castExpr !is null) {
        return exprToTypeReference(castExpr.targetType);
    }

    auto arrayIdxExpr = cast(ArrayIndexExpr) expr;
    if (arrayIdxExpr !is null) {
        auto arrayType = inferExpressionType(arrayIdxExpr.arrayExpr, local, allModules);
        if (arrayType.arrayElementType !is null) {
            return arrayType.arrayElementType;
        }
        throw new CompilerException("Cannot index non-array type: " ~ arrayType.baseName, arrayIdxExpr.token);
    }

    else if (auto q = cast(QualifiedAccessExpr) expr) {
        auto targetType = inferExpressionType(q.target, local, allModules);
        if (targetType is null) {
            throw new CompilerException("Cannot resolve type of target in qualified access.", q.token);
        }

        // Resolve struct or interface type symbol
        auto mod = local.mod;
        Symbol qsym = resolveTypeReference(targetType, local, allModules);
        if (qsym is null) {
            throw new CompilerException("Cannot resolve type: " ~ targetType.baseName, q.token);
        }

        // Handle struct access
        if (auto qstructSym = cast(StructSymbol) qsym) {
            auto member = qstructSym.symbols.getSymbol(q.member.name);
            if (member is null)
                throw new CompilerException("Unknown member: " ~ q.member.name, q.member.token);

            // Infer the type based on the member kind
            if (auto vs = cast(VariableSymbol) member) {
                return vs.decl.type;
            } else if (auto ps = cast(PropertySymbol) member) {
                return ps.decl.type;
            } else if (auto fs = cast(FunctionSymbol) member) {
                enforceCompilerException(fs.decl.returnTypes && fs.decl.returnTypes.length,
                    "Function does not return any values.", callExpr.token);

                return fs.decl.returnTypes[0];
            } else {
                throw new CompilerException("Unsupported member type in qualified access.", q.member.token);
            }
        }
        else if (auto qenumSym = cast(EnumSymbol) qsym) {
            foreach (ev; qenumSym.decl.values) {
                if (ev.name == q.member.name) {
                    return new TypeReference(qenumSym.name);
                }
            }
            throw new CompilerException("Enum " ~ qenumSym.name ~ " has no member named '" ~ q.member.name ~ "'", q.member.token);
        }

        // TODO: Optionally support InterfaceSymbol if needed
        throw new CompilerException("Qualified access not supported on type: " ~ targetType.baseName, q.token);
    }

    // Handle other Expr types here...

    throw new CompilerException("Type inference not implemented for expression type: " ~ expr.classinfo.toString, expr.token);
}

// Helper to infer TypeReference from an Expr representing a type (IdentifierExpr, TemplatedExpr, TypeExpr)
TypeReference inferTypeExprFromExpr(Expr expr, SymbolTable symtab) {
    if (auto id = cast(IdentifierExpr) expr) {
        auto sym = symtab.getSymbol(id.name);
        if (sym is null)
            throw new CompilerException("Unknown type: "~id.name, expr.token);

        // Return TypeReference for this type symbol
        return new TypeReference(id.name);
    }
    else if (auto templ = cast(TemplatedExpr) expr) {
        auto baseType = inferTypeExprFromExpr(templ.target, symtab);
        foreach (arg; templ.templateArgs) {
            baseType.typeArguments ~= inferTypeExprFromExpr(arg, symtab);
        }
        return baseType;
    }
    else if (auto typeExpr = cast(TypeExpr) expr) {
        return inferTypeExprFromExpr(typeExpr.innerType, symtab);
    }
    else {
        throw new CompilerException("Cannot infer type expression from " ~ typeof(expr).stringof, expr.token);
    }
}

// Simple numeric type check helper
bool isNumericType(TypeReference t) {
    return
        t.baseName == Keywords.Int8 ||
        t.baseName == Keywords.Int16 ||
        t.baseName == Keywords.Int32 ||
        t.baseName == Keywords.Int64 ||

        t.baseName == Keywords.UInt8 ||
        t.baseName == Keywords.UInt16 ||
        t.baseName == Keywords.UInt32 ||
        t.baseName == Keywords.UInt64 ||

        t.baseName == Keywords.Float ||
        t.baseName == Keywords.Double ||
        t.baseName == Keywords.Real ||

        t.baseName == Keywords.Size_T ||
        t.baseName == Keywords.Ptrdiff_T;
}

// Check if type `from` can be assigned to type `to` (basic version)
bool typesAreAssignable(TypeReference from, TypeReference to) {
    if (from.baseName == to.baseName)
        return true;

    // e.g. int to float allowed implicitly
    if (from.baseName == Keywords.Int32 && to.baseName == Keywords.Float)
        return true;

    // Add your language's assignability rules here
    return false;
}

// Placeholder: implement according to your type system
bool canCast(TypeReference from, TypeReference to) {
    // For now: allow if assignable or exact match
    return typesAreAssignable(from, to) || from.baseName == to.baseName;
}

// Placeholder: lookup member symbol from a type (Struct, Class, etc.)
Symbol lookupMemberSymbol(TypeReference type, string memberName) {
    // TODO: You must implement member lookup by querying the struct, class, or interface declarations

    // For now return null to indicate not found
    return null;
}

// Placeholder: check constructors matching args
bool checkConstructorArgs(TypeReference type, Expr[] args, SymbolTable symtab) {
    // TODO: lookup constructors of type, check if any matches args by count and type

    return true; // assume true for now
}

bool areTypesCompatible(TypeReference expected, TypeReference actual) {
    if (expected is null || actual is null)
        return false;

    // Exact match
    if (expected.baseName != actual.baseName)
        return false;

    // Check template type arguments
    if (expected.typeArguments.length != actual.typeArguments.length)
        return false;

    foreach (i, expectedArg; expected.typeArguments) {
        if (!areTypesCompatible(expectedArg, actual.typeArguments[i]))
            return false;
    }

    // Check qualifiers (optional, depending on your design)
    if (expected.qualifiers != actual.qualifiers)
        return false;

    // Array types
    if ((expected.arrayElementType !is null) || (actual.arrayElementType !is null)) {
        if (expected.arrayElementType is null || actual.arrayElementType is null)
            return false;

        if (!areTypesCompatible(expected.arrayElementType, actual.arrayElementType))
            return false;

        // Optional: handle size checking
        if ((expected.arraySizeExpr is null) != (actual.arraySizeExpr is null))
            return false;
    }

    // Map types
    if ((expected.keyType !is null) || (actual.keyType !is null)) {
        if (expected.keyType is null || actual.keyType is null)
            return false;

        if (!areTypesCompatible(expected.keyType, actual.keyType))
            return false;
    }

    return true;
}

bool isTemplateType(TypeReference type, TemplateDecl templateDecl) {
    foreach (param; templateDecl.templateParams) {
        if (param == type.baseName) {
            return true;
        }
    }
    return false;
}

Symbol resolveTraitSymbol(string name, SymbolTable local, SymbolTable[string] allModules, Module fromModule) {
    // First: try local scope (always accessible)
    auto sym = local.getSymbol(name);
    if (sym !is null && sym.kind == SymbolKind.Trait)
        return sym;

    // Then: search across all modules
    foreach (moduleTable; allModules.byValue) {
        auto s = moduleTable.getSymbol(name);
        if (s !is null && s.kind == SymbolKind.Trait) {
            if (isAccessible(s.access, fromModule, s.mod)) {
                return s;
            }
        }
    }

    return null; // Not found or not accessible
}