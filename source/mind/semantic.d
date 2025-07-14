module mind.semantic;

import std.string : indexOf;
import std.algorithm : canFind;

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
        builtinSymbols[typeName] = new BuiltinSymbol(typeName);
    }
}

SymbolTable[string] allTables;

SymbolTable[string] createTables(Module[string] modules) {
    foreach (k, mod; modules) {
        auto table = buildSymbolTable(mod);
        allTables[mod.name] = table;
    }

    foreach (k, mod; modules) {
        auto table = allTables[mod.name];

        // Validate aliases after building the table
        foreach (a; mod.aliases) {
            if (!validateTypeReference(a.type, table, allTables)) {
                throw new CompilerException("Unresolved type in alias: " ~ a.name, a.token);
            }
        }
    }

    return allTables;
}

SymbolTable buildSymbolTable(Module mod) {
    auto table = new SymbolTable();

    foreach (imp; mod.imports)
        table.addImport(imp);

    foreach (v; mod.variables)
        table.addSymbol(new VariableSymbol(v));

    foreach (f; mod.functions)
        table.addSymbol(new FunctionSymbol(f));

    foreach (s; mod.structs)
        table.addSymbol(new StructSymbol(s));

    foreach (e; mod.enums)
        table.addSymbol(new EnumSymbol(e));

    foreach (p; mod.properties)
        table.addSymbol(new PropertySymbol(p));

    foreach (i; mod.interfaces)
        table.addSymbol(new InterfaceSymbol(i));

    foreach (t; mod.templates)
        table.addSymbol(new TemplateSymbol(t));

    foreach (a; mod.aliases)
        table.addSymbol(new AliasSymbol(a));

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
        auto remainder = base[dotIndex + 1 .. $]; // e.g. 'Foo' or 'Foo.Bar'

        // Check if prefix is an imported alias or module name
        auto impInfo = local.getImport(prefix);
        if (impInfo !is null) {
            // If explicit members imported, verify remainder's first part is allowed
            if (impInfo.members.length > 0) {
                // Get the first segment of remainder before any dot
                auto firstDot = indexOf(remainder, ".");
                string firstMember = (firstDot == -1) ? remainder : remainder[0 .. firstDot];

                if (!impInfo.members.canFind(firstMember)) {
                    // The member is not imported explicitly → fail resolution
                    return null;
                }
            }

            // Lookup remainder in the imported module's symbol table
            if (impInfo.moduleName in allModules) {
                auto modTable = allTables[impInfo.moduleName];

                // Recurse with remainder as baseName to handle nested qualified names
                auto subTypeRef = new TypeReference();
                subTypeRef.baseName = remainder;

                auto resolved = resolveTypeReference(subTypeRef, modTable, allModules);
                if (resolved !is null)
                    return resolved;
            }

            return null;
        }

        // Prefix not found → no symbol
        return null;
    }

    // 3. Unqualified name resolution

    // a) Check local symbols directly
    if (auto sym = local.getSymbol(base))
        return sym;

    // b) Check explicitly imported members (import foo.bar : baz, qux;)
    foreach (aliasName, impInfo; local.imports) {
        // Only consider explicit members imports (impInfo.members not empty)
        if (impInfo.members.length > 0) {
            // If the requested name is in members list, lookup in that module
            if (impInfo.members.canFind(base)) {
                if (impInfo.moduleName in allModules) {
                    auto modTable = allTables[impInfo.moduleName];
                    if (auto sym = modTable.getSymbol(base))
                        return sym;
                }
            }
        }
    }

    // c) Check unaliased imports (import whole module without alias)
    foreach (aliasName, impInfo; local.imports) {
        if (impInfo.members.length == 0) {
            if (impInfo.moduleName in allModules) {
                auto modTable = allTables[impInfo.moduleName];
                if (auto sym = modTable.getSymbol(base))
                    return sym;
            }
        }
    }

    // Not found anywhere
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
                if (memberName !in moduleTable.symbols) {
                    throw new CompilerException(
                        "Imported member '" ~ memberName ~ "' does not exist in module '" ~ moduleName ~ "'.",
                        imp.token
                    );
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