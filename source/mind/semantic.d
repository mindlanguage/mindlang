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
        builtinSymbols[typeName] = new BuiltinSymbol(typeName, new Module(UnknownToken, "__builtin"));
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