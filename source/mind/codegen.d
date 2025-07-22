module mind.codegen;

import std.array : appender, Appender, array, join;
import std.string : format, startsWith;
import std.algorithm : sort;

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
import mind.semantic;

class ModuleSymbolIndex {
    int index;
    SymbolTable table;

    this(int index, SymbolTable table) {
        this.index = index;
        this.table = table;
    }
}

ModuleSymbolIndex[] calculateModuleSymbolIndex(SymbolTable[string] allModules) {
    ModuleSymbolIndex[string] index;
    index["runtime"] = new ModuleSymbolIndex(1000000, allModules["runtime"]);

    foreach (modName, modTable; allModules) {
        if (modName == "runtime") continue;

        if (modName !in index) {
            index[modName] = new ModuleSymbolIndex(0, modTable);
        }

        foreach (i; modTable.imports) {
            auto importIndex = index.get(i.moduleName, null);

            if (importIndex) {
                importIndex.index++;
            } else {
                index[i.moduleName] = new ModuleSymbolIndex(1, allModules[i.moduleName]);
            }
        }
    }

    return index.values.sort!((a, b) => b.index < a.index).array;
}

struct CodeOutput {
    Appender!string header;
    Appender!string source;
}

void prepareCodeOutput(ref CodeOutput output) {
    output.header = appender!string;
    output.source = appender!string;
}

string getDefaultValueFromType(string primitiveType, Token token) {
    switch (primitiveType) {
        case Keywords.Float:
        case Keywords.Double:
        case Keywords.Real:

        case Keywords.Int8:
        case Keywords.Int16:
        case Keywords.Int32:
        case Keywords.Int64:

        case Keywords.UInt8:
        case Keywords.UInt16:
        case Keywords.UInt32:
        case Keywords.UInt64:

        case Keywords.Size_T:
        case Keywords.Ptrdiff_T:

        case Keywords.Bool:
            return "0";

        default:
            throw new CompilerException("Could not determine primitive type for variable.", token);
    }
}

string convertPrimitiveTypeToCType(TypeReference type, Token token) {
    auto baseType = type.baseName;

    auto settings = getSettings();

    switch (baseType) {
        case Keywords.Char: return "unsigned char";

        case Keywords.Float: return "float";
        case Keywords.Double: return "double";
        case Keywords.Real: return "long double";

        case Keywords.Int8: return "char";
        case Keywords.Int16: return "short";
        case Keywords.Int32: return "int";
        case Keywords.Int64: return "long";

        case Keywords.UInt8: return "unsigned char";
        case Keywords.UInt16: return "unsigned short";
        case Keywords.UInt32: return "unsigned int";
        case Keywords.UInt64: return "unsigned long";

        case Keywords.Size_T: return settings.is64Bit ? "unsigned long" : "unsigned int";
        case Keywords.Ptrdiff_T: return settings.is64Bit ? "long" : "int";

        case Keywords.Bool: return settings.is64Bit ? "long" : "int";

        case Keywords.Ptr:
            if (!type.typeArguments || !type.typeArguments.length) {
                return "void*";
            }

            return convertPrimitiveTypeToCType(type.typeArguments[0], token) ~ "*";

        default: return type.toString;
    }
}

void generateModules(ModuleSymbolIndex[] allModules, ref CodeOutput output) {
    output.header ~= "#ifndef PROGRAM_H\r\n";
    output.header ~= "#define PROGRAM_H\r\n";

    output.source ~= "#include \"program.h\"\r\n";
    
    foreach (modIndex; allModules) {
        generateModule(modIndex.table, output);
    }

    output.header ~= "#endif\r\n";
}

void generateModule(SymbolTable mod, ref CodeOutput output) {
    output.header ~= format("// === module: %s ===\r\n", mod.mod.name);
    output.source ~= format("// === module: %s ===\r\n", mod.mod.name);

    // Do enums

    foreach (variableSymbol; mod.getSymbols!VariableSymbol) {
        generateGlobalVariable(variableSymbol, output);
    }

    // Do properties

    foreach (structSymbol; mod.getSymbols!StructSymbol) {
        generateStruct(structSymbol, output);
    }

    foreach (functionSymbol; mod.getSymbols!FunctionSymbol) {
        generateFunction("", functionSymbol, output);
    }
}

void generateGlobalVariable(VariableSymbol symbol, ref CodeOutput output) {
    auto cType = convertPrimitiveTypeToCType(symbol.decl.type, symbol.decl.token);
    output.header ~= format("extern %s %s;\r\n", cType, symbol.name);
    if (symbol.decl.initializer) {
        auto expr = flattenExpression(symbol.decl.initializer);
        if (!expr) {
            throw new CompilerException("Could not flatten initializer for variable.", symbol.decl.token);
        }
        output.source ~= format("%s %s = %s;\r\n", cType, symbol.name, expr);
    } else {
        output.source ~= format("%s %s = %s;\r\n", cType, symbol.name, getDefaultValueFromType(symbol.decl.type.baseName, symbol.decl.token));
    }
}

void generateVariable(VariableSymbol symbol, ref CodeOutput output, bool isHeader) {
    auto cType = convertPrimitiveTypeToCType(symbol.decl.type, symbol.decl.token);

    if (isHeader) {
        output.header ~= format("%s %s;\r\n", cType, symbol.name);
    } else {
        if (symbol.decl.initializer) {
            auto expr = flattenExpression(symbol.decl.initializer);
            if (!expr) {
                throw new CompilerException("Could not flatten initializer for variable.", symbol.decl.token);
            }

            output.source ~= format("%s %s = %s;\r\n", cType, symbol.name, expr);
        } else {
            output.source ~= format("%s %s = %s;\r\n", cType, symbol.name, getDefaultValueFromType(symbol.decl.type.baseName, symbol.decl.token));
        }
    }
}

void generateFunction(string prefix, FunctionSymbol fn, ref CodeOutput output) {
    if (fn.decl.templateParams && fn.decl.templateParams.length) {
        return; // Templates are discarded atm.
    }

    string[] parameters = [];

    foreach (param; fn.decl.params) {
        auto cType = convertPrimitiveTypeToCType(param.type, param.token);

        parameters ~= format("%s %s", cType, param.name);
    }

    auto returnType = convertPrimitiveTypeToCType(fn.decl.returnTypes && fn.decl.returnTypes.length ? fn.decl.returnTypes[0] : new TypeReference(Keywords.Void), fn.token);

    output.header ~= format("%s %s%s(%s);\r\n", returnType, prefix ? prefix : "", fn.name, parameters.join(","));
    output.source ~= format("%s %s%s(%s) {\r\n", returnType, prefix ? prefix : "", fn.name, parameters.join(","));

    // statements ...

    output.source ~= "}\r\n";
}

void generateStruct(StructSymbol symbol, ref CodeOutput output) {
    if (symbol.decl.genericParams && symbol.decl.genericParams.length) {
        return; // Templates are discarded atm.
    }

    output.header ~= format("typedef struct %s {\r\n", symbol.name);

    auto variables = symbol.symbols.getSymbols!VariableSymbol;

    foreach (variable; variables) {
        generateVariable(variable, output, true);
    }

    output.header ~= format("} %s;\r\n", symbol.name);

    auto functions = symbol.symbols.getSymbols!FunctionSymbol;

    foreach (fn; functions) {
        generateFunction(symbol.name ~ "_", fn, output);
    }
}