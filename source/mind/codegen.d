module mind.codegen;

import std.array : appender, Appender;
import std.string : format, startsWith;

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

struct CodeOutput {
    Appender!string header;
    Appender!string source;
}

void prepareCodeOutput(ref CodeOutput output) {
    output.header = appender!string;
    output.source = appender!string;
}

void generateModules(SymbolTable[string] allModules, ref CodeOutput output) {
    output.header ~= "#ifndef PROGRAM_H\r\n";
    output.header ~= "#define PROGRAM_H\r\n";

    output.source ~= "#include \"program.h\"\r\n";

    // Output runtime first
    foreach (modName, modTable; allModules) {
        if (modName == "runtime") {
            generateModule(modTable, output);
        }
    }

    // Output stdlib second
    foreach (modName, modTable; allModules) {
        if (modName.startsWith("std")) {
            generateModule(modTable, output);
        }
    }

    // Output other modules last
    foreach (modName, modTable; allModules) {
        if (modName != "runtime" && !modName.startsWith("std")) {
            generateModule(modTable, output);
        }
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

    // Do structs

    // Do functions
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

        default:
            throw new CompilerException("Could not determine primitive type.", token);
    }
}