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
    output.header ~= format("extern %s %s;\r\n", symbol.decl.type.baseName, symbol.name);
    if (symbol.decl.initializer) {
        auto expr = flattenExpression(symbol.decl.initializer);
        if (!expr) {
            throw new CompilerException("Could not flatten initializer for variable.", symbol.decl.token);
        }
        output.source ~= format("%s %s = %s;\r\n", symbol.decl.type.baseName, symbol.name, expr);
    } else {
        output.source ~= format("%s %s = %s;\r\n", symbol.decl.type.baseName, symbol.name, getDefaultValueFromType(symbol.decl.type.baseName, symbol.decl.token));
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