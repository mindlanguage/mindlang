module main;

import std.stdio : writefln,writeln,readln,stderr;
import std.file : readText, exists, dirEntries, SpanMode, write, remove;
import std.path : baseName;
import std.array : split;
import std.algorithm : any;

import mind;

void main(string[] args) {
  bool isVerbose = true;
  try {
    auto settings = handleArgs(args);
    isVerbose = settings.isVerbose;

    setSettings(settings);

    initializeKeywordMap();

    Module[string] modules;

    string[] sourceFiles = [];

    if (exists("runtime.mind")) {
      sourceFiles ~= "runtime.mind";
    }

    foreach (string name; dirEntries("stdlib", SpanMode.depth))
    {
      sourceFiles ~= name;
    }

    sourceFiles ~= settings.sourceFiles;

    foreach (file; sourceFiles) {
      auto source = readText(file);
      auto tokens = tokenize(source, true, file);

      if (settings.isVerbose) {
        foreach (token; tokens) {
            writefln("[%s] '%s' %s,%s", token.type, token.lexeme, token.line, token.column);
        }
      }

      auto parser = Parser(tokens);
      auto firstToken = parser.peek();
      auto mod = parseModule(baseName(file).split(".")[0], parser);

      if (mod.name in modules) {
        throw new CompilerException("Duplicate module '" ~ mod.name ~ "'.", firstToken);
      }

      modules[mod.name] = mod;
    }

    if ("runtime" in modules) {
      foreach (k, mod; modules) {
          if (mod.name != "runtime") {
              bool hasRuntime = mod.imports.any!(imp => imp.moduleName == "runtime");
              if (!hasRuntime) {
                  mod.imports ~= new ImportStatement(DefaultAccessModifier, UnknownToken, null, "runtime");
              }
          }
      }
    }

    initBuiltinSymbols();
    
    auto symbolTables = createTables(modules);
    analyzeTables(modules, symbolTables);

    auto moduleIndex = calculateModuleSymbolIndex(symbolTables);

    CodeOutput output;
    prepareCodeOutput(output);
    generateModules(moduleIndex, symbolTables, output);

    if (settings.isVerbose) {
      writeln("---- HEADER ----");
      writeln(output.header[]);

      writeln("---- SOURCE ----");
      writeln(output.source[]);
    }

    if (exists("program.h")) remove("program.h");
    if (exists("program.c")) remove("program.c");

    write("program.h", output.header[]);
    write("program.c", output.source[]);
  }
  catch (Exception e) {
    if (isVerbose) stderr.writeln(e);
    else stderr.writeln(e.message);
  }

  readln();
}
