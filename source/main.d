module main;

import std.stdio : writefln,writeln,readln,stderr;
import std.file : readText;
import std.path : baseName;
import std.array : split;
import std.algorithm : any;

import mind;

void main(string[] args) {
  bool isVerbose = true;
  try {
    auto settings = handleArgs(args);
    isVerbose = settings.isVerbose;

    Module[string] modules;

    foreach (file; settings.sourceFiles) {
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

    validateAllImportsAndMembers(modules, allTables);

    resolveAliases(symbolTables);
  }
  catch (Exception e) {
    if (isVerbose) stderr.writeln(e);
    else stderr.writeln(e.message);
  }

  readln();
}
