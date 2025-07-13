module mind.args;

import std.stdio;
import std.string;
import std.uni;
import std.conv;
import std.array;
import std.exception;

import mind.settings;

struct Command {
    string longName;
    string shortName;
    string description;
    bool expectsValue;
    bool isFlag;
    string value; // Will store "true" if flag set without explicit value
}

void printHelp(Command[] commands) {
    writeln("Available commands:");
    foreach (cmd; commands) {
        string flagForm = "--" ~ cmd.longName;
        if (!cmd.shortName.empty)
            flagForm ~= ", -" ~ cmd.shortName;
        if (cmd.expectsValue)
            flagForm ~= " <value>";
        writeln("  ", flagForm, "\t", cmd.description);
    }
}

string[] splitOnce(string s, char delim = '=') {
    auto idx = s.indexOf(delim);
    if (idx == -1) return [s];
    else return [s[0 .. idx], s[idx + 1 .. $]];
}

Command[string] parseArgs(string[] args, Command[] commands) {
    Command[string] results;

    // Initialize results with defaults
    foreach (cmd; commands) {
        results[cmd.longName] = cmd;
        results[cmd.longName].value = cmd.isFlag ? "false" : "";
    }

    size_t i = 0;
    while (i < args.length) {
        auto arg = args[i];

        if (arg == "--help" || arg == "-h") {
            printHelp(commands);
            break;
        }

        if (arg.startsWith("--")) {
            // Long option, possibly with =value
            auto parts = splitOnce(arg[2 .. $], '=');
            auto name = parts[0];
            string val = parts.length > 1 ? parts[1] : "";

            enforce(name in results, "Unknown option: " ~ arg);

            auto cmd = results[name];
            if (cmd.expectsValue) {
                if (val.empty) {
                    enforce(i + 1 < args.length, "Option '--" ~ name ~ "' expects a value.");
                    val = args[++i];
                }
                results[name].value = stripQuotes(val);
            } else {
                // Flag option: presence means true or explicit value
                results[name].value = val.empty ? "true" : stripQuotes(val);
            }
        }
        else if (arg.startsWith("-") && arg.length > 1) {
            // Short option, possibly with =value
            string shortName;
            string val;

            auto eqIndex = arg.indexOf('=');
            if (eqIndex != -1) {
                // -s=value form
                shortName = arg[1 .. eqIndex];
                val = arg[eqIndex + 1 .. $];
            } else {
                // -s form
                shortName = arg[1 .. $];
                val = "";
            }

            Command matchedCmd;
            bool found = false;
            foreach (cmd; commands) {
                if (cmd.shortName == shortName) {
                    matchedCmd = cmd;
                    found = true;
                    break;
                }
            }

            enforce(found, "Unknown option: " ~ arg);

            if (matchedCmd.expectsValue) {
                if (val.empty) {
                    enforce(i + 1 < args.length, "Option '-" ~ shortName ~ "' expects a value.");
                    val = args[++i];
                }
                results[matchedCmd.longName].value = stripQuotes(val);
            }
            else {
                results[matchedCmd.longName].value = val.empty ? "true" : stripQuotes(val);
            }
        }
        else {
            enforce(false, "Unexpected argument: " ~ arg);
        }

        i++;
    }

    return results;
}

string stripQuotes(string s) {
    if (s.length >= 2 && ((s[0] == '"' && s[$-1] == '"') || (s[0] == '\'' && s[$-1] == '\''))) {
        return s[1 .. $-1];
    }
    return s;
}

string[] splitRespectingQuotes(string input) {
    string[] result;
    size_t i = 0;
    size_t len = input.length;
    
    while (i < len) {
        // Skip whitespace
        while (i < len && input[i].isWhite) i++;
        if (i >= len) break;
        
        char quote = 0;
        if (input[i] == '"' || input[i] == '\'') {
            quote = input[i];
            i++;
            size_t start = i;
            while (i < len && input[i] != quote) i++;
            if (i >= len) {
                throw new Exception("Unclosed quote in input string");
            }
            result ~= input[start .. i];
            i++; // skip closing quote
        } else {
            size_t start = i;
            while (i < len && !input[i].isWhite) i++;
            result ~= input[start .. i];
        }
    }
    return result;
}

Settings handleArgs(string[] args)
{
    auto settings = new Settings;

    // Remove program name from args
    auto actualArgs = args.length > 1 ? args[1 .. $] : [];
    
    Command[] commands = [
        Command("source", "s", "Sets the source files", true, false, ""),
        Command("verbose", "v", "Makes compiler output messages verbose.", false, true, "")
    ];
    
    auto parsed = parseArgs(actualArgs, commands);

    foreach (cmd; parsed) {
        bool hasFlag = parsed[cmd.longName].value == "true";
        switch (cmd.longName)
        {
            case "source":
                settings.sourceFiles = splitRespectingQuotes(parsed[cmd.longName].value);
                break;

            case "verbose":
                settings.isVerbose = hasFlag;
                break;

            default: break;
        }
    }
    
    return settings;
}