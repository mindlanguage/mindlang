module mind.keywords;

import std.traits : EnumMembers;

public enum Keywords {
    Module = "module",
    Import = "import",
    Static = "static",
    Public = "public",
    Private = "private",
    Protected = "protected",
    Package = "package",
    Final = "final",
    Alias = "alias",
    Let = "let",
    Mut = "mut",
    Const = "const",
    Enum = "enum",
    Include = "include",
    New = "new",
    Cast = "cast",
    Ref = "ref",
    Out = "out",
    Struct = "struct",
    Fn = "fn",
    Return = "return",
    If = "if",
    Else = "else",
    Switch = "switch",
    Case = "case",
    Default = "default",
    Break = "break",
    Continue = "continue",
    Guard = "guard",
    While = "while",
    Do = "do",
    Union = "union",
    Prop = "prop",
    Get = "get",
    Set = "set",
    This = "this",
    For = "for",
    Foreach = "foreach",
    Interface = "interface",
    Template = "template",
    Unittest = "unittest",
    Assert = "assert",
    Extern = "extern",
    Internal = "internal",
    Int8 = "i8",
    Int16 = "i16",
    Int32 = "i32",
    Int64 = "i64",
    UInt8 = "u8",
    UInt16 = "u16",
    UInt32 = "u32",
    UInt64 = "u64",
    Float = "float",
    Double = "double",
    Real = "real",
    Char = "char",
    Void = "void",
    Bool = "bool",
    Ptr = "ptr",
    Size_T = "size_t",
    Ptrdiff_T = "ptrdiff_t",
    Sizeof = "sizeof",
    Trait = "trait",
    True = "true",
    False = "false",

    // Not built-in types, but aliases in runtime but should still not be allowed as an identifier etc.
    String = "string"
}

Keywords[string] keywordsMap;

void initializeKeywordMap() {
    static foreach (name; __traits(allMembers, Keywords)) {{
        static if (__traits(compiles, __traits(getMember, Keywords, name))) {
            enum member = __traits(getMember, Keywords, name);
            keywordsMap[member] = member;
        }
    }}
}

bool isKeyword(string s) {
    return cast(bool)(s in keywordsMap);
}