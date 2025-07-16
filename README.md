# mindlang

mindlang is a statically-typed programming language written in D.

Currently a WIP and only supports somewhat parsing of expressions, types etc.

There is no backend currently and there is no semantic analysis.

Hello World!

```
import std.stdio : println;

fn main() => println("Hello mind!");
```

## Compiling

```
dub build -a=x86_64 --force
```

## Parsing a mind file

Ex. 

```
mind.exe -s="main.mind" -v
```

Where main.mind looks like this:

```
module main;

import std.stdio : println;

fn main() => println("Hello mind!");
```

That will currently output the tokens of the program and parse it properly.

```
[Identifier] 'module' 1,1
[Identifier] 'main' 1,8
[Semicolon] ';' 1,12
[Identifier] 'import' 3,1
[Identifier] 'std' 3,8
[Dot] '.' 3,11
[Identifier] 'stdio' 3,12
[Colon] ':' 3,18
[Identifier] 'println' 3,20
[Semicolon] ';' 3,27
[Identifier] 'fn' 5,1
[Identifier] 'main' 5,4
[LParen] '(' 5,8
[RParen] ')' 5,9
[EqualsArrow] '=>' 5,11
[Identifier] 'println' 5,14
[LParen] '(' 5,21
[StringLiteral] '"Hello mind!"' 5,22
[RParen] ')' 5,35
[Semicolon] ';' 5,36
```

## More Info

The semantic analyzer is a WIP, so programs might (probably will) error out.

See the wiki: https://github.com/mindlanguage/mindlang/wiki
