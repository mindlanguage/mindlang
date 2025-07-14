# mindlang

mindlang is a statically-typed programming language written in D.

Hello World!

```
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

fn main() => println("Hello mind!");
```
