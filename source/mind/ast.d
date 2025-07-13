module mind.ast;

import mind.statements;
import mind.tokenizer;

abstract class Expr {}

class IdentifierExpr : Expr {
    string name;
    this(string name) { this.name = name; }
}

class LiteralExpr : Expr {
    string value;
    this(string value) { this.value = value; }
}

class BinaryExpr : Expr {
    Expr left;
    string op;
    Expr right;
    this(Expr left, string op, Expr right) {
        this.left = left;
        this.op = op;
        this.right = right;
    }
}

class CallExpr : Expr {
    Expr callee;
    Expr[] arguments;
    this(Expr callee, Expr[] arguments) {
        this.callee = callee;
        this.arguments = arguments.dup;
    }
}

class ListExpr : Expr {
    Expr[] elements;
    this(Expr[] elements) {
        this.elements = elements.dup;
    }
}

class GroupingExpr : Expr {
    Expr expression;
    this(Expr expression) {
        this.expression = expression;
    }
}

class CastExpr : Expr {
    Expr targetType;  // or a richer Type AST if you have type nodes
    Expr expr;

    this(Expr targetType, Expr expr) {
        this.targetType = targetType;
        this.expr = expr;
    }
}

class TemplatedExpr : Expr {
    Expr target;
    Expr[] templateArgs;
    this(Expr target, Expr[] templateArgs) {
        this.target = target;
        this.templateArgs = templateArgs;
    }
}

class NewExpr : Expr {
    Expr typeExpr; // The type being instantiated
    Expr[] arguments;

    this(Expr typeExpr, Expr[] arguments) {
        this.typeExpr = typeExpr;
        this.arguments = arguments;
    }
}

class InterpolatedStringExpr : Expr {
    Expr[] parts; // sequence of string literals and embedded expressions

    this(Expr[] parts) {
        this.parts = parts;
    }

    override string toString() {
        import std.algorithm : map;
        import std.array : join;
        return parts.map!(p => p.toString()).join("");
    }
}

class SwitchExpr : Expr {
    Token switchToken;
    Expr condition;
    CaseExpr[] cases;
    DefaultExpr defaultCase;  // nullable

    this(Token switchToken, Expr condition, CaseExpr[] cases, DefaultExpr defaultCase = null) {
        this.switchToken = switchToken;
        this.condition = condition;
        this.cases = cases;
        this.defaultCase = defaultCase;
    }
}

class CaseExpr {
    Token caseToken;
    Expr value;        // the case matching expression
    Expr body;         // the expression for the case body

    this(Token caseToken, Expr value, Expr body) {
        this.caseToken = caseToken;
        this.value = value;
        this.body = body;
    }
}

class DefaultExpr {
    Token defaultToken;
    Expr body;

    this(Token defaultToken, Expr body) {
        this.defaultToken = defaultToken;
        this.body = body;
    }
}

class QualifiedAccessExpr : Expr {
    Expr target;
    IdentifierExpr member;

    this(Expr target, IdentifierExpr member) {
        this.target = target;
        this.member = member;
    }
}

class LambdaExpr : Expr {
    Token fnToken;
    string[] paramNames; // or VariableDecl[] if you want types
    Statement[] bodyStatements; // non-null if block body
    Expr bodyExpression; // non-null if expression body

    this(Token fnToken, string[] paramNames, Statement[] stmts, Expr expr) {
        this.fnToken = fnToken;
        this.paramNames = paramNames;
        this.bodyStatements = stmts;
        this.bodyExpression = expr;
    }
}