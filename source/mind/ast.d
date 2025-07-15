module mind.ast;

import mind.statements;
import mind.tokenizer;

abstract class Expr {
    Token token;
    this(Token token) {
        this.token = token;
    }
}

class IdentifierExpr : Expr {
    string name;
    this(string name, Token token) {
        super(token);
        this.name = name;
    }
}

class LiteralExpr : Expr {
    string value;
    this(string value, Token token) {
        super(token);
        this.value = value;
    }
}

class BinaryExpr : Expr {
    Expr left;
    string op;
    Expr right;
    this(Expr left, string op, Expr right, Token token) {
        super(token);
        this.left = left;
        this.op = op;
        this.right = right;
    }
}

class CallExpr : Expr {
    Expr callee;
    Expr[] arguments;
    this(Expr callee, Expr[] arguments, Token token) {
        super(token);
        this.callee = callee;
        this.arguments = arguments.dup;
    }
}

class ListExpr : Expr {
    Expr[] elements;
    this(Expr[] elements, Token token) {
        super(token);
        this.elements = elements.dup;
    }
}

class GroupingExpr : Expr {
    Expr expression;
    this(Expr expression, Token token) {
        super(token);
        this.expression = expression;
    }
}

class CastExpr : Expr {
    Expr targetType;
    Expr expr;
    this(Expr targetType, Expr expr, Token token) {
        super(token);
        this.targetType = targetType;
        this.expr = expr;
    }
}

class TemplatedExpr : Expr {
    Expr target;
    Expr[] templateArgs;
    this(Expr target, Expr[] templateArgs, Token token) {
        super(token);
        this.target = target;
        this.templateArgs = templateArgs;
    }
}

class NewExpr : Expr {
    Expr typeExpr;
    Expr[] arguments;
    this(Expr typeExpr, Expr[] arguments, Token token) {
        super(token);
        this.typeExpr = typeExpr;
        this.arguments = arguments;
    }
}

class InterpolatedStringExpr : Expr {
    Expr[] parts;
    this(Expr[] parts, Token token) {
        super(token);
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
    DefaultExpr defaultCase;

    this(Token switchToken, Expr condition, CaseExpr[] cases, DefaultExpr defaultCase = null) {
        super(switchToken);
        this.switchToken = switchToken;
        this.condition = condition;
        this.cases = cases;
        this.defaultCase = defaultCase;
    }
}

class CaseExpr {
    Token caseToken;
    Expr value;
    Expr body;

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

    this(Expr target, IdentifierExpr member, Token token) {
        super(token);
        this.target = target;
        this.member = member;
    }
}

class LambdaExpr : Expr {
    Token fnToken;
    string[] paramNames;
    Statement[] bodyStatements;
    Expr bodyExpression;

    this(Token fnToken, string[] paramNames, Statement[] stmts, Expr expr) {
        super(fnToken);
        this.fnToken = fnToken;
        this.paramNames = paramNames;
        this.bodyStatements = stmts;
        this.bodyExpression = expr;
    }
}

class UnaryExpr : Expr {
    string op;
    Expr operand;
    bool isPostfix;

    this(string op, Expr operand, Token token, bool isPostfix = false) {
        super(token);
        this.op = op;
        this.operand = operand;
        this.isPostfix = isPostfix;
    }
}

class ArrayIndexExpr : Expr {
    Expr arrayExpr;
    Expr indexExpr;

    this(Expr arrayExpr, Expr indexExpr, Token token) {
        super(token);
        this.arrayExpr = arrayExpr;
        this.indexExpr = indexExpr;
    }
}
