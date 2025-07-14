module mind.statements;

import mind.ast;
import mind.tokenizer;
import mind.functions;
import mind.variables;

abstract class Statement {}

class LRStatement : Statement {
    Token token;
    Expr leftExpression;
    Token operator;
    Expr rightExpression;

    this(Token token, Expr leftExpression, Token operator, Expr rightExpression) {
        this.leftExpression = leftExpression;
        this.operator = operator;
        this.rightExpression = rightExpression;
    }
}

class ReturnStatement : Statement {
    Token token;
    Expr returnExpression;

    this(Token token, Expr returnExpression) {
        this.token = token;
        this.returnExpression = returnExpression;
    }
}

class ExprStatement : Statement {
    Expr expression;
    this(Expr e) {
        expression = e;
    }
}

class IfStatement : Statement {
    Token token;
    Expr condition;
    Statement[] body;
    Statement elseBranch;  // can be null, another IfStatement, or a BlockStatement

    this(Token token, Expr condition, Statement[] body, Statement elseBranch = null) {
        this.token = token;
        this.condition = condition;
        this.body = body;
        this.elseBranch = elseBranch;
    }
}

class BlockStatement : Statement {
    Statement[] statements;

    this(Statement[] statements) {
        this.statements = statements;
    }
}

class SwitchStatement : Statement {
    Token token;
    Expr condition;
    CaseClause[] cases;
    DefaultClause defaultClause;

    this(Token token, Expr condition, CaseClause[] cases, DefaultClause defaultClause = null) {
        this.token = token;
        this.condition = condition;
        this.cases = cases;
        this.defaultClause = defaultClause;
    }
}

class CaseClause {
    Token token;
    Expr value;
    Statement[] body;

    this(Token token, Expr value, Statement[] body) {
        this.token = token;
        this.value = value;
        this.body = body;
    }
}

class DefaultClause {
    Token token;
    Statement[] body;

    this(Token token, Statement[] body) {
        this.token = token;
        this.body = body;
    }
}

class BreakStatement : Statement {
    Token token;

    this(Token token) {
        this.token = token;
    }
}

class ContinueStatement : Statement {
    Token token;

    this(Token token) {
        this.token = token;
    }
}

class GuardStatement : Statement {
    Token guardToken;
    Expr condition;
    Expr elseExpression;

    this(Token guardToken, Expr condition, Expr elseExpression) {
        this.guardToken = guardToken;
        this.condition = condition;
        this.elseExpression = elseExpression;
    }
}

class FunctionDeclStatement : Statement {
    Token token;
    FunctionDecl fn;

    this(Token token, FunctionDecl fn) {
        this.token = token;
        this.fn = fn;
    }
}

class WhileStatement : Statement {
    Token whileToken;
    Expr condition;
    Statement body;

    this(Token whileToken, Expr condition, Statement body) {
        this.whileToken = whileToken;
        this.condition = condition;
        this.body = body;
    }
}

class DoWhileStatement : Statement {
    Token doToken;
    Statement body;
    Token whileToken;
    Expr condition;

    this(Token doToken, Statement body, Token whileToken, Expr condition) {
        this.doToken = doToken;
        this.body = body;
        this.whileToken = whileToken;
        this.condition = condition;
    }
}

class ForStatement : Statement {
    Token forToken;
    VariableDecl initializer;
    Expr condition;
    Statement update;
    Statement body;

    this(Token forToken, VariableDecl initializer, Expr condition, Statement update, Statement body) {
        this.forToken = forToken;
        this.initializer = initializer;
        this.condition = condition;
        this.update = update;
        this.body = body;
    }
}

class ForeachStatement : Statement {
    Token foreachToken;
    string[] identifiers;
    Expr iterableOrStart;
    Expr endRange; // null unless range-based
    Statement body;

    this(Token foreachToken, string[] identifiers, Expr iterableOrStart, Expr endRange, Statement body) {
        this.foreachToken = foreachToken;
        this.identifiers = identifiers;
        this.iterableOrStart = iterableOrStart;
        this.endRange = endRange;
        this.body = body;
    }
}

class VariableStatement : Statement {
    Token variableToken;
    VariableDecl variable;

    this(Token variableToken, VariableDecl variable) {
        this.variableToken = variableToken;
        this.variable = variable;
    }
}

class AssertStatement : Statement {
    Token assertToken;
    Expr condition;
    Expr message;

    this(Token assertToken, Expr condition, Expr message) {
        this.assertToken = assertToken;
        this.condition = condition;
        this.message = message;
    }
}