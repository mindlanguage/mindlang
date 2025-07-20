module mind.functions;

import mind.ast;
import mind.tokenizer;
import mind.parser;
import mind.access;
import mind.errors;
import mind.keywords;
import mind.types;
import mind.variables;
import mind.statements;
import mind.expressions;
import mind.attributes;
import mind.traits;

class FunctionDecl {
    Attribute[] attributes;
    AccessModifier access;
    Token token;
    string name;
    string[] templateParams;
    TraitEntry[] templateTraits;
    VariableDecl[] params;
    TypeReference[] returnTypes;
    bool isErrorFn;
    Statement[] statements;
    bool isExtern;
    bool isInternal;
    bool isVariadic;
    bool isMethod;

    this(Attribute[] attributes, AccessModifier access, Token token, string name, string[] templateParams, VariableDecl[] params, TypeReference[] returnTypes, bool isErrorFn) {
        this.attributes = attributes;
        this.access = access;
        this.token = token;
        this.name = name;
        this.templateParams = templateParams;
        this.params = params;
        this.returnTypes = returnTypes;
        this.isErrorFn = isErrorFn;
        this.statements = [];
        this.isMethod = false;
    }
}

void addThisParameterToFunction(FunctionDecl fn, string structName) {
    if (fn.access.isStatic) {
        return;
    }

    auto params = fn.params;
    auto thisTypeReference = new TypeReference;
    thisTypeReference.baseName = "ptr";
    auto structTypeReference = new TypeReference;
    structTypeReference.baseName = structName;
    thisTypeReference.typeArguments = [structTypeReference];
    // TODO: Change VarKind later on when language is more mature
    fn.params = [new VariableDecl(DefaultAccessModifier, [], fn.token, VarKind.Mut, "this", thisTypeReference, null)];
    fn.params ~= params;
}

FunctionDecl parseFunction(Attribute[] attributes, AccessModifier access, bool requireFnKeyword, ref Parser p, bool allowThisName = false) {
    bool hasFnToken = false;
    Token fnToken;
    if (requireFnKeyword) {
        fnToken = p.expect(TokenType.Identifier);
        hasFnToken = true;
        if (fnToken.lexeme != Keywords.Fn)
            throw new CompilerException("Expected 'fn' keyword.", fnToken);
    }

    bool isExtern = false;
    bool isInternal = false;

    if (p.match(TokenType.LParen)) {
        auto modeToken = p.expect(TokenType.Identifier);
        if (modeToken.lexeme == Keywords.Extern) {
            isExtern = true;
        } else if (modeToken.lexeme == Keywords.Internal) {
            isInternal = true;
        } else {
            throw new CompilerException("Expected 'extern' or 'internal' in fn(...)", modeToken);
        }

        p.expect(TokenType.RParen);
    }

    auto nameToken = p.expect(TokenType.Identifier);
    if (!hasFnToken) {
        fnToken = nameToken;
        hasFnToken = true;
    }
    string fnName = nameToken.lexeme;

    if (!allowThisName && isKeyword(nameToken.lexeme)) {
        throw new CompilerException("Cannot use keyword as identifier.", nameToken);
    }

    string[] templateParams;
    TraitEntry[] templateTraits;
    bool[string] typeMap;

    // Optional template parameters parsing
    if (p.peek().type == TokenType.LParen) {
        size_t openParens = 1;
        size_t i = p.pos + 1;

        while (i < p.tokens.length && openParens > 0) {
            if (p.tokens[i].type == TokenType.LParen) openParens++;
            else if (p.tokens[i].type == TokenType.RParen) openParens--;
            i++;
        }

        bool hasSecondParen = (i < p.tokens.length && p.tokens[i].type == TokenType.LParen);

        if (hasSecondParen) {
            p.expect(TokenType.LParen);
            if (p.peek().type != TokenType.RParen) {
                while (true) {
                    auto t = p.expect(TokenType.Identifier);
                    templateParams ~= t.lexeme;
            
                    if (t.lexeme in typeMap) {
                        throw new CompilerException("Duplicate type name.", t);
                    }
                    
                    typeMap[t.lexeme] = true;

                    if (p.peek().type == TokenType.Colon) {
                        p.next(); // consume :
                        
                        TraitArgument[] traitEntries;
                        while (true) {
                            auto trait = p.expect(TokenType.Identifier);

                            auto traitName = trait.lexeme;
                            string traitArgument;

                            if (p.peek().type == TokenType.Exclamation) {
                                p.next(); // consume !

                                traitArgument = p.expect(TokenType.Identifier).lexeme;
                            }

                            traitEntries ~= new TraitArgument(traitName, traitArgument);

                            if (p.peek().type == TokenType.RParen ||
                                p.peek().type == TokenType.Comma)
                                break;
                            p.expect(TokenType.AndAnd);
                        }

                        templateTraits ~= new TraitEntry(traitEntries);
                    } else {
                        templateTraits ~= new TraitEntry([]);
                    }

                    if (p.match(TokenType.RParen)) break;
                    p.expect(TokenType.Comma);
                }
            } else {
                p.expect(TokenType.RParen);
            }
        }
    }

    // Function parameters parsing with variadic support added:
    VariableDecl[] parameters;
    bool isVariadic = false;  // Track variadic flag

    if (p.peek().type != TokenType.LParen)
        throw new CompilerException("Expected '(' to start function parameters.", p.peek());

    p.expect(TokenType.LParen);

    if (p.peek().type == TokenType.RParen) {
        p.expect(TokenType.RParen); // empty params
    } else {
        while (true) {
            // Check for variadic ... token
            if (p.peek().type == TokenType.DotDotDot) {
                p.next(); // consume '...'
                isVariadic = true;
                p.expect(TokenType.RParen);
                break;
            }

            auto paramAttributes = parseAttributes(p);
            auto paramAccess = parseAccessModifier(true, p);
            auto param = parseVariableDeclaration(paramAttributes, true, VarKind.Let, paramAccess, p);
            parameters ~= param;

            if (p.match(TokenType.RParen)) break;
            p.expect(TokenType.Comma);
        }
    }

    // Optional return types
    TypeReference[] returnTypes;
    bool isErrorFn = false;
    if (p.peek().type == TokenType.TildeArrow ||
        p.peek().type == TokenType.DashArrow) {
        isErrorFn = p.peek().type == TokenType.TildeArrow;
        auto tok = p.next(); // consume arrow

        while (true) {
            returnTypes ~= parseTypeReference(p);
            if (!p.match(TokenType.Comma)) break;
        }

        if (returnTypes && (
            isErrorFn ? (returnTypes.length > 2) : (returnTypes.length > 1)
        )) {
            throw new CompilerException("Too many return types specified in function '" ~ fnName ~ "'.", tok);
        }
    }

    if (!returnTypes || !returnTypes.length) {
        returnTypes = [new TypeReference(Keywords.Void)];
    }

    auto fn = new FunctionDecl(
        attributes,
        access,
        fnToken,
        fnName,
        templateParams,
        parameters,
        returnTypes,
        isErrorFn
    );

    fn.isExtern = isExtern;
    fn.isInternal = isInternal;
    fn.isVariadic = isVariadic;  // Set variadic flag

    fn.templateTraits = templateTraits;

    // Parse function body or semicolon
    if (p.peek().type == TokenType.EqualsArrow) {
        p.next(); // consume =>
        fn.statements = [parseReturnStatement(false, p)];
        p.expect(TokenType.Semicolon);
    } else if (p.peek().type == TokenType.LBrace) {
        p.next(); // consume {
        fn.statements = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
    } else {
        p.expect(TokenType.Semicolon);
    }

    return fn;
}

Statement[] parseStatements(TokenType endToken, ref Parser p) {
    Statement[] statements;

    while (p.peek().type != endToken) {
        auto stmt = parseStatement(p);
        if (requiresSemicolon(stmt)) {
            p.expect(TokenType.Semicolon);
        }
        statements ~= stmt;
    }

    return statements;
}

Statement parseStatement(ref Parser p) {
    auto token = p.peek();

    // Direct block statement support
    if (token.type == TokenType.LBrace) {
        p.next(); // consume '{'
        auto stmts = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
        return new BlockStatement(stmts);
    }

    // Handle keyword-based statements first
    if (token.type == TokenType.Identifier) {
        switch (token.lexeme) {      
            case Keywords.Fn:
                // Parse nested function
                auto nestedFn = parseFunction([], DefaultAccessModifier, true, p);
                return new FunctionDeclStatement(token, nestedFn);

            case Keywords.Return:
                return parseReturnStatement(true, p);
            case Keywords.If:
                return parseIfStatement(p);
            case Keywords.Switch:
                return parseSwitchStatement(p);
            case Keywords.Break:
                return parseBreakStatement(p);
            case Keywords.Continue:
                return parseContinueStatement(p);
            case Keywords.Guard:
                return parseGuardStatement(p);
            case Keywords.While:
                return parseWhileStatement(p);
            case Keywords.Do:
                return parseDoWhileStatement(p);
            case Keywords.For:
                return parseForStatement(p);
            case Keywords.Foreach:
                return parseForeachStatement(p);

            case Keywords.Let:
            case Keywords.Mut:
            case Keywords.Const:
            case Keywords.Enum:
                auto variable = parseVariableDeclaration([], false, VarKind.Let, DefaultAccessModifier, p);

                return new VariableStatement(token, variable);

            case Keywords.Assert:
                return parseAssertStatement(p);
                
            default:
                break;
        }
    }

    // Try to parse an expression, then check if it's followed by an LR-assignment operator
    auto expr = parseExpression(p);

    auto op = p.peek();
    bool isLRAssignment =
        op.type == TokenType.Equal ||             // =
        op.type == TokenType.PlusEqual ||         // +=
        op.type == TokenType.MinusEqual ||        // -=
        op.type == TokenType.MulEqual ||          // *=
        op.type == TokenType.DivEqual ||          // /=
        op.type == TokenType.ModEqual ||          // %=
        op.type == TokenType.OrEqual ||           // |=
        op.type == TokenType.AndEqual ||          // &=
        op.type == TokenType.XorEqual ||          // ^=
        op.type == TokenType.LeftShiftAssign ||   // <<=
        op.type == TokenType.DoubleShiftAssign || // >>=
        op.type == TokenType.TildeEqual;          // ~=

    if (isLRAssignment) {
        return parseLRStatementFromExpr(expr, p); // pass expr to avoid re-parsing
    }

    return new ExprStatement(expr);
}
LRStatement parseLRStatementFromExpr(Expr left, ref Parser p) {
    auto statementToken = p.peek();
    auto op = p.next(); // consume operator

    bool allowedOperator =
        op.type == TokenType.Equal ||             // =
        op.type == TokenType.PlusEqual ||         // +=
        op.type == TokenType.MinusEqual ||        // -=
        op.type == TokenType.MulEqual ||          // *=
        op.type == TokenType.DivEqual ||          // /=
        op.type == TokenType.ModEqual ||          // %=
        op.type == TokenType.OrEqual ||           // |=
        op.type == TokenType.AndEqual ||          // &=
        op.type == TokenType.XorEqual ||          // ^=
        op.type == TokenType.LeftShiftAssign ||   // <<=
        op.type == TokenType.DoubleShiftAssign || // >>=
        op.type == TokenType.TildeEqual;          // ~=

    if (!allowedOperator) {
        throw new CompilerException("Invalid operator for LR statement.", op);
    }

    auto right = parseExpression(p);
    
    return new LRStatement(statementToken, left, op, right);
}

ReturnStatement parseReturnStatement(bool parseReturnKeyword, ref Parser p) {
    Token returnToken;
    if (parseReturnKeyword) {
        returnToken = p.expect(TokenType.Identifier);
        if (returnToken.lexeme != Keywords.Return)
            throw new CompilerException("Expected 'return' keyword.", returnToken);
    }
    else {
        returnToken = p.peek();
    }

    auto expression = parseExpression(p);

    return new ReturnStatement(returnToken, expression);
}

IfStatement parseIfStatement(ref Parser p) {
    auto ifToken = p.expect(TokenType.Identifier);
    if (ifToken.lexeme != Keywords.If)
        throw new CompilerException("Expected 'if' keyword.", ifToken);

    p.expect(TokenType.LParen);
    auto condition = parseExpression(p);
    p.expect(TokenType.RParen);

    Statement[] thenBody;

    if (p.peek().type == TokenType.EqualsArrow) {
        p.next(); // consume =>
        thenBody = [parseStatementWithOptionalSemicolon(p)];
    }
    else if (p.peek().type == TokenType.LBrace) {
        p.next(); // consume {
        thenBody = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
    }
    else {
        thenBody = [parseStatementWithOptionalSemicolon(p)];
    }

    Statement elseBranch = null;

    if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.Else) {
        p.next(); // consume else

        if (p.peek().type == TokenType.Identifier && p.peek().lexeme == Keywords.If) {
            // else if — parse recursively
            elseBranch = parseIfStatement(p);
        }
        else if (p.peek().type == TokenType.LBrace) {
            p.next(); // consume {
            elseBranch = new BlockStatement(parseStatements(TokenType.RBrace, p));
            p.expect(TokenType.RBrace);
        }
        else {
            elseBranch = parseStatementWithOptionalSemicolon(p);
        }
    }

    return new IfStatement(ifToken, condition, thenBody, elseBranch);
}

Statement parseStatementWithOptionalSemicolon(ref Parser p) {
    auto stmt = parseStatement(p);
    if (requiresSemicolon(stmt)) {
        p.expect(TokenType.Semicolon);
    }
    return stmt;
}

SwitchStatement parseSwitchStatement(ref Parser p) {
    auto switchToken = p.expect(TokenType.Identifier);
    if (switchToken.lexeme != Keywords.Switch)
        throw new CompilerException("Expected 'switch' keyword.", switchToken);

    p.expect(TokenType.LParen);
    auto condition = parseExpression(p);
    p.expect(TokenType.RParen);
    p.expect(TokenType.LBrace);

    CaseClause[] cases;
    DefaultClause defaultClause = null;

    while (p.peek().type != TokenType.RBrace) {
        auto current = p.peek();

        if (current.type == TokenType.Identifier && current.lexeme == Keywords.Case) {
            auto caseToken = p.next();
            auto value = parseExpression(p);

            if (!p.match(TokenType.Colon) && !p.match(TokenType.EqualsArrow))
                throw new CompilerException("Expected ':' or '=>' after case expression.", p.peek());

            Statement[] body;

            if (p.peek().type == TokenType.LBrace) {
                p.next(); // consume {
                body = parseStatements(TokenType.RBrace, p);
                p.expect(TokenType.RBrace);
            } else {
                // allow one-liners or indented block-like lines
                body = [];

                while (p.peek().type != TokenType.Identifier ||
                       (p.peek().lexeme != Keywords.Case &&
                        p.peek().lexeme != Keywords.Default &&
                        p.peek().type != TokenType.RBrace)) {
                    body ~= parseStatementWithOptionalSemicolon(p);
                    if (p.peek().type == TokenType.RBrace) break;
                    if (p.peek().lexeme == Keywords.Case || p.peek().lexeme == Keywords.Default) break;
                }
            }

            cases ~= new CaseClause(caseToken, value, body);
        }
        else if (current.type == TokenType.Identifier && current.lexeme == Keywords.Default) {
            if (defaultClause !is null)
                throw new CompilerException("Multiple 'default' clauses not allowed.", current);

            auto defaultToken = p.next();

            if (!p.match(TokenType.Colon) && !p.match(TokenType.EqualsArrow))
                throw new CompilerException("Expected ':' or '=>' after 'default'.", p.peek());

            Statement[] body;

            if (p.peek().type == TokenType.LBrace) {
                p.next(); // consume {
                body = parseStatements(TokenType.RBrace, p);
                p.expect(TokenType.RBrace);
            } else {
                body = [];

                while (p.peek().type != TokenType.RBrace) {
                    body ~= parseStatementWithOptionalSemicolon(p);
                    if (p.peek().lexeme == Keywords.Case || p.peek().lexeme == Keywords.Default) break;
                }
            }

            defaultClause = new DefaultClause(defaultToken, body);
        }
        else {
            throw new CompilerException("Expected 'case' or 'default' in switch statement.", current);
        }
    }

    p.expect(TokenType.RBrace);

    return new SwitchStatement(switchToken, condition, cases, defaultClause);
}

BreakStatement parseBreakStatement(ref Parser p) {
    auto tok = p.expect(TokenType.Identifier);
    if (tok.lexeme != Keywords.Break)
        throw new CompilerException("Expected 'break'.", tok);

    return new BreakStatement(tok);
}

ContinueStatement parseContinueStatement(ref Parser p) {
    auto tok = p.expect(TokenType.Identifier);
    if (tok.lexeme != Keywords.Continue)
        throw new CompilerException("Expected 'continue'.", tok);

    return new ContinueStatement(tok);
}

GuardStatement parseGuardStatement(ref Parser p) {
    auto guardToken = p.expect(TokenType.Identifier);
    if (guardToken.lexeme != Keywords.Guard)
        throw new CompilerException("Expected 'guard' keyword.", guardToken);

    auto condition = parseExpression(p);

    if (!p.match(TokenType.Colon))
        throw new CompilerException("Expected ':' after guard condition.", p.peek());

    auto elseExpr = parseExpression(p);
    
    return new GuardStatement(guardToken, condition, elseExpr);
}

WhileStatement parseWhileStatement(ref Parser p) {
    auto whileToken = p.expect(TokenType.Identifier);
    if (whileToken.lexeme != Keywords.While)
        throw new CompilerException("Expected 'while' keyword.", whileToken);

    p.expect(TokenType.LParen);
    auto condition = parseExpression(p);
    p.expect(TokenType.RParen);

    Statement body;

    if (p.peek().type == TokenType.LBrace) {
        p.next(); // consume '{'
        auto stmts = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
        body = new BlockStatement(stmts);
    } else {
        body = parseStatementWithOptionalSemicolon(p);
    }

    return new WhileStatement(whileToken, condition, body);
}

DoWhileStatement parseDoWhileStatement(ref Parser p) {
    auto doToken = p.expect(TokenType.Identifier);
    if (doToken.lexeme != Keywords.Do)
        throw new CompilerException("Expected 'do' keyword.", doToken);

    Statement body;

    if (p.peek().type == TokenType.LBrace) {
        p.next(); // consume '{'
        auto stmts = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
        body = new BlockStatement(stmts);
    } else {
        body = parseStatementWithOptionalSemicolon(p);
    }

    auto whileToken = p.expect(TokenType.Identifier);
    if (whileToken.lexeme != Keywords.While)
        throw new CompilerException("Expected 'while' keyword after 'do' body.", whileToken);

    p.expect(TokenType.LParen);
    auto condition = parseExpression(p);
    p.expect(TokenType.RParen);
    p.expect(TokenType.Semicolon);

    return new DoWhileStatement(doToken, body, whileToken, condition);
}

ForStatement parseForStatement(ref Parser p) {
    auto forToken = p.expect(TokenType.Identifier);
    if (forToken.lexeme != Keywords.For)
        throw new CompilerException("Expected 'for' keyword.", forToken);

    p.expect(TokenType.LParen);

    // Parse variable declaration (only VarKind.Let allowed for now)
    auto varAccess = parseAccessModifier(true, p);
    auto initializer = parseVariableDeclaration([], true, VarKind.Let, varAccess, p);
    p.expect(TokenType.Semicolon);

    auto condition = parseExpression(p);
    p.expect(TokenType.Semicolon);

    auto updateStmt = parseStatement(p); // must be a statement

    p.expect(TokenType.RParen);

    Statement body;
    if (p.peek().type == TokenType.LBrace) {
        p.next(); // consume {
        auto stmts = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
        body = new BlockStatement(stmts);
    } else {
        body = parseStatementWithOptionalSemicolon(p);
    }

    return new ForStatement(forToken, initializer, condition, updateStmt, body);
}

ForeachStatement parseForeachStatement(ref Parser p) {
    auto foreachToken = p.expect(TokenType.Identifier);
    if (foreachToken.lexeme != Keywords.Foreach)
        throw new CompilerException("Expected 'foreach' keyword.", foreachToken);

    p.expect(TokenType.LParen);

    // Parse identifiers: either 1 or 2
    string[] identifiers;

    auto firstIdent = p.expect(TokenType.Identifier);
    identifiers ~= firstIdent.lexeme;

    if (p.match(TokenType.Comma)) {
        auto secondIdent = p.expect(TokenType.Identifier);
        identifiers ~= secondIdent.lexeme;
    }

    p.expect(TokenType.Semicolon);

    // Detect range vs collection
    auto firstExpr = parseExpression(p);
    Expr endExpr = null;

    if (p.match(TokenType.DotDot)) { // Range token should be for `..`
        endExpr = parseExpression(p);
    }

    p.expect(TokenType.RParen);

    Statement body;
    if (p.peek().type == TokenType.LBrace) {
        p.next(); // consume {
        auto stmts = parseStatements(TokenType.RBrace, p);
        p.expect(TokenType.RBrace);
        body = new BlockStatement(stmts);
    } else {
        body = parseStatementWithOptionalSemicolon(p);
    }

    return new ForeachStatement(foreachToken, identifiers, firstExpr, endExpr, body);
}

AssertStatement parseAssertStatement(ref Parser p) {
    auto assertToken = p.expect(TokenType.Identifier);
    if (assertToken.lexeme != Keywords.Assert)
        throw new CompilerException("Expected 'assert' keyword.", assertToken);

    p.expect(TokenType.LParen);

    auto condition = parseExpression(p);
    Expr message = null;

    if (p.match(TokenType.Comma)) {
        message = parseExpression(p);
    }

    p.expect(TokenType.RParen);

    return new AssertStatement(assertToken, condition, message);
}

bool requiresSemicolon(Statement stmt) {
    if (cast(IfStatement) stmt !is null) return false;
    if (cast(BlockStatement) stmt !is null) return false;
    if (cast(SwitchStatement) stmt !is null) return false;
    if (cast(FunctionDeclStatement) stmt !is null) return false;
    if (cast(WhileStatement) stmt !is null) return false;
    if (cast(DoWhileStatement) stmt !is null) return false;
    if (cast(ForStatement) stmt !is null) return false;
    if (cast(ForeachStatement) stmt !is null) return false;

    if (cast(ReturnStatement) stmt !is null) return true;
    if (cast(LRStatement) stmt !is null) return true;
    if (cast(ExprStatement) stmt !is null) return true;
    if (cast(BreakStatement) stmt !is null) return true;
    if (cast(ContinueStatement) stmt !is null) return true;
    if (cast(GuardStatement) stmt !is null) return true;

    // Default to requiring semicolon
    return true;
}