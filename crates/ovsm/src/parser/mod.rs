//! Parser for OVSM language
//!
//! Converts token stream into Abstract Syntax Tree (AST) using recursive descent parsing.
//!
//! # Architecture
//!
//! The parser implements a **recursive descent parser** with operator precedence climbing:
//! - **Top-down**: Starts from high-level constructs (statements) and descends to expressions
//! - **Precedence levels**: Expression parsing respects operator precedence (ternary > or > and > equality > comparison > term > factor > unary > power > call > primary)
//! - **Lookahead**: Uses single-token lookahead (`peek()`) for parsing decisions
//!
//! # Usage Example
//!
//! ```rust
//! use ovsm::lexer::Scanner;
//! use ovsm::parser::Parser;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let code = "$x = 42\nRETURN $x * 2";
//! let mut scanner = Scanner::new(code);
//! let tokens = scanner.scan_tokens()?;
//! let mut parser = Parser::new(tokens);
//! let program = parser.parse()?;  // Returns AST
//! # Ok(())
//! # }
//! ```
//!
//! # Parsing Strategy
//!
//! - **Statements**: Assignment, control flow (IF/WHILE/FOR), RETURN, BREAK, etc.
//! - **Expressions**: Binary ops, unary ops, literals, variables, tool calls, arrays, objects
//! - **Indentation-agnostic**: Uses keywords (THEN, ELSE, colons) for block structure
//! - **Error recovery**: Fails fast on syntax errors with detailed messages

/// Abstract Syntax Tree (AST) definitions for OVSM
pub mod ast;

pub use ast::*;

use crate::error::{Error, Result};
use crate::lexer::{Token, TokenKind};

/// Recursive descent parser for OVSM language
///
/// Transforms a token stream into an Abstract Syntax Tree (AST). The parser
/// maintains a cursor (`current`) that advances through the tokens, using
/// lookahead and recursive descent to build the AST.
///
/// # Key Methods
///
/// - `parse()`: Entry point - parses entire program into AST
/// - `statement()`: Parses individual statements (assignment, control flow, etc.)
/// - `expression()`: Parses expressions with operator precedence
pub struct Parser {
    /// Token stream to parse
    tokens: Vec<Token>,
    /// Current position in token stream (acts as cursor)
    current: usize,
}

impl Parser {
    /// Create new parser from token stream
    ///
    /// # Example
    ///
    /// ```rust
    /// use ovsm::lexer::Scanner;
    /// use ovsm::parser::Parser;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut scanner = Scanner::new("$x = 10");
    /// let tokens = scanner.scan_tokens()?;
    /// let parser = Parser::new(tokens);
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    /// Parse tokens into a Program AST
    ///
    /// This is the main entry point for parsing. It processes all statements
    /// in the token stream and returns a complete AST representation.
    ///
    /// # Returns
    ///
    /// - `Ok(Program)`: Successfully parsed AST with statements and metadata
    /// - `Err(Error)`: Syntax error with detailed location and context
    ///
    /// # Example
    ///
    /// ```rust
    /// use ovsm::lexer::Scanner;
    /// use ovsm::parser::Parser;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut scanner = Scanner::new("$x = 10");
    /// let tokens = scanner.scan_tokens()?;
    /// let mut parser = Parser::new(tokens);
    /// let program = parser.parse()?;
    /// // program.statements contains the AST
    /// # Ok(())
    /// # }
    /// ```
    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = Vec::new();

        // Skip leading newlines
        self.skip_newlines();

        while !self.is_at_end() {
            let stmt = self.statement()?;
            statements.push(stmt);
            self.skip_newlines();
        }

        Ok(Program {
            metadata: ProgramMetadata::default(),
            statements,
        })
    }

    // Statement parsing

    fn statement(&mut self) -> Result<Statement> {
        match &self.peek().kind {
            TokenKind::Variable(_) => self.assignment_or_expression(),
            TokenKind::Const => self.constant_def(),
            TokenKind::If => self.if_statement(),
            TokenKind::While => self.while_statement(),
            TokenKind::For => self.for_statement(),
            TokenKind::Return => self.return_statement(),
            TokenKind::Break => self.break_statement(),
            TokenKind::Continue => self.continue_statement(),
            TokenKind::Try => self.try_statement(),
            TokenKind::Parallel => self.parallel_statement(),
            TokenKind::WaitAll | TokenKind::WaitAny | TokenKind::Race => {
                self.wait_strategy_statement()
            }
            TokenKind::Decision => self.decision_statement(),
            TokenKind::Guard => self.guard_statement(),
            _ => self.expression_statement(),
        }
    }

    fn assignment_or_expression(&mut self) -> Result<Statement> {
        if let TokenKind::Variable(name) = &self.peek().kind.clone() {
            let name = name.clone();
            self.advance();

            if self.match_token(&TokenKind::Assign) {
                self.advance();
                let value = self.expression()?;
                self.consume_statement_separator()?;
                return Ok(Statement::Assignment { name, value });
            } else {
                // It's just a variable reference, backtrack
                self.current -= 1;
            }
        }

        self.expression_statement()
    }

    fn constant_def(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Const, "Expected CONST")?;

        let name = match &self.peek().kind {
            TokenKind::Constant(n) => n.clone(),
            _ => {
                return Err(Error::ParseError(
                    "Expected constant name (UPPERCASE)".to_string(),
                ))
            }
        };
        self.advance();

        self.consume(TokenKind::Assign, "Expected '=' after constant name")?;

        let value = self.expression()?;
        self.consume_statement_separator()?;

        Ok(Statement::ConstantDef { name, value })
    }

    fn if_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::If, "Expected IF")?;

        let condition = self.expression()?;

        self.consume(TokenKind::Then, "Expected THEN after IF condition")?;
        self.skip_newlines();

        let mut then_branch = Vec::new();
        while !self.check(&TokenKind::Else)
            && !self.check(&TokenKind::Eof)
            && !self.is_end_of_block()
        {
            then_branch.push(self.statement()?);
            self.skip_newlines();
        }

        let else_branch = if self.match_token(&TokenKind::Else) {
            self.advance();
            self.skip_newlines();

            let mut else_stmts = Vec::new();
            while !self.is_end_of_block() && !self.is_at_end() {
                else_stmts.push(self.statement()?);
                self.skip_newlines();
            }
            Some(else_stmts)
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::While, "Expected WHILE")?;

        let condition = self.expression()?;

        self.consume(TokenKind::Colon, "Expected ':' after WHILE condition")?;
        self.skip_newlines();

        let mut body = Vec::new();
        while !self.is_at_end() {
            self.skip_newlines(); // Skip newlines before checking end condition
            if self.is_end_of_loop_block() {
                break;
            }
            body.push(self.statement()?);
        }

        Ok(Statement::While { condition, body })
    }

    fn for_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::For, "Expected FOR")?;

        let variable = match &self.peek().kind {
            TokenKind::Variable(name) => name.clone(),
            _ => return Err(Error::ParseError("Expected variable after FOR".to_string())),
        };
        self.advance();

        self.consume(TokenKind::In, "Expected IN after FOR variable")?;

        let iterable = self.expression()?;

        self.consume(TokenKind::Colon, "Expected ':' after FOR iterable")?;
        self.skip_newlines();

        let mut body = Vec::new();
        while !self.is_at_end() {
            self.skip_newlines(); // Skip newlines before checking end condition
            if self.is_end_of_loop_block() {
                break;
            }
            body.push(self.statement()?);
        }

        Ok(Statement::For {
            variable,
            iterable,
            body,
        })
    }

    fn return_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Return, "Expected RETURN")?;

        let value = if self.check(&TokenKind::Newline) || self.is_at_end() {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume_statement_separator()?;

        Ok(Statement::Return { value })
    }

    fn break_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Break, "Expected BREAK")?;

        let condition = if self.match_token(&TokenKind::If) {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        self.consume_statement_separator()?;

        Ok(Statement::Break { condition })
    }

    fn continue_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Continue, "Expected CONTINUE")?;

        let condition = if self.match_token(&TokenKind::If) {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        self.consume_statement_separator()?;

        Ok(Statement::Continue { condition })
    }

    fn try_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Try, "Expected TRY")?;
        self.consume(TokenKind::Colon, "Expected ':' after TRY")?;
        self.skip_newlines();

        let mut body = Vec::new();
        while !self.check(&TokenKind::Catch) && !self.is_end_of_try_block() && !self.is_at_end() {
            body.push(self.statement()?);
            self.skip_newlines();
        }

        let mut catch_clauses = Vec::new();
        while self.match_token(&TokenKind::Catch) {
            self.advance();

            let error_type = if self.match_token(&TokenKind::Fatal) {
                self.advance();
                Some(ErrorType::Fatal)
            } else if self.match_token(&TokenKind::Recoverable) {
                self.advance();
                Some(ErrorType::Recoverable)
            } else if self.match_token(&TokenKind::Warning) {
                self.advance();
                Some(ErrorType::Warning)
            } else {
                None
            };

            self.consume(TokenKind::Colon, "Expected ':' after CATCH")?;
            self.skip_newlines();

            let mut catch_body = Vec::new();
            while !self.check(&TokenKind::Catch) && !self.is_end_of_try_block() && !self.is_at_end()
            {
                catch_body.push(self.statement()?);
                self.skip_newlines();
            }

            catch_clauses.push(CatchClause {
                error_type,
                body: catch_body,
            });
        }

        Ok(Statement::Try {
            body,
            catch_clauses,
        })
    }

    fn parallel_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Parallel, "Expected PARALLEL")?;
        self.consume(TokenKind::LeftBrace, "Expected '{' after PARALLEL")?;
        self.skip_newlines();

        let mut tasks = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            tasks.push(self.statement()?);
            self.skip_newlines();
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after PARALLEL block")?;
        self.consume_statement_separator()?;

        Ok(Statement::Parallel { tasks })
    }

    fn wait_strategy_statement(&mut self) -> Result<Statement> {
        let strategy = match &self.peek().kind {
            TokenKind::WaitAll => WaitStrategy::WaitAll,
            TokenKind::WaitAny => WaitStrategy::WaitAny,
            TokenKind::Race => WaitStrategy::Race,
            _ => return Err(Error::ParseError("Expected wait strategy".to_string())),
        };
        self.advance();
        self.consume_statement_separator()?;

        Ok(Statement::WaitStrategy(strategy))
    }

    fn decision_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Decision, "Expected DECISION")?;
        self.consume(TokenKind::Colon, "Expected ':' after DECISION")?;

        let description = if let TokenKind::String(s) = &self.peek().kind {
            let desc = s.clone();
            self.advance();
            desc
        } else {
            String::new()
        };

        self.skip_newlines();

        let mut branches = Vec::new();
        while self.match_token(&TokenKind::Branch) {
            self.advance();

            let name = match &self.peek().kind {
                TokenKind::Identifier(n) => n.clone(),
                TokenKind::Constant(n) => n.clone(),
                _ => return Err(Error::ParseError("Expected branch name".to_string())),
            };
            self.advance();

            self.consume(TokenKind::LeftParen, "Expected '(' after branch name")?;
            let condition = self.expression()?;
            self.consume(TokenKind::RightParen, "Expected ')' after condition")?;
            self.consume(TokenKind::Colon, "Expected ':' after branch condition")?;
            self.skip_newlines();

            let mut body = Vec::new();
            while !self.check(&TokenKind::Branch) && !self.is_end_of_block() && !self.is_at_end() {
                body.push(self.statement()?);
                self.skip_newlines();
            }

            branches.push(DecisionBranch {
                name,
                condition,
                body,
            });
        }

        Ok(Statement::Decision {
            description,
            branches,
        })
    }

    fn guard_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Guard, "Expected GUARD")?;

        let condition = self.expression()?;

        self.consume(TokenKind::Else, "Expected ELSE after GUARD condition")?;
        self.skip_newlines();

        // GUARD ELSE body is just a single statement (typically RETURN)
        let stmt = self.statement()?;
        let else_body = vec![stmt];

        Ok(Statement::Guard {
            condition,
            else_body,
        })
    }

    fn expression_statement(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        self.consume_statement_separator()?;
        Ok(Statement::Expression(expr))
    }

    // Expression parsing with precedence

    fn expression(&mut self) -> Result<Expression> {
        self.ternary()
    }

    fn ternary(&mut self) -> Result<Expression> {
        let mut expr = self.or()?;

        if self.match_token(&TokenKind::Question) {
            self.advance();
            let then_expr = self.expression()?;
            self.consume(TokenKind::Colon, "Expected ':' in ternary operator")?;
            let else_expr = self.expression()?;

            expr = Expression::Ternary {
                condition: Box::new(expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            };
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expression> {
        let mut expr = self.and()?;

        while self.match_token(&TokenKind::Or) {
            self.advance();
            let right = self.and()?;
            expr = Expression::Binary {
                op: BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression> {
        let mut expr = self.equality()?;

        while self.match_token(&TokenKind::And) {
            self.advance();
            let right = self.equality()?;
            expr = Expression::Binary {
                op: BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression> {
        let mut expr = self.comparison()?;

        while matches!(self.peek().kind, TokenKind::Eq | TokenKind::NotEq) {
            let op = match self.advance().kind {
                TokenKind::Eq => BinaryOp::Eq,
                TokenKind::NotEq => BinaryOp::NotEq,
                _ => unreachable!(),
            };

            let right = self.comparison()?;
            expr = Expression::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expr = self.term()?;

        while matches!(
            self.peek().kind,
            TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq | TokenKind::In
        ) {
            let op = match self.advance().kind {
                TokenKind::Lt => BinaryOp::Lt,
                TokenKind::Gt => BinaryOp::Gt,
                TokenKind::LtEq => BinaryOp::LtEq,
                TokenKind::GtEq => BinaryOp::GtEq,
                TokenKind::In => BinaryOp::In,
                _ => unreachable!(),
            };

            let right = self.term()?;
            expr = Expression::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expr = self.factor()?;

        while matches!(self.peek().kind, TokenKind::Plus | TokenKind::Minus) {
            let op = match self.advance().kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            let right = self.factor()?;
            expr = Expression::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;

        while matches!(
            self.peek().kind,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent
        ) {
            let op = match self.advance().kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => unreachable!(),
            };

            let right = self.unary()?;
            expr = Expression::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression> {
        if matches!(self.peek().kind, TokenKind::Not | TokenKind::Minus) {
            let op = match self.advance().kind {
                TokenKind::Not => UnaryOp::Not,
                TokenKind::Minus => UnaryOp::Neg,
                _ => unreachable!(),
            };

            let operand = self.unary()?;
            return Ok(Expression::Unary {
                op,
                operand: Box::new(operand),
            });
        }

        self.power()
    }

    fn power(&mut self) -> Result<Expression> {
        let mut expr = self.call()?;

        if self.match_token(&TokenKind::StarStar) {
            self.advance();
            let right = self.power()?; // Right associative
            expr = Expression::Binary {
                op: BinaryOp::Pow,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn call(&mut self) -> Result<Expression> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&TokenKind::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.match_token(&TokenKind::Dot) {
                self.advance();
                let field = match &self.peek().kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => {
                        return Err(Error::ParseError(
                            "Expected field name after '.'".to_string(),
                        ))
                    }
                };
                self.advance();

                expr = Expression::FieldAccess {
                    object: Box::new(expr),
                    field,
                };
            } else if self.match_token(&TokenKind::LeftBracket) {
                self.advance();
                let index = self.expression()?;
                self.consume(TokenKind::RightBracket, "Expected ']' after index")?;

                expr = Expression::IndexAccess {
                    array: Box::new(expr),
                    index: Box::new(index),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression> {
        self.consume(TokenKind::LeftParen, "Expected '('")?;

        let name = match callee {
            Expression::Variable(n) => n,
            _ => {
                return Err(Error::ParseError(
                    "Can only call named functions/tools".to_string(),
                ))
            }
        };

        let mut args = Vec::new();

        if !self.check(&TokenKind::RightParen) {
            loop {
                // Check for named argument
                if let TokenKind::Identifier(param_name) = &self.peek().kind.clone() {
                    let next_idx = self.current + 1;
                    if next_idx < self.tokens.len()
                        && matches!(self.tokens[next_idx].kind, TokenKind::Colon)
                    {
                        let param_name = param_name.clone();
                        self.advance(); // consume identifier
                        self.advance(); // consume colon
                        let value = self.expression()?;
                        args.push(Argument::named(param_name, value));
                    } else {
                        // Positional
                        let value = self.expression()?;
                        args.push(Argument::positional(value));
                    }
                } else {
                    // Positional
                    let value = self.expression()?;
                    args.push(Argument::positional(value));
                }

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;

        Ok(Expression::ToolCall { name, args })
    }

    fn primary(&mut self) -> Result<Expression> {
        let token = self.peek().clone();

        match &token.kind {
            TokenKind::True => {
                self.advance();
                Ok(Expression::BoolLiteral(true))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expression::BoolLiteral(false))
            }
            TokenKind::Null => {
                self.advance();
                Ok(Expression::NullLiteral)
            }
            TokenKind::Integer(n) => {
                let val = *n;
                self.advance();
                Ok(Expression::IntLiteral(val))
            }
            TokenKind::Float(f) => {
                let val = *f;
                self.advance();
                Ok(Expression::FloatLiteral(val))
            }
            TokenKind::String(s) => {
                let val = s.clone();
                self.advance();
                Ok(Expression::StringLiteral(val))
            }
            TokenKind::Variable(name) => {
                let var_name = name.clone();
                self.advance();
                Ok(Expression::Variable(var_name))
            }
            TokenKind::Identifier(name) | TokenKind::Constant(name) => {
                let func_name = name.clone();
                self.advance();
                Ok(Expression::Variable(func_name))
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenKind::RightParen, "Expected ')' after expression")?;
                Ok(Expression::Grouping(Box::new(expr)))
            }
            TokenKind::LeftBracket => self.array_literal(),
            TokenKind::LeftBrace => self.object_literal(),
            _ => Err(Error::UnexpectedToken {
                expected: "expression".to_string(),
                got: format!("{:?}", token.kind),
            }),
        }
    }

    fn array_literal(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftBracket, "Expected '['")?;

        let mut elements = Vec::new();

        if !self.check(&TokenKind::RightBracket) {
            // Check for range syntax: [1..10]
            let first = self.expression()?;

            if self.match_token(&TokenKind::DotDot) {
                self.advance();
                let end = self.expression()?;
                self.consume(TokenKind::RightBracket, "Expected ']'")?;
                return Ok(Expression::Range {
                    start: Box::new(first),
                    end: Box::new(end),
                });
            }

            elements.push(first);

            while self.match_token(&TokenKind::Comma) {
                self.advance();
                if self.check(&TokenKind::RightBracket) {
                    break;
                }
                elements.push(self.expression()?);
            }
        }

        self.consume(TokenKind::RightBracket, "Expected ']'")?;
        Ok(Expression::ArrayLiteral(elements))
    }

    fn object_literal(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftBrace, "Expected '{'")?;

        let mut fields = Vec::new();

        if !self.check(&TokenKind::RightBrace) {
            loop {
                let key = match &self.peek().kind {
                    TokenKind::Identifier(k) => k.clone(),
                    TokenKind::String(k) => k.clone(),
                    _ => return Err(Error::ParseError("Expected field name".to_string())),
                };
                self.advance();

                self.consume(TokenKind::Colon, "Expected ':' after field name")?;
                let value = self.expression()?;

                fields.push((key, value));

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
                self.advance();

                if self.check(&TokenKind::RightBrace) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightBrace, "Expected '}'")?;
        Ok(Expression::ObjectLiteral(fields))
    }

    // Helper methods

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
    }

    fn match_token(&self, kind: &TokenKind) -> bool {
        self.check(kind)
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<&Token> {
        if self.check(&kind) {
            Ok(self.advance())
        } else {
            Err(Error::UnexpectedToken {
                expected: message.to_string(),
                got: format!("{:?}", self.peek().kind),
            })
        }
    }

    fn skip_newlines(&mut self) {
        while self.match_token(&TokenKind::Newline) {
            self.advance();
        }
    }

    fn consume_statement_separator(&mut self) -> Result<()> {
        if self.match_token(&TokenKind::Newline) || self.match_token(&TokenKind::Semicolon) {
            self.advance();
            Ok(())
        } else if self.is_at_end() {
            Ok(())
        } else {
            // Allow missing separator in some cases
            Ok(())
        }
    }

    fn is_end_of_block(&self) -> bool {
        // Check if we're at a keyword that ends the current block (for IF/TRY/etc.)
        // These are keywords that belong to a parent construct (ELSE, CATCH, BRANCH)
        // or explicitly close blocks (RightBrace, wait strategies)
        //
        // NOTE: IF, FOR, WHILE, RETURN, BREAK, CONTINUE are NOT block terminators!
        // They are control flow statements that can appear within blocks.
        matches!(
            self.peek().kind,
            TokenKind::Else
                | TokenKind::Branch
                | TokenKind::Catch
                | TokenKind::RightBrace
                | TokenKind::WaitAll
                | TokenKind::WaitAny
                | TokenKind::Race
        )
    }

    fn is_end_of_loop_block(&self) -> bool {
        // Check if we're at a keyword that ends a loop block (FOR/WHILE)
        // Loops terminate on the same keywords as other blocks, PLUS:
        // - RETURN (which exits the function, ending the loop)
        // This is because OVSM has no explicit block delimiters, so RETURN
        // serves as a signal that the loop body has ended and we're back
        // at the top level of the function/script.
        self.is_end_of_block() || matches!(self.peek().kind, TokenKind::Return)
    }

    fn is_end_of_try_block(&self) -> bool {
        // TRY/CATCH blocks need special handling like loops
        // They terminate on the same keywords as loops
        self.is_end_of_loop_block()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;

    fn parse_expr(source: &str) -> Result<Expression> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        parser.expression()
    }

    fn parse_stmt(source: &str) -> Result<Statement> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        parser.statement()
    }

    #[test]
    fn test_parse_integer() {
        let expr = parse_expr("42").unwrap();
        assert_eq!(expr, Expression::IntLiteral(42));
    }

    #[test]
    fn test_parse_variable() {
        let expr = parse_expr("$x").unwrap();
        assert_eq!(expr, Expression::Variable("x".to_string()));
    }

    #[test]
    fn test_parse_binary_add() {
        let expr = parse_expr("1 + 2").unwrap();
        match expr {
            Expression::Binary { op, .. } => assert_eq!(op, BinaryOp::Add),
            _ => panic!("Expected Binary expression"),
        }
    }

    #[test]
    fn test_operator_precedence() {
        let expr = parse_expr("2 + 3 * 4").unwrap();
        // Should parse as: 2 + (3 * 4)
        match expr {
            Expression::Binary {
                op: BinaryOp::Add,
                left,
                right,
            } => {
                assert_eq!(*left, Expression::IntLiteral(2));
                match *right {
                    Expression::Binary {
                        op: BinaryOp::Mul, ..
                    } => {}
                    _ => panic!("Expected multiplication as right operand"),
                }
            }
            _ => panic!("Expected addition at top level"),
        }
    }

    #[test]
    fn test_parse_assignment() {
        let stmt = parse_stmt("$x = 42").unwrap();
        match stmt {
            Statement::Assignment { name, value } => {
                assert_eq!(name, "x");
                assert_eq!(value, Expression::IntLiteral(42));
            }
            _ => panic!("Expected Assignment statement"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let expr = parse_expr("getSlot()").unwrap();
        match expr {
            Expression::ToolCall { name, args } => {
                assert_eq!(name, "getSlot");
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected ToolCall expression"),
        }
    }

    #[test]
    fn test_parse_function_call_with_args() {
        let expr = parse_expr("ADD(10, 20)").unwrap();
        match expr {
            Expression::ToolCall { name, args } => {
                assert_eq!(name, "ADD");
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected ToolCall expression"),
        }
    }

    #[test]
    fn test_parse_array_literal() {
        let expr = parse_expr("[1, 2, 3]").unwrap();
        match expr {
            Expression::ArrayLiteral(elements) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected ArrayLiteral"),
        }
    }

    #[test]
    fn test_parse_range() {
        let expr = parse_expr("[1..10]").unwrap();
        match expr {
            Expression::Range { .. } => {}
            _ => panic!("Expected Range expression"),
        }
    }
}
