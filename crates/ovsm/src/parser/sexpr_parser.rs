use super::ast::{Argument, Expression, Program, ProgramMetadata, Statement, BinaryOp};
use crate::error::{Error, Result};
use crate::lexer::{Token, TokenKind};

/// S-expression parser for LISP-style OVSM syntax
pub struct SExprParser {
    tokens: Vec<Token>,
    current: usize,
}

impl SExprParser {
    /// Creates a new S-expression parser
    pub fn new(tokens: Vec<Token>) -> Self {
        SExprParser { tokens, current: 0 }
    }

    /// Parses the tokens into an AST
    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        Ok(Program {
            metadata: ProgramMetadata::default(),
            statements,
        })
    }

    /// Parse a single statement (which is an S-expression)
    fn parse_statement(&mut self) -> Result<Statement> {
        let expr = self.parse_expression()?;

        // Convert expression to statement
        // Most expressions can be statements directly
        Ok(Statement::Expression(expr))
    }

    /// Parse an expression (which is the core of S-expression parsing)
    fn parse_expression(&mut self) -> Result<Expression> {
        match self.peek().kind {
            TokenKind::LeftParen => self.parse_list(),
            TokenKind::Quote => self.parse_quoted(),
            TokenKind::Backtick => self.parse_quasiquote(),
            TokenKind::Comma => self.parse_unquote(),
            TokenKind::CommaAt => self.parse_unquote_splice(),
            TokenKind::Integer(n) => {
                self.advance();
                Ok(Expression::IntLiteral(n))
            }
            TokenKind::Float(f) => {
                self.advance();
                Ok(Expression::FloatLiteral(f))
            }
            TokenKind::String(ref s) => {
                let s = s.clone();
                self.advance();
                Ok(Expression::StringLiteral(s))
            }
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
            TokenKind::Identifier(ref name) => {
                let name = name.clone();
                self.advance();
                Ok(Expression::Variable(name))
            }
            TokenKind::LeftBracket => self.parse_array_literal(),
            TokenKind::LeftBrace => self.parse_object_literal(),
            _ => Err(Error::ParseError(format!(
                "Unexpected token {:?} at line {}",
                self.peek().kind,
                self.peek().line
            ))),
        }
    }

    /// Parse a list (the core S-expression form)
    fn parse_list(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftParen)?;

        // Empty list
        if self.check(&TokenKind::RightParen) {
            self.advance();
            return Ok(Expression::ArrayLiteral(Vec::new()));
        }

        // The first element determines what kind of form this is
        let first = self.peek();

        match &first.kind {
            // Special forms (keywords)
            TokenKind::Identifier(name) if name == "if" => self.parse_if_expr(),
            TokenKind::Identifier(name) if name == "let" => self.parse_let_expr(),
            TokenKind::Identifier(name) if name == "let*" => self.parse_let_star_expr(),
            TokenKind::Identifier(name) if name == "flet" => self.parse_flet_expr(),
            TokenKind::Identifier(name) if name == "const" => self.parse_const(),
            TokenKind::Identifier(name) if name == "define" => self.parse_define(),
            TokenKind::Identifier(name) if name == "set!" => self.parse_set(),
            TokenKind::Identifier(name) if name == "while" => self.parse_while(),
            TokenKind::Identifier(name) if name == "for" => self.parse_for(),
            TokenKind::Identifier(name) if name == "lambda" => self.parse_lambda(),
            TokenKind::Identifier(name) if name == "do" => self.parse_do(),
            TokenKind::Identifier(name) if name == "when" => self.parse_when(),
            TokenKind::Identifier(name) if name == "cond" => self.parse_cond(),

            // Special accessor forms
            TokenKind::Dot => self.parse_field_access(),
            TokenKind::LeftBracket => self.parse_index_access(),

            // Operators
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::LtEq
            | TokenKind::GtEq
            | TokenKind::And
            | TokenKind::Or => self.parse_operator_call(),

            // Identifier (function call or variable)
            TokenKind::Identifier(_) => self.parse_function_call(),

            _ => Err(Error::ParseError(format!(
                "Unexpected form starting with {:?}",
                first.kind
            ))),
        }
    }

    /// Parse a quoted expression '(1 2 3)
    fn parse_quoted(&mut self) -> Result<Expression> {
        self.consume(TokenKind::Quote)?;
        let expr = self.parse_expression()?;

        // Convert to a quoted list
        // For now, just return the expression as-is
        // In a full LISP implementation, we'd wrap this in a Quote expression
        Ok(expr)
    }

    /// Parse (if condition then-expr else-expr)
    fn parse_if_expr(&mut self) -> Result<Expression> {
        self.advance(); // consume 'if'

        let condition = Box::new(self.parse_expression()?);
        let then_expr = Box::new(self.parse_expression()?);
        let else_expr = Box::new(self.parse_expression()?);

        self.consume(TokenKind::RightParen)?;

        Ok(Expression::Ternary {
            condition,
            then_expr,
            else_expr,
        })
    }

    /// Parse (let ((x 10) (y 20)) body...)
    fn parse_let_expr(&mut self) -> Result<Expression> {
        self.advance(); // consume 'let'

        // Parse bindings list
        self.consume(TokenKind::LeftParen)?;
        let mut bindings = Vec::new();

        while !self.check(&TokenKind::RightParen) {
            self.consume(TokenKind::LeftParen)?;

            let var_name = if let TokenKind::Identifier(name) = &self.peek().kind {
                name.clone()
            } else {
                return Err(Error::ParseError("Expected identifier in let binding".to_string()));
            };
            self.advance();

            let value = self.parse_expression()?;
            bindings.push((var_name, value));

            self.consume(TokenKind::RightParen)?;
        }
        self.consume(TokenKind::RightParen)?; // close bindings list

        // Parse body expressions
        let mut body_exprs = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            body_exprs.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        // Convert bindings to an ArrayLiteral of pairs
        let binding_pairs: Vec<Expression> = bindings.into_iter()
            .map(|(name, expr)| {
                Expression::ArrayLiteral(vec![
                    Expression::Variable(name),
                    expr
                ])
            })
            .collect();

        let mut args = vec![
            Argument::positional(Expression::ArrayLiteral(binding_pairs))
        ];

        // Add body expressions as arguments
        for body_expr in body_exprs {
            args.push(Argument::positional(body_expr));
        }

        Ok(Expression::ToolCall {
            name: "let".to_string(),
            args,
        })
    }

    /// Parse (let* ((var val)...) body) - Sequential binding version of let
    fn parse_let_star_expr(&mut self) -> Result<Expression> {
        self.advance(); // consume 'let*'

        // Parse bindings list (same as let)
        self.consume(TokenKind::LeftParen)?;
        let mut bindings = Vec::new();

        while !self.check(&TokenKind::RightParen) {
            self.consume(TokenKind::LeftParen)?;

            let var_name = if let TokenKind::Identifier(name) = &self.peek().kind {
                name.clone()
            } else {
                return Err(Error::ParseError("Expected identifier in let* binding".to_string()));
            };
            self.advance();

            let value = self.parse_expression()?;
            bindings.push((var_name, value));

            self.consume(TokenKind::RightParen)?;
        }
        self.consume(TokenKind::RightParen)?; // close bindings list

        // Parse body expressions (same as let)
        let mut body_exprs = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            body_exprs.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        // Convert bindings to an ArrayLiteral of pairs
        let binding_pairs: Vec<Expression> = bindings.into_iter()
            .map(|(name, expr)| {
                Expression::ArrayLiteral(vec![
                    Expression::Variable(name),
                    expr
                ])
            })
            .collect();

        let mut args = vec![
            Argument::positional(Expression::ArrayLiteral(binding_pairs))
        ];

        // Add body expressions as arguments
        for body_expr in body_exprs {
            args.push(Argument::positional(body_expr));
        }

        Ok(Expression::ToolCall {
            name: "let*".to_string(),
            args,
        })
    }

    /// Parse (flet ((name (params) body)...) body) - Local function definitions
    fn parse_flet_expr(&mut self) -> Result<Expression> {
        self.advance(); // consume 'flet'

        // Parse function definitions list
        self.consume(TokenKind::LeftParen)?;
        let mut func_defs = Vec::new();

        while !self.check(&TokenKind::RightParen) {
            // Each function definition: (name (params) body)
            self.consume(TokenKind::LeftParen)?;

            // Parse name
            let name = if let TokenKind::Identifier(n) = &self.peek().kind {
                n.clone()
            } else {
                return Err(Error::ParseError("Expected function name in flet".to_string()));
            };
            self.advance();

            // Parse parameters
            let params = self.parse_expression()?;

            // Parse body
            let body = self.parse_expression()?;

            self.consume(TokenKind::RightParen)?;

            // Create function definition as array: [name, params, body]
            func_defs.push(Expression::ArrayLiteral(vec![
                Expression::Variable(name),
                params,
                body,
            ]));
        }
        self.consume(TokenKind::RightParen)?; // close function definitions list

        // Parse body expressions
        let mut body_exprs = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            body_exprs.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        let mut args = vec![
            Argument::positional(Expression::ArrayLiteral(func_defs))
        ];

        // Add body expressions as arguments
        for body_expr in body_exprs {
            args.push(Argument::positional(body_expr));
        }

        Ok(Expression::ToolCall {
            name: "flet".to_string(),
            args,
        })
    }

    /// Parse (const NAME value)
    fn parse_const(&mut self) -> Result<Expression> {
        self.advance(); // consume 'const'

        let name = if let TokenKind::Identifier(n) = &self.peek().kind {
            n.clone()
        } else {
            return Err(Error::ParseError("Expected identifier after const".to_string()));
        };
        self.advance();

        let value = self.parse_expression()?;
        self.consume(TokenKind::RightParen)?;

        // Return as a tool call for now
        Ok(Expression::ToolCall {
            name: "const".to_string(),
            args: vec![
                Argument::positional(Expression::Variable(name)),
                Argument::positional(value),
            ],
        })
    }

    /// Parse (define name value)
    fn parse_define(&mut self) -> Result<Expression> {
        self.advance(); // consume 'define'

        let name = if let TokenKind::Identifier(n) = &self.peek().kind {
            n.clone()
        } else {
            return Err(Error::ParseError("Expected identifier after define".to_string()));
        };
        self.advance();

        let value = self.parse_expression()?;
        self.consume(TokenKind::RightParen)?;

        Ok(Expression::ToolCall {
            name: "define".to_string(),
            args: vec![
                Argument::positional(Expression::Variable(name)),
                Argument::positional(value),
            ],
        })
    }

    /// Parse (set! name value)
    fn parse_set(&mut self) -> Result<Expression> {
        self.advance(); // consume 'set!'

        let name = if let TokenKind::Identifier(n) = &self.peek().kind {
            n.clone()
        } else {
            return Err(Error::ParseError("Expected identifier after set!".to_string()));
        };
        self.advance();

        let value = self.parse_expression()?;
        self.consume(TokenKind::RightParen)?;

        Ok(Expression::ToolCall {
            name: "set!".to_string(),
            args: vec![
                Argument::positional(Expression::Variable(name)),
                Argument::positional(value),
            ],
        })
    }

    /// Parse (while condition body...)
    fn parse_while(&mut self) -> Result<Expression> {
        self.advance(); // consume 'while'

        let condition = self.parse_expression()?;

        let mut body = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            body.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        // Build args list: [condition, ...body_expressions]
        let mut args = vec![Argument::positional(condition)];
        for expr in body {
            args.push(Argument::positional(expr));
        }

        Ok(Expression::ToolCall {
            name: "while".to_string(),
            args,
        })
    }

    /// Parse (for (var collection) body...)
    fn parse_for(&mut self) -> Result<Expression> {
        self.advance(); // consume 'for'

        self.consume(TokenKind::LeftParen)?;

        let var_name = if let TokenKind::Identifier(n) = &self.peek().kind {
            n.clone()
        } else {
            return Err(Error::ParseError("Expected identifier in for loop".to_string()));
        };
        self.advance();

        let collection = self.parse_expression()?;
        self.consume(TokenKind::RightParen)?;

        let mut body = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            body.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        // Build args list: [variable, collection, ...body_expressions]
        let mut args = vec![
            Argument::positional(Expression::Variable(var_name)),
            Argument::positional(collection),
        ];
        for expr in body {
            args.push(Argument::positional(expr));
        }

        Ok(Expression::ToolCall {
            name: "for".to_string(),
            args,
        })
    }

    /// Parse (lambda (params...) body)
    fn parse_lambda(&mut self) -> Result<Expression> {
        self.advance(); // consume 'lambda'

        // Parse parameters
        self.consume(TokenKind::LeftParen)?;
        let mut params = Vec::new();

        while !self.check(&TokenKind::RightParen) {
            if let TokenKind::Identifier(name) = &self.peek().kind {
                params.push(name.clone());
                self.advance();
            } else {
                return Err(Error::ParseError("Expected identifier in lambda params".to_string()));
            }
        }
        self.consume(TokenKind::RightParen)?;

        // Parse body
        let body = Box::new(self.parse_expression()?);
        self.consume(TokenKind::RightParen)?;

        Ok(Expression::Lambda { params, body })
    }

    /// Parse (do expr1 expr2 ... exprN) - returns last expression
    fn parse_do(&mut self) -> Result<Expression> {
        self.advance(); // consume 'do'

        let mut exprs = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            exprs.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        // Return the last expression, or null if empty
        Ok(exprs.last().cloned().unwrap_or(Expression::NullLiteral))
    }

    /// Parse (when condition body...)
    fn parse_when(&mut self) -> Result<Expression> {
        self.advance(); // consume 'when'

        let condition = Box::new(self.parse_expression()?);

        let mut body = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            body.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        // Convert to if-then-null
        let then_expr = Box::new(body.last().cloned().unwrap_or(Expression::NullLiteral));
        let else_expr = Box::new(Expression::NullLiteral);

        Ok(Expression::Ternary {
            condition,
            then_expr,
            else_expr,
        })
    }

    /// Parse (cond (test result)... (else result))
    fn parse_cond(&mut self) -> Result<Expression> {
        self.advance(); // consume 'cond'

        let mut clauses = Vec::new();

        while !self.check(&TokenKind::RightParen) {
            self.consume(TokenKind::LeftParen)?;

            // Check for 'else' clause
            let is_else = if let TokenKind::Identifier(name) = &self.peek().kind {
                name == "else"
            } else {
                false
            };

            if is_else {
                self.advance(); // consume 'else'
                let result = self.parse_expression()?;
                self.consume(TokenKind::RightParen)?;

                // else clause - always true condition
                clauses.push((Expression::BoolLiteral(true), result));
                break;
            } else {
                // Regular test clause - parse both test and result
                let test = self.parse_expression()?;
                let result = self.parse_expression()?;
                self.consume(TokenKind::RightParen)?;

                clauses.push((test, result));
            }
        }
        self.consume(TokenKind::RightParen)?;

        // Convert clauses into nested ternary expressions
        let mut result_expr = Expression::NullLiteral;
        for (test, result) in clauses.into_iter().rev() {
            result_expr = Expression::Ternary {
                condition: Box::new(test),
                then_expr: Box::new(result),
                else_expr: Box::new(result_expr),
            };
        }

        Ok(result_expr)
    }

    /// Parse operator call like (+ 1 2 3)
    fn parse_operator_call(&mut self) -> Result<Expression> {
        let op_token = self.advance();
        let op = self.token_to_binary_op(&op_token)?;

        let mut operands = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            operands.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        if operands.is_empty() {
            return Err(Error::ParseError("Operator requires at least one operand".to_string()));
        }

        // For variadic operators like +, *, and, or - chain them
        if operands.len() == 1 {
            return Ok(operands[0].clone());
        }

        let mut result = operands[0].clone();
        for operand in operands.iter().skip(1) {
            result = Expression::Binary {
                op,
                left: Box::new(result),
                right: Box::new(operand.clone()),
            };
        }

        Ok(result)
    }

    /// Parse function call (func arg1 arg2 :key val)
    fn parse_function_call(&mut self) -> Result<Expression> {
        let name = if let TokenKind::Identifier(n) = &self.peek().kind {
            n.clone()
        } else {
            return Err(Error::ParseError("Expected function name".to_string()));
        };
        self.advance();

        let mut args = Vec::new();
        while !self.check(&TokenKind::RightParen) {
            // Check for keyword argument
            if self.check(&TokenKind::Colon) {
                self.advance(); // consume :
                let key = if let TokenKind::Identifier(k) = &self.peek().kind {
                    k.clone()
                } else {
                    return Err(Error::ParseError("Expected identifier after :".to_string()));
                };
                self.advance();

                let value = self.parse_expression()?;
                args.push(Argument::named(key, value));
            } else {
                // Positional argument
                let value = self.parse_expression()?;
                args.push(Argument::positional(value));
            }
        }
        self.consume(TokenKind::RightParen)?;

        Ok(Expression::ToolCall { name, args })
    }

    /// Parse array literal [1, 2, 3]
    fn parse_array_literal(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftBracket)?;

        let mut elements = Vec::new();
        while !self.check(&TokenKind::RightBracket) {
            elements.push(self.parse_expression()?);

            if self.check(&TokenKind::Comma) {
                self.advance();
            }
        }
        self.consume(TokenKind::RightBracket)?;

        Ok(Expression::ArrayLiteral(elements))
    }

    /// Parse object literal {:key value}
    fn parse_object_literal(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftBrace)?;

        let mut pairs = Vec::new();
        while !self.check(&TokenKind::RightBrace) {
            // Check for two syntaxes:
            // 1. :key value (explicit syntax)
            // 2. identifier (shorthand - expands to :identifier identifier)

            if self.check(&TokenKind::Colon) {
                // Explicit syntax: :key value
                self.advance(); // consume colon
                let key = if let TokenKind::Identifier(k) = &self.peek().kind {
                    k.clone()
                } else {
                    return Err(Error::ParseError("Expected identifier for object key".to_string()));
                };
                self.advance();

                let value = self.parse_expression()?;
                pairs.push((key, value));
            } else if let TokenKind::Identifier(name) = &self.peek().kind {
                // Shorthand syntax: identifier expands to :identifier identifier
                let key = name.clone();
                self.advance();

                // Value is a variable reference with the same name
                let value = Expression::Variable(key.clone());
                pairs.push((key, value));
            } else {
                return Err(Error::ParseError(format!(
                    "Expected ':key value' or 'identifier' in object literal, got {:?}",
                    self.peek().kind
                )));
            }

            if self.check(&TokenKind::Comma) {
                self.advance();
            }
        }
        self.consume(TokenKind::RightBrace)?;

        Ok(Expression::ObjectLiteral(pairs))
    }

    /// Convert token to binary operator
    fn token_to_binary_op(&self, token: &Token) -> Result<BinaryOp> {
        match &token.kind {
            TokenKind::Plus => Ok(BinaryOp::Add),
            TokenKind::Minus => Ok(BinaryOp::Sub),
            TokenKind::Star => Ok(BinaryOp::Mul),
            TokenKind::Slash => Ok(BinaryOp::Div),
            TokenKind::Percent => Ok(BinaryOp::Mod),
            TokenKind::Eq => Ok(BinaryOp::Eq),
            TokenKind::NotEq => Ok(BinaryOp::NotEq),
            TokenKind::Lt => Ok(BinaryOp::Lt),
            TokenKind::Gt => Ok(BinaryOp::Gt),
            TokenKind::LtEq => Ok(BinaryOp::LtEq),
            TokenKind::GtEq => Ok(BinaryOp::GtEq),
            TokenKind::And => Ok(BinaryOp::And),
            TokenKind::Or => Ok(BinaryOp::Or),
            _ => Err(Error::ParseError(format!("Not a binary operator: {:?}", token.kind))),
        }
    }

    /// Parse (. object field) - field access expression
    fn parse_field_access(&mut self) -> Result<Expression> {
        self.advance(); // consume '.'

        // Parse object expression
        let object = Box::new(self.parse_expression()?);

        // Parse field name (must be identifier)
        let field = if let TokenKind::Identifier(name) = &self.peek().kind {
            name.clone()
        } else {
            return Err(Error::ParseError(format!(
                "Expected field name after '.', got {:?}",
                self.peek().kind
            )));
        };
        self.advance();

        self.consume(TokenKind::RightParen)?;

        Ok(Expression::FieldAccess { object, field })
    }

    /// Parse ([] array index) - index access expression
    /// Syntax: ([] array-expr index-expr)
    fn parse_index_access(&mut self) -> Result<Expression> {
        self.advance(); // consume '['

        // The next token MUST be ']' to form the '[]' operator
        if !self.check(&TokenKind::RightBracket) {
            return Err(Error::ParseError(format!(
                "Expected ']' after '[' to form index operator, got {:?}",
                self.peek().kind
            )));
        }
        self.advance(); // consume ']'

        // Now parse the array expression
        let array = Box::new(self.parse_expression()?);

        // Parse the index expression
        let index = Box::new(self.parse_expression()?);

        // Consume closing paren of the list
        self.consume(TokenKind::RightParen)?;

        Ok(Expression::IndexAccess { array, index })
    }

    // Helper methods

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens[self.current - 1].clone()
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token> {
        if self.check(&kind) {
            Ok(self.advance())
        } else {
            Err(Error::ParseError(format!(
                "Expected {:?}, got {:?} at line {}",
                kind,
                self.peek().kind,
                self.peek().line
            )))
        }
    }

    /// Parse a quasiquoted expression `(...)
    /// Used in macros for code templates
    fn parse_quasiquote(&mut self) -> Result<Expression> {
        self.consume(TokenKind::Backtick)?;
        let expr = self.parse_expression()?;
        Ok(Expression::Quasiquote(Box::new(expr)))
    }

    /// Parse an unquote expression ,(...)
    /// Evaluates expression inside quasiquote
    fn parse_unquote(&mut self) -> Result<Expression> {
        self.consume(TokenKind::Comma)?;
        let expr = self.parse_expression()?;
        Ok(Expression::Unquote(Box::new(expr)))
    }

    /// Parse an unquote-splice expression ,@(...)
    /// Evaluates and splices list elements into quasiquote
    fn parse_unquote_splice(&mut self) -> Result<Expression> {
        self.consume(TokenKind::CommaAt)?;
        let expr = self.parse_expression()?;
        Ok(Expression::UnquoteSplice(Box::new(expr)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::SExprScanner;

    fn parse_str(source: &str) -> Result<Program> {
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = SExprParser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_simple_arithmetic() {
        let program = parse_str("(+ 1 2)").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_if_expression() {
        let program = parse_str("(if (== x 0) true false)").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_nested_expressions() {
        let program = parse_str("(+ (* 2 3) (- 10 5))").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_function_call_with_keywords() {
        let program = parse_str("(log :message \"hello\" :level 1)").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_lambda() {
        let program = parse_str("(lambda (x y) (+ x y))").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_if_in_while_no_ambiguity() {
        // This is the critical test case that was buggy in Python-style syntax
        // S-expressions make it unambiguous - no indentation parsing needed!
        let source = r#"
(while (not done)
  (if (== count 0)
      (set! x 1)
      (set! x 2))
  (set! done true))
"#;
        let program = parse_str(source).unwrap();
        assert_eq!(program.statements.len(), 1);

        // The parser should successfully parse this without any ambiguity
        // because parentheses explicitly delimit all blocks
    }

    #[test]
    fn test_nested_if_in_while() {
        let source = r#"
(while (not done)
  (if (> x 0)
      (if (< x 10)
          (set! result "middle")
          (set! result "high"))
      (set! result "low"))
  (set! done true))
"#;
        let program = parse_str(source).unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_multiple_statements_in_while() {
        let source = r#"
(while (< count 10)
  (log :message count)
  (if (== (% count 2) 0)
      (log :message "even")
      (log :message "odd"))
  (set! count (+ count 1)))
"#;
        let program = parse_str(source).unwrap();
        assert_eq!(program.statements.len(), 1);
    }
}
