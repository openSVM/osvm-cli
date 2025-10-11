use crate::error::{Error, Result};
use crate::parser::{BinaryOp, Expression, Program, Statement, UnaryOp, Argument};
use crate::runtime::{Environment, Value};
use crate::tools::ToolRegistry;
use std::sync::Arc;

/// Evaluator for OVSM programs
pub struct Evaluator {
    env: Environment,
    registry: Arc<ToolRegistry>,
}

/// Execution flow control
enum ExecutionFlow {
    Continue,
    Break,
    ContinueLoop,
    Return(Value),
}

impl Evaluator {
    /// Create new evaluator with default tool registry
    pub fn new() -> Self {
        Evaluator {
            env: Environment::new(),
            registry: Arc::new(ToolRegistry::new()),
        }
    }

    /// Create evaluator with custom tool registry
    pub fn with_registry(registry: ToolRegistry) -> Self {
        Evaluator {
            env: Environment::new(),
            registry: Arc::new(registry),
        }
    }

    /// Execute a program
    pub fn execute(&mut self, program: &Program) -> Result<Value> {
        for stmt in &program.statements {
            match self.execute_statement(stmt)? {
                ExecutionFlow::Continue => {}
                ExecutionFlow::Return(val) => return Ok(val),
                ExecutionFlow::Break => {
                    return Err(Error::InvalidBreak);
                }
                ExecutionFlow::ContinueLoop => {
                    return Err(Error::InvalidContinue);
                }
            }
        }

        Ok(Value::Null)
    }

    // Statement execution

    fn execute_statement(&mut self, stmt: &Statement) -> Result<ExecutionFlow> {
        match stmt {
            Statement::Assignment { name, value } => {
                let val = self.evaluate_expression(value)?;
                // Use set() instead of define() to update existing variables
                // or create in current scope if new
                self.env.set(name, val)?;
                Ok(ExecutionFlow::Continue)
            }

            Statement::ConstantDef { name, value } => {
                let val = self.evaluate_expression(value)?;
                self.env.define_constant(name.clone(), val)?;
                Ok(ExecutionFlow::Continue)
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.evaluate_expression(condition)?;

                if cond_val.is_truthy() {
                    self.execute_block(then_branch)
                } else if let Some(else_b) = else_branch {
                    self.execute_block(else_b)
                } else {
                    Ok(ExecutionFlow::Continue)
                }
            }

            Statement::While { condition, body } => {
                loop {
                    let cond_val = self.evaluate_expression(condition)?;
                    if !cond_val.is_truthy() {
                        break;
                    }

                    match self.execute_block(body)? {
                        ExecutionFlow::Continue => {}
                        ExecutionFlow::Break => break,
                        ExecutionFlow::ContinueLoop => continue,
                        ExecutionFlow::Return(val) => return Ok(ExecutionFlow::Return(val)),
                    }
                }
                Ok(ExecutionFlow::Continue)
            }

            Statement::For {
                variable,
                iterable,
                body,
            } => {
                let iter_val = self.evaluate_expression(iterable)?;
                let items = self.get_iterable_items(&iter_val)?;

                self.env.enter_scope();

                for item in items {
                    self.env.define(variable.clone(), item);

                    match self.execute_block(body)? {
                        ExecutionFlow::Continue => {}
                        ExecutionFlow::Break => break,
                        ExecutionFlow::ContinueLoop => continue,
                        ExecutionFlow::Return(val) => {
                            self.env.exit_scope();
                            return Ok(ExecutionFlow::Return(val));
                        }
                    }
                }

                self.env.exit_scope();
                Ok(ExecutionFlow::Continue)
            }

            Statement::Return { value } => {
                let val = if let Some(v) = value {
                    self.evaluate_expression(v)?
                } else {
                    Value::Null
                };
                Ok(ExecutionFlow::Return(val))
            }

            Statement::Break { condition } => {
                if let Some(cond) = condition {
                    let cond_val = self.evaluate_expression(cond)?;
                    if cond_val.is_truthy() {
                        Ok(ExecutionFlow::Break)
                    } else {
                        Ok(ExecutionFlow::Continue)
                    }
                } else {
                    Ok(ExecutionFlow::Break)
                }
            }

            Statement::Continue { condition } => {
                if let Some(cond) = condition {
                    let cond_val = self.evaluate_expression(cond)?;
                    if cond_val.is_truthy() {
                        Ok(ExecutionFlow::ContinueLoop)
                    } else {
                        Ok(ExecutionFlow::Continue)
                    }
                } else {
                    Ok(ExecutionFlow::ContinueLoop)
                }
            }

            Statement::Expression(expr) => {
                self.evaluate_expression(expr)?;
                Ok(ExecutionFlow::Continue)
            }

            Statement::Try { body, catch_clauses } => {
                // Try to execute the body
                match self.execute_block(body) {
                    Ok(flow) => Ok(flow),
                    Err(error) => {
                        // An error occurred, try to find a matching catch clause
                        for catch_clause in catch_clauses {
                            // If error_type is specified, check if it matches
                            // For now, we'll accept any error in any catch clause
                            // TODO: Implement error type matching
                            return self.execute_block(&catch_clause.body);
                        }

                        // No catch clause matched, re-throw the error
                        Err(error)
                    }
                }
            }

            Statement::Parallel { .. } => {
                Err(Error::NotImplemented {
                    tool: "PARALLEL execution".to_string(),
                })
            }

            Statement::WaitStrategy(_) => {
                Err(Error::NotImplemented {
                    tool: "WAIT strategies (WAIT_ALL/WAIT_ANY/RACE)".to_string(),
                })
            }

            Statement::Decision { .. } => {
                Err(Error::NotImplemented {
                    tool: "DECISION points".to_string(),
                })
            }

            Statement::Guard { condition, else_body } => {
                let cond_val = self.evaluate_expression(condition)?;

                // If condition is false, execute the else_body (guard failed)
                if !cond_val.is_truthy() {
                    self.execute_block(else_body)
                } else {
                    // Guard passed, continue
                    Ok(ExecutionFlow::Continue)
                }
            }
        }
    }

    fn execute_block(&mut self, stmts: &[Statement]) -> Result<ExecutionFlow> {
        for stmt in stmts {
            match self.execute_statement(stmt)? {
                ExecutionFlow::Continue => {}
                flow => return Ok(flow),
            }
        }
        Ok(ExecutionFlow::Continue)
    }

    // Expression evaluation

    fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::IntLiteral(n) => Ok(Value::Int(*n)),
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
            Expression::NullLiteral => Ok(Value::Null),

            Expression::Variable(name) => self.env.get(name),

            Expression::ArrayLiteral(elements) => {
                let mut arr = Vec::with_capacity(elements.len());
                for elem in elements {
                    arr.push(self.evaluate_expression(elem)?);
                }
                Ok(Value::array(arr))
            }

            Expression::ObjectLiteral(fields) => {
                let mut obj = std::collections::HashMap::new();
                for (key, value_expr) in fields {
                    let value = self.evaluate_expression(value_expr)?;
                    obj.insert(key.clone(), value);
                }
                Ok(Value::object(obj))
            }

            Expression::Range { start, end } => {
                let start_val = self.evaluate_expression(start)?.as_int()?;
                let end_val = self.evaluate_expression(end)?.as_int()?;
                Ok(Value::Range {
                    start: start_val,
                    end: end_val,
                })
            }

            Expression::Binary { op, left, right } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;
                self.apply_binary_op(*op, left_val, right_val)
            }

            Expression::Unary { op, operand } => {
                let val = self.evaluate_expression(operand)?;
                self.apply_unary_op(*op, val)
            }

            Expression::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_val = self.evaluate_expression(condition)?;
                if cond_val.is_truthy() {
                    self.evaluate_expression(then_expr)
                } else {
                    self.evaluate_expression(else_expr)
                }
            }

            Expression::FieldAccess { object, field } => {
                let obj_val = self.evaluate_expression(object)?;
                obj_val.get_field(field)
            }

            Expression::IndexAccess { array, index } => {
                let arr_val = self.evaluate_expression(array)?;
                let idx_val = self.evaluate_expression(index)?;
                arr_val.get_index(&idx_val)
            }

            Expression::Grouping(expr) => self.evaluate_expression(expr),

            Expression::ToolCall { name, args } => {
                // Get the tool from registry
                let tool = self.registry.get(name)?;

                // Evaluate arguments
                let mut evaluated_args = Vec::new();
                for arg in args {
                    let val = self.evaluate_expression(&arg.value)?;
                    evaluated_args.push(val);
                }

                // Execute tool
                tool.execute(&evaluated_args)
            }

            Expression::Lambda { .. } => {
                // Placeholder - will implement lambda support
                Err(Error::NotImplemented {
                    tool: "lambdas".to_string(),
                })
            }
        }
    }

    // Operator application

    fn apply_binary_op(&self, op: BinaryOp, left: Value, right: Value) -> Result<Value> {
        match op {
            BinaryOp::Add => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "add".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Sub => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - r as f64)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "subtract".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Mul => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * r as f64)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "multiply".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Div => match (left, right) {
                (Value::Int(l), Value::Int(r)) => {
                    if r == 0 {
                        Err(Error::DivisionByZero)
                    } else {
                        Ok(Value::Int(l / r))
                    }
                }
                (Value::Float(l), Value::Float(r)) => {
                    if r == 0.0 {
                        Err(Error::DivisionByZero)
                    } else {
                        Ok(Value::Float(l / r))
                    }
                }
                (Value::Int(l), Value::Float(r)) => {
                    if r == 0.0 {
                        Err(Error::DivisionByZero)
                    } else {
                        Ok(Value::Float(l as f64 / r))
                    }
                }
                (Value::Float(l), Value::Int(r)) => {
                    if r == 0 {
                        Err(Error::DivisionByZero)
                    } else {
                        Ok(Value::Float(l / r as f64))
                    }
                }
                (l, r) => Err(Error::InvalidOperation {
                    op: "divide".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Mod => match (left, right) {
                (Value::Int(l), Value::Int(r)) => {
                    if r == 0 {
                        Err(Error::DivisionByZero)
                    } else {
                        Ok(Value::Int(l % r))
                    }
                }
                (l, r) => Err(Error::InvalidOperation {
                    op: "modulo".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Pow => match (left, right) {
                (Value::Int(l), Value::Int(r)) => {
                    if r < 0 {
                        Ok(Value::Float((l as f64).powf(r as f64)))
                    } else {
                        Ok(Value::Int(l.pow(r as u32)))
                    }
                }
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l.powf(r))),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float((l as f64).powf(r))),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l.powf(r as f64))),
                (l, r) => Err(Error::InvalidOperation {
                    op: "power".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Eq => Ok(Value::Bool(left == right)),
            BinaryOp::NotEq => Ok(Value::Bool(left != right)),

            BinaryOp::Lt => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l < r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) < r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l < (r as f64))),
                (l, r) => Err(Error::InvalidComparison {
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Gt => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l > r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) > r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l > (r as f64))),
                (l, r) => Err(Error::InvalidComparison {
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::LtEq => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l <= r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) <= r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l <= (r as f64))),
                (l, r) => Err(Error::InvalidComparison {
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::GtEq => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l >= r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) >= r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l >= (r as f64))),
                (l, r) => Err(Error::InvalidComparison {
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::And => Ok(Value::Bool(left.is_truthy() && right.is_truthy())),

            BinaryOp::Or => Ok(Value::Bool(left.is_truthy() || right.is_truthy())),

            BinaryOp::In => {
                // Check if left is in right (array/string)
                match right {
                    Value::Array(arr) => Ok(Value::Bool(arr.iter().any(|v| v == &left))),
                    Value::String(s) => {
                        if let Value::String(needle) = left {
                            Ok(Value::Bool(s.contains(&needle)))
                        } else {
                            Err(Error::InvalidOperation {
                                op: "in".to_string(),
                                left_type: left.type_name(),
                                right_type: "string".to_string(),
                            })
                        }
                    }
                    _ => Err(Error::InvalidOperation {
                        op: "in".to_string(),
                        left_type: left.type_name(),
                        right_type: right.type_name(),
                    }),
                }
            }
        }
    }

    fn apply_unary_op(&self, op: UnaryOp, value: Value) -> Result<Value> {
        match op {
            UnaryOp::Neg => match value {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(f) => Ok(Value::Float(-f)),
                _ => Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: value.type_name(),
                }),
            },

            UnaryOp::Not => Ok(Value::Bool(!value.is_truthy())),
        }
    }

    // Helper methods

    fn get_iterable_items(&self, value: &Value) -> Result<Vec<Value>> {
        match value {
            Value::Array(arr) => Ok(arr.as_ref().clone()),
            Value::Range { .. } => value.expand_range(),
            Value::String(s) => {
                // Iterate over characters
                Ok(s.chars()
                    .map(|c| Value::String(c.to_string()))
                    .collect())
            }
            _ => Err(Error::TypeError {
                expected: "iterable (array, range, or string)".to_string(),
                got: value.type_name(),
            }),
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;
    use crate::parser::Parser;

    fn execute_ovsm(source: &str) -> Result<Value> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;
        let mut evaluator = Evaluator::new();
        evaluator.execute(&program)
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(execute_ovsm("$x = 2 + 3").unwrap(), Value::Null);
        assert_eq!(execute_ovsm("$x = 10 - 3\nRETURN $x").unwrap(), Value::Int(7));
        assert_eq!(execute_ovsm("$x = 4 * 5\nRETURN $x").unwrap(), Value::Int(20));
        assert_eq!(execute_ovsm("$x = 15 / 3\nRETURN $x").unwrap(), Value::Int(5));
    }

    #[test]
    fn test_operator_precedence() {
        let result = execute_ovsm("$x = 2 + 3 * 4\nRETURN $x").unwrap();
        assert_eq!(result, Value::Int(14)); // 2 + (3 * 4)
    }

    #[test]
    fn test_variable_assignment() {
        let result = execute_ovsm("$x = 42\nRETURN $x").unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_if_statement() {
        let result = execute_ovsm(
            r#"
            $x = 10
            IF $x > 5 THEN
                RETURN "high"
            ELSE
                RETURN "low"
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::String("high".to_string()));
    }

    #[test]
    fn test_while_loop() {
        let result = execute_ovsm(
            r#"
            $sum = 0
            $i = 1
            WHILE $i <= 5:
                $sum = $sum + $i
                $i = $i + 1
            RETURN $sum
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(15)); // 1+2+3+4+5
    }

    #[test]
    fn test_for_loop_array() {
        let result = execute_ovsm(
            r#"
            $sum = 0
            FOR $x IN [1, 2, 3, 4, 5]:
                $sum = $sum + $x
            RETURN $sum
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(15));
    }

    #[test]
    fn test_for_loop_range() {
        let result = execute_ovsm(
            r#"
            $sum = 0
            FOR $i IN [1..6]:
                $sum = $sum + $i
            RETURN $sum
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(15));
    }

    #[test]
    fn test_array_indexing() {
        let result = execute_ovsm(
            r#"
            $arr = [10, 20, 30]
            $val = $arr[1]
            RETURN $val
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(20));
    }

    #[test]
    fn test_object_field_access() {
        let result = execute_ovsm(
            r#"
            $obj = {name: "Alice", age: 30}
            $name = $obj.name
            RETURN $name
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::String("Alice".to_string()));
    }

    #[test]
    fn test_comparison_operators() {
        assert_eq!(execute_ovsm("RETURN 5 > 3").unwrap(), Value::Bool(true));
        assert_eq!(execute_ovsm("RETURN 5 < 3").unwrap(), Value::Bool(false));
        assert_eq!(execute_ovsm("RETURN 5 == 5").unwrap(), Value::Bool(true));
        assert_eq!(execute_ovsm("RETURN 5 != 3").unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_logical_operators() {
        assert_eq!(
            execute_ovsm("$x = true AND true\nRETURN $x").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            execute_ovsm("$x = true AND false\nRETURN $x").unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            execute_ovsm("$x = true OR false\nRETURN $x").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            execute_ovsm("$x = false OR false\nRETURN $x").unwrap(),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_unary_operators() {
        assert_eq!(execute_ovsm("RETURN -5").unwrap(), Value::Int(-5));
        assert_eq!(execute_ovsm("RETURN !true").unwrap(), Value::Bool(false));
        assert_eq!(execute_ovsm("RETURN !false").unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_break_statement() {
        let result = execute_ovsm(
            r#"
            $i = 0
            WHILE true:
                $i = $i + 1
                BREAK IF $i >= 3
            RETURN $i
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_ternary_operator() {
        let result = execute_ovsm("$x = 10 > 5 ? 100 : 200\nRETURN $x").unwrap();
        assert_eq!(result, Value::Int(100));
    }
}
