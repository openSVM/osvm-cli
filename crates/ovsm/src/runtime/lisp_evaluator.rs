use crate::error::{Error, Result};
use crate::parser::{BinaryOp, Expression, Program, Statement, UnaryOp};
use crate::runtime::{Environment, Value};
use crate::tools::ToolRegistry;
use std::sync::Arc;

/// LISP-specific evaluator that handles special forms
///
/// This is a standalone evaluator for LISP syntax with special forms:
/// - `(set! var value)` - Variable mutation
/// - `(define var value)` - Variable definition
/// - `(let ((x v)...) body)` - Lexical scoping
/// - `(while cond body)` - While loops
/// - `(for (var coll) body)` - For loops
/// - `(const name value)` - Constants
pub struct LispEvaluator {
    /// Variable environment
    env: Environment,
    /// Tool registry
    registry: Arc<ToolRegistry>,
}

impl LispEvaluator {
    /// Creates a new LISP evaluator
    pub fn new() -> Self {
        LispEvaluator {
            env: Environment::new(),
            registry: Arc::new(ToolRegistry::new()),
        }
    }

    /// Creates a new LISP evaluator with custom tool registry
    pub fn with_registry(registry: ToolRegistry) -> Self {
        LispEvaluator {
            env: Environment::new(),
            registry: Arc::new(registry),
        }
    }

    /// Execute a LISP-style program
    pub fn execute(&mut self, program: &Program) -> Result<Value> {
        let mut last_val = Value::Null;

        for statement in &program.statements {
            last_val = self.evaluate_statement(statement)?;
        }

        Ok(last_val)
    }

    /// Evaluate a statement
    fn evaluate_statement(&mut self, stmt: &Statement) -> Result<Value> {
        match stmt {
            Statement::Expression(expr) => self.evaluate_expression(expr),

            Statement::ConstantDef { name, value } => {
                let val = self.evaluate_expression(value)?;
                self.env.define(name.clone(), val.clone());
                Ok(val)
            }

            Statement::Assignment { name, value } => {
                let val = self.evaluate_expression(value)?;
                self.env.set(&name, val.clone())?;
                Ok(val)
            }

            _ => {
                // For other statements, delegate to base evaluator
                // This is a simplified approach - full integration would refactor this
                Err(Error::NotImplemented {
                    tool: "Statement type in LISP evaluator".to_string(),
                })
            }
        }
    }

    /// Evaluate an expression with LISP special form handling
    fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::ToolCall { name, args } => {
                // Check if this is a LISP special form
                match name.as_str() {
                    "set!" => self.eval_set(args),
                    "define" => self.eval_define(args),
                    "defun" => self.eval_defun(args),
                    "const" => self.eval_const(args),
                    "let" => self.eval_let(args),
                    "while" => self.eval_while(args),
                    "for" => self.eval_for(args),
                    "do" => self.eval_do(args),
                    "when" => self.eval_when(args),
                    "not" => self.eval_not(args),
                    "and" => self.eval_and(args),
                    "or" => self.eval_or(args),
                    "null?" => self.eval_null_check(args),
                    "empty?" => self.eval_empty_check(args),
                    "length" => self.eval_length(args),
                    "last" => self.eval_last(args),
                    "range" => self.eval_range(args),
                    "min" => self.eval_min(args),
                    "max" => self.eval_max(args),
                    "now" => self.eval_now(args),
                    "log" => self.eval_log(args),
                    "map" => self.eval_map(args),
                    "filter" => self.eval_filter(args),
                    "reduce" => self.eval_reduce(args),
                    "sort" => self.eval_sort(args),
                    "str" => self.eval_str(args),
                    "slice" => self.eval_slice(args),
                    "keys" => self.eval_keys(args),
                    "get" => self.eval_get(args),
                    _ => {
                        // Not a special form, delegate to base evaluator
                        // This would call regular tools
                        self.eval_tool_call(name, args)
                    }
                }
            }

            // For all other expressions, use the base evaluator's logic
            Expression::IntLiteral(n) => Ok(Value::Int(*n)),
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
            Expression::NullLiteral => Ok(Value::Null),

            Expression::Variable(name) => {
                self.env.get(name)
            }

            Expression::ArrayLiteral(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    values.push(self.evaluate_expression(elem)?);
                }
                Ok(Value::Array(Arc::new(values)))
            }

            Expression::ObjectLiteral(pairs) => {
                let mut map = std::collections::HashMap::new();
                for (key, val_expr) in pairs {
                    let val = self.evaluate_expression(val_expr)?;
                    map.insert(key.clone(), val);
                }
                Ok(Value::Object(Arc::new(map)))
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

            Expression::Ternary { condition, then_expr, else_expr } => {
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

            Expression::Lambda { params, body } => {
                let closure = Arc::new(std::collections::HashMap::new());
                Ok(Value::Function {
                    params: params.clone(),
                    body: Arc::new((**body).clone()),
                    closure,
                })
            }

            _ => Err(Error::NotImplemented {
                tool: format!("Expression type: {:?}", expr),
            }),
        }
    }

    // LISP Special Forms Implementation

    /// (set! var value) - Mutate existing variable
    fn eval_set(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 2, args.len()),
            })?;
        }

        // First arg should be a variable name
        let var_name = match &args[0].value {
            Expression::Variable(name) => name.clone(),
            _ => return Err(Error::ParseError("set! requires variable name".to_string())),
        };

        // Evaluate the value
        let value = self.evaluate_expression(&args[1].value)?;

        // Set the variable
        self.env.set(&var_name, value.clone())?;

        Ok(value)
    }

    /// (define var value) - Define new variable
    fn eval_define(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 2, args.len()),
            })?;
        }

        let var_name = match &args[0].value {
            Expression::Variable(name) => name.clone(),
            _ => return Err(Error::ParseError("define requires variable name".to_string())),
        };

        let value = self.evaluate_expression(&args[1].value)?;
        self.env.define(var_name, value.clone());

        Ok(value)
    }

    /// (defun name (params...) body) - Define named function
    fn eval_defun(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "defun".to_string(),
                reason: "Expected 3 arguments: name, parameters, body".to_string(),
            });
        }

        // Get function name
        let func_name = match &args[0].value {
            Expression::Variable(name) => name.clone(),
            _ => return Err(Error::ParseError("defun requires function name".to_string())),
        };

        // Get parameters list
        let params = match &args[1].value {
            Expression::ArrayLiteral(param_exprs) => {
                let mut param_names = Vec::new();
                for param_expr in param_exprs {
                    if let Expression::Variable(name) = param_expr {
                        param_names.push(name.clone());
                    } else {
                        return Err(Error::ParseError("defun parameters must be identifiers".to_string()));
                    }
                }
                param_names
            }
            _ => return Err(Error::ParseError("defun requires parameter list".to_string())),
        };

        // Create function value
        let func_value = Value::Function {
            params,
            body: Arc::new(args[2].value.clone()),
            closure: Arc::new(std::collections::HashMap::new()),
        };

        // Define function in environment
        self.env.define(func_name, func_value.clone());

        Ok(func_value)
    }

    /// (const name value) - Define constant
    fn eval_const(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // For now, treat const the same as define
        // In a full implementation, we'd mark it as immutable
        self.eval_define(args)
    }

    /// (let ((x v)...) body) - Lexical scope with bindings
    fn eval_let(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "let".to_string(),
                reason: "Expected at least 2 arguments: bindings and body".to_string(),
            });
        }

        // First arg should be bindings list: ((x 10) (y 20))
        let bindings_expr = &args[0].value;

        // Parse bindings (simplified - expecting array of arrays)
        let bindings = match bindings_expr {
            Expression::ArrayLiteral(pairs) => {
                let mut result = Vec::new();
                for pair in pairs {
                    match pair {
                        Expression::ArrayLiteral(elements) if elements.len() == 2 => {
                            let var_name = match &elements[0] {
                                Expression::Variable(n) => n.clone(),
                                _ => return Err(Error::ParseError(
                                    "let binding requires variable name".to_string(),
                                )),
                            };
                            result.push((var_name, &elements[1]));
                        }
                        _ => {
                            return Err(Error::ParseError(
                                "let bindings must be pairs: (var value)".to_string(),
                            ))
                        }
                    }
                }
                result
            }
            _ => {
                return Err(Error::ParseError(
                    "let requires bindings list: ((x 10) (y 20))".to_string(),
                ))
            }
        };

        // Create new scope
        self.env.enter_scope();

        // Evaluate and bind variables
        for (var_name, value_expr) in bindings {
            let value = self.evaluate_expression(value_expr)?;
            self.env.define(var_name, value);
        }

        // Execute body
        let mut last_val = Value::Null;
        for arg in &args[1..] {
            last_val = self.evaluate_expression(&arg.value)?;
        }

        // Exit scope
        self.env.exit_scope();

        Ok(last_val)
    }

    /// (while cond body...) - While loop
    fn eval_while(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "while".to_string(),
                reason: "Expected at least 1 argument (condition)".to_string(),
            });
        }

        let condition_expr = &args[0].value;
        let body_args = &args[1..];

        let mut last_val = Value::Null;
        let max_iterations = 100000; // Safety limit
        let mut iterations = 0;

        loop {
            // Check iteration limit
            iterations += 1;
            if iterations > max_iterations {
                return Err(Error::TooManyIterations {
                    limit: max_iterations,
                });
            }

            // Evaluate condition
            let cond_val = self.evaluate_expression(condition_expr)?;
            if !cond_val.is_truthy() {
                break;
            }

            // Execute body
            for arg in body_args {
                last_val = self.evaluate_expression(&arg.value)?;
            }
        }

        Ok(last_val)
    }

    /// (for (var coll) body...) - For loop
    ///
    /// When parsing (for (x [1 2 3]) body...), the S-expression parser flattens the inner list,
    /// so we receive: args = [Variable("x"), ArrayLiteral([1,2,3]), body...]
    fn eval_for(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "for".to_string(),
                reason: "Expected at least 3 arguments: var collection body...".to_string(),
            });
        }

        // Extract variable name from first arg
        let var_name = match &args[0].value {
            Expression::Variable(n) => n.clone(),
            _ => {
                return Err(Error::ParseError(
                    "for syntax: (for (var collection) body...), var must be a variable name".to_string(),
                ))
            }
        };

        // Second arg is the collection expression
        let collection_expr = &args[1].value;

        // Evaluate the collection
        let collection = self.evaluate_expression(collection_expr)?;

        // Get items to iterate over
        let items = match collection {
            Value::Array(ref arr) => arr.iter().cloned().collect::<Vec<_>>(),
            _ => {
                return Err(Error::TypeError {
                    expected: "array".to_string(),
                    got: collection.type_name(),
                })
            }
        };

        // Create new scope for loop
        self.env.enter_scope();

        let mut last_val = Value::Null;
        for item in items {
            // Bind loop variable
            self.env.define(var_name.clone(), item);

            // Execute body (args[2..] because args[0]=var, args[1]=collection)
            for arg in &args[2..] {
                last_val = self.evaluate_expression(&arg.value)?;
            }
        }

        self.env.exit_scope();

        Ok(last_val)
    }

    /// (do expr1 expr2 ... exprN) - Sequential execution
    fn eval_do(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        let mut last_val = Value::Null;
        for arg in args {
            last_val = self.evaluate_expression(&arg.value)?;
        }
        Ok(last_val)
    }

    /// (when cond body...) - Conditional execution
    fn eval_when(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments", 1),
            });
        }

        let cond_val = self.evaluate_expression(&args[0].value)?;
        if cond_val.is_truthy() {
            let mut last_val = Value::Null;
            for arg in &args[1..] {
                last_val = self.evaluate_expression(&arg.value)?;
            }
            Ok(last_val)
        } else {
            Ok(Value::Null)
        }
    }

    // Helper functions

    /// (not x) - Logical NOT
    fn eval_not(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 1, args.len()),
            })?;
        }

        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(!val.is_truthy()))
    }

    /// (and x y ...) - Logical AND (short-circuiting)
    fn eval_and(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            if !val.is_truthy() {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }

    /// (or x y ...) - Logical OR (short-circuiting)
    fn eval_or(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            if val.is_truthy() {
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    }

    /// (null? x) - Check if null
    fn eval_null_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 1, args.len()),
            })?;
        }

        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Null)))
    }

    /// (empty? x) - Check if collection is empty
    fn eval_empty_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 1, args.len()),
            })?;
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let is_empty = match val {
            Value::Array(ref arr) => arr.is_empty(),
            Value::String(ref s) => s.is_empty(),
            _ => false,
        };
        Ok(Value::Bool(is_empty))
    }

    /// (length x) - Get length of collection
    fn eval_length(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 1, args.len()),
            })?;
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let len = match val {
            Value::Array(ref arr) => arr.len(),
            Value::String(ref s) => s.len(),
            _ => return Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: val.type_name(),
            }),
        };
        Ok(Value::Int(len as i64))
    }

    /// (last x) - Get last element of collection
    fn eval_last(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 1, args.len()),
            })?;
        }

        let val = self.evaluate_expression(&args[0].value)?;
        match val {
            Value::Array(ref arr) => {
                arr.last().cloned().ok_or(Error::IndexOutOfBounds {
                    index: 0,
                    length: 0,
                })
            }
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: val.type_name(),
            }),
        }
    }

    /// (range start end) - Create range
    fn eval_range(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 2, args.len()),
            })?;
        }

        let start_val = self.evaluate_expression(&args[0].value)?;
        let end_val = self.evaluate_expression(&args[1].value)?;

        let start = match start_val {
            Value::Int(n) => n,
            _ => return Err(Error::TypeError {
                expected: "int".to_string(),
                got: start_val.type_name(),
            }),
        };

        let end = match end_val {
            Value::Int(n) => n,
            _ => return Err(Error::TypeError {
                expected: "int".to_string(),
                got: end_val.type_name(),
            }),
        };

        let values: Vec<Value> = (start..end).map(Value::Int).collect();
        Ok(Value::Array(Arc::new(values)))
    }

    /// (min x y ...) - Get minimum value
    fn eval_min(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "min".to_string(),
                reason: "Expected at least 1 argument".to_string(),
            });
        }

        let mut min_val: Option<i64> = None;
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            let num = match val {
                Value::Int(n) => n,
                _ => return Err(Error::TypeError {
                    expected: "int".to_string(),
                    got: val.type_name(),
                }),
            };
            min_val = Some(min_val.map_or(num, |m| m.min(num)));
        }
        Ok(Value::Int(min_val.unwrap()))
    }

    /// (max x y ...) - Get maximum value
    fn eval_max(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "max".to_string(),
                reason: "Expected at least 1 argument".to_string(),
            });
        }

        let mut max_val: Option<i64> = None;
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            let num = match val {
                Value::Int(n) => n,
                _ => return Err(Error::TypeError {
                    expected: "int".to_string(),
                    got: val.type_name(),
                }),
            };
            max_val = Some(max_val.map_or(num, |m| m.max(num)));
        }
        Ok(Value::Int(max_val.unwrap()))
    }

    /// (now) - Get current timestamp
    fn eval_now(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if !args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "lisp_evaluator".to_string(),
                reason: format!("Expected {} arguments, got {}", 0, args.len()),
            })?;
        }

        use std::time::{SystemTime, UNIX_EPOCH};
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|e| Error::ParseError(format!("Time error: {}", e)))?
            .as_secs();

        Ok(Value::Int(timestamp as i64))
    }

    /// (log :message msg) - Log message
    fn eval_log(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Collect message and value separately
        let mut message_val = None;
        let mut value_val = None;

        for arg in args {
            if let Some(ref name) = arg.name {
                match name.as_str() {
                    "message" => {
                        message_val = Some(self.evaluate_expression(&arg.value)?);
                    }
                    "value" => {
                        value_val = Some(self.evaluate_expression(&arg.value)?);
                    }
                    _ => {}
                }
            }
        }

        // Print message and value
        if let Some(msg) = message_val {
            if let Some(val) = value_val {
                println!("{} {}", msg, val);
            } else {
                println!("{}", msg);
            }
        } else if let Some(val) = value_val {
            println!("{}", val);
        } else {
            // If no named args, print all positional args
            for arg in args {
                if arg.name.is_none() {
                    let val = self.evaluate_expression(&arg.value)?;
                    println!("{}", val);
                }
            }
        }

        Ok(Value::Null)
    }

    /// (map collection lambda) - Map function over collection
    fn eval_map(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "map".to_string(),
                reason: "Expected 2 arguments: collection and lambda".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get lambda function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, closure: _ } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "map".to_string(),
                        reason: format!("Lambda must take exactly 1 parameter, got {}", params.len()),
                    });
                }

                let mut result = Vec::new();

                // Apply lambda to each element
                for elem in array.iter() {
                    // Create new scope for lambda execution
                    self.env.enter_scope();

                    // Bind parameter
                    self.env.define(params[0].clone(), elem.clone());

                    // Evaluate body
                    let val = self.evaluate_expression(&body)?;
                    result.push(val);

                    // Exit scope
                    self.env.exit_scope();
                }

                Ok(Value::Array(Arc::new(result)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (filter collection lambda) - Filter collection by predicate
    fn eval_filter(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "filter".to_string(),
                reason: "Expected 2 arguments: collection and predicate".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get predicate function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, closure: _ } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "filter".to_string(),
                        reason: format!("Lambda must take exactly 1 parameter, got {}", params.len()),
                    });
                }

                let mut result = Vec::new();

                // Apply predicate to each element
                for elem in array.iter() {
                    // Create new scope for lambda execution
                    self.env.enter_scope();

                    // Bind parameter
                    self.env.define(params[0].clone(), elem.clone());

                    // Evaluate predicate
                    let val = self.evaluate_expression(&body)?;

                    // Exit scope
                    self.env.exit_scope();

                    // Include element if predicate is truthy
                    if val.is_truthy() {
                        result.push(elem.clone());
                    }
                }

                Ok(Value::Array(Arc::new(result)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (reduce collection initial lambda) - Reduce collection to single value using accumulator lambda
    fn eval_reduce(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "reduce".to_string(),
                reason: "Expected 3 arguments: collection, initial value, and reducer lambda".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Evaluate initial accumulator value
        let mut accumulator = self.evaluate_expression(&args[1].value)?;

        // Get reducer function
        let func = self.evaluate_expression(&args[2].value)?;

        match func {
            Value::Function { params, body, closure: _ } => {
                if params.len() != 2 {
                    return Err(Error::InvalidArguments {
                        tool: "reduce".to_string(),
                        reason: format!("Lambda must take exactly 2 parameters (accumulator, element), got {}", params.len()),
                    });
                }

                // Apply reducer to each element
                for elem in array.iter() {
                    // Create new scope for lambda execution
                    self.env.enter_scope();

                    // Bind parameters: accumulator and current element
                    self.env.define(params[0].clone(), accumulator.clone());
                    self.env.define(params[1].clone(), elem.clone());

                    // Evaluate reducer body
                    accumulator = self.evaluate_expression(&body)?;

                    // Exit scope
                    self.env.exit_scope();
                }

                Ok(accumulator)
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (sort collection comparator) - Sort collection using comparator lambda
    fn eval_sort(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "sort".to_string(),
                reason: "Expected 2 arguments: collection and comparator".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get comparator function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, closure: _ } => {
                if params.len() != 2 {
                    return Err(Error::InvalidArguments {
                        tool: "sort".to_string(),
                        reason: format!("Lambda must take exactly 2 parameters, got {}", params.len()),
                    });
                }

                // Clone array for sorting
                let mut sorted = array.to_vec();

                // Manual bubble sort to avoid closure borrowing issues
                let n = sorted.len();
                for i in 0..n {
                    for j in 0..(n - i - 1) {
                        // Create new scope
                        self.env.enter_scope();

                        // Bind parameters (a=sorted[j], b=sorted[j+1])
                        self.env.define(params[0].clone(), sorted[j].clone());
                        self.env.define(params[1].clone(), sorted[j + 1].clone());

                        // Evaluate comparator: if (comparator a b) is false, swap
                        let result = self.evaluate_expression(&body)?;

                        // Exit scope
                        self.env.exit_scope();

                        // If comparator returns false, swap
                        if !result.is_truthy() {
                            sorted.swap(j, j + 1);
                        }
                    }
                }

                Ok(Value::Array(Arc::new(sorted)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (str args...) - Concatenate values into string
    fn eval_str(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        let mut result = String::new();

        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            // Convert value to string
            let s = match val {
                Value::String(s) => s,
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Null => "null".to_string(),
                _ => format!("{}", val),
            };
            result.push_str(&s);
        }

        Ok(Value::String(result))
    }

    /// (slice array start end) - Extract subarray from start to end (exclusive)
    fn eval_slice(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "slice".to_string(),
                reason: "Expected 3 arguments: array, start, end".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        let start_val = self.evaluate_expression(&args[1].value)?;
        let start = start_val.as_int()? as usize;

        let end_val = self.evaluate_expression(&args[2].value)?;
        let end = end_val.as_int()? as usize;

        // Bounds checking
        if start > array.len() || end > array.len() || start > end {
            return Err(Error::InvalidArguments {
                tool: "slice".to_string(),
                reason: format!("Invalid slice bounds: start={}, end={}, len={}", start, end, array.len()),
            });
        }

        let sliced: Vec<Value> = array[start..end].to_vec();
        Ok(Value::Array(Arc::new(sliced)))
    }

    /// keys(object) - Get array of object keys
    fn eval_keys(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "keys".to_string(),
                reason: "Expected 1 argument: object".to_string(),
            });
        }

        let obj_val = self.evaluate_expression(&args[0].value)?;
        let obj = obj_val.as_object()?;

        let keys: Vec<Value> = obj.keys()
            .map(|k| Value::String(k.clone()))
            .collect();

        Ok(Value::Array(Arc::new(keys)))
    }

    /// get(object, key) - Safely get object property, returns null if not found
    fn eval_get(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "get".to_string(),
                reason: "Expected 2 arguments: object, key".to_string(),
            });
        }

        let obj_val = self.evaluate_expression(&args[0].value)?;
        let obj = obj_val.as_object()?;

        let key_val = self.evaluate_expression(&args[1].value)?;
        let key = key_val.as_string()?;

        Ok(obj.get(&*key).cloned().unwrap_or(Value::Null))
    }

    /// Evaluate a regular tool call
    fn eval_tool_call(&mut self, name: &str, args: &[crate::parser::Argument]) -> Result<Value> {
        // Check if this is a user-defined function first
        if let Ok(func_val) = self.env.get(name) {
            if let Value::Function { params, body, closure: _ } = func_val {
                // This is a function call!

                // Evaluate arguments
                let mut evaluated_args = Vec::new();
                for arg in args {
                    let val = self.evaluate_expression(&arg.value)?;
                    evaluated_args.push(val);
                }

                // Check parameter count
                if params.len() != evaluated_args.len() {
                    return Err(Error::InvalidArguments {
                        tool: name.to_string(),
                        reason: format!("Function expects {} parameters, got {}", params.len(), evaluated_args.len()),
                    });
                }

                // Create new scope for function execution
                self.env.enter_scope();

                // Bind parameters to arguments
                for (param, arg_val) in params.iter().zip(evaluated_args.iter()) {
                    self.env.set(param, arg_val.clone());
                }

                // Evaluate function body
                let result = self.evaluate_expression(&body);

                // Exit function scope
                self.env.exit_scope();

                return result;
            }
        }

        // Not a function, try tool registry
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

    // Binary operator implementation (simplified from base evaluator)

    fn apply_binary_op(&self, op: BinaryOp, left: Value, right: Value) -> Result<Value> {
        match op {
            BinaryOp::Add => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
                (Value::Array(l), Value::Array(r)) => {
                    // Array concatenation
                    let mut result = (*l).clone();
                    result.extend((*r).clone());
                    Ok(Value::Array(Arc::new(result)))
                }
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
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l % r)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "modulo".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Eq => Ok(Value::Bool(left == right)),
            BinaryOp::NotEq => Ok(Value::Bool(left != right)),

            BinaryOp::Lt => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l < r)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "less than".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Gt => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l > r)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "greater than".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::LtEq => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l <= r)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "less than or equal".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::GtEq => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l >= r)),
                (l, r) => Err(Error::InvalidOperation {
                    op: "greater than or equal".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::And => Ok(Value::Bool(left.is_truthy() && right.is_truthy())),
            BinaryOp::Or => Ok(Value::Bool(left.is_truthy() || right.is_truthy())),

            _ => Err(Error::NotImplemented {
                tool: format!("Binary operator: {:?}", op),
            }),
        }
    }

    fn apply_unary_op(&self, op: UnaryOp, operand: Value) -> Result<Value> {
        match op {
            UnaryOp::Neg => match operand {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(f) => Ok(Value::Float(-f)),
                v => Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: v.type_name(),
                }),
            },
            UnaryOp::Not => Ok(Value::Bool(!operand.is_truthy())),
        }
    }
}

impl Default for LispEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::SExprScanner;
    use crate::parser::SExprParser;

    fn eval_str(source: &str) -> Result<Value> {
        let mut scanner = SExprScanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = SExprParser::new(tokens);
        let program = parser.parse()?;
        let mut evaluator = LispEvaluator::new();
        evaluator.execute(&program)
    }

    #[test]
    fn test_define_and_reference() {
        let result = eval_str("(define x 42) x").unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_set_mutation() {
        let result = eval_str("(define x 10) (set! x 20) x").unwrap();
        assert_eq!(result, Value::Int(20));
    }

    #[test]
    fn test_arithmetic() {
        let result = eval_str("(+ 1 2 3)").unwrap();
        assert_eq!(result, Value::Int(6));
    }

    #[test]
    fn test_not() {
        let result = eval_str("(not true)").unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_length() {
        let result = eval_str("(length [1 2 3 4 5])").unwrap();
        assert_eq!(result, Value::Int(5));
    }

    #[test]
    fn test_log() {
        let result = eval_str("(log :message \"Hello, World!\")");
        assert!(result.is_ok());
    }
}
