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
    /// Gensym counter for generating unique symbols
    gensym_counter: std::cell::Cell<u64>,
}

impl LispEvaluator {
    /// Creates a new LISP evaluator
    pub fn new() -> Self {
        LispEvaluator {
            env: Environment::new(),
            registry: Arc::new(ToolRegistry::new()),
            gensym_counter: std::cell::Cell::new(0),
        }
    }

    /// Creates a new LISP evaluator with custom tool registry
    pub fn with_registry(registry: ToolRegistry) -> Self {
        LispEvaluator {
            env: Environment::new(),
            registry: Arc::new(registry),
            gensym_counter: std::cell::Cell::new(0),
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
        // First, try macro expansion
        if let Some(expanded) = self.try_expand_macro(expr)? {
            // Recursively evaluate expanded form (macros can expand to macro calls)
            return self.evaluate_expression(&expanded);
        }

        match expr {
            // Handle quasiquote expressions
            Expression::Quasiquote(_) => self.eval_quasiquote(expr),

            Expression::ToolCall { name, args } => {
                // Check if this is a LISP special form
                match name.as_str() {
                    "set!" => self.eval_set(args),
                    "define" => self.eval_define(args),
                    "defun" => self.eval_defun(args),
                    "defmacro" => self.eval_defmacro(args),
                    "const" => self.eval_const(args),
                    "let" => self.eval_let(args),
                    "let*" => self.eval_let_star(args),
                    "flet" => self.eval_flet(args),
                    "while" => self.eval_while(args),
                    "for" => self.eval_for(args),
                    "do" => self.eval_do(args),
                    "when" => self.eval_when(args),
                    "unless" => self.eval_unless(args),
                    "cond" => self.eval_cond(args),
                    "not" => self.eval_not(args),
                    "and" => self.eval_and(args),
                    "or" => self.eval_or(args),
                    "null?" => self.eval_null_check(args),
                    "empty?" => self.eval_empty_check(args),
                    // Type predicates
                    "int?" => self.eval_int_check(args),
                    "float?" => self.eval_float_check(args),
                    "number?" => self.eval_number_check(args),
                    "string?" => self.eval_string_check(args),
                    "bool?" => self.eval_bool_check(args),
                    "array?" => self.eval_array_check(args),
                    "object?" => self.eval_object_check(args),
                    "function?" => self.eval_function_check(args),
                    // Assertions
                    "assert" => self.eval_assert(args),
                    "assert-type" => self.eval_assert_type(args),
                    // Error handling
                    "try" => self.eval_try(args),
                    "error" => self.eval_error(args),
                    // String operations
                    "split" => self.eval_split(args),
                    "join" => self.eval_join(args),
                    "replace" => self.eval_replace(args),
                    "trim" => self.eval_trim(args),
                    "upper" => self.eval_upper(args),
                    "lower" => self.eval_lower(args),
                    // Advanced math
                    "sqrt" => self.eval_sqrt(args),
                    "pow" => self.eval_pow(args),
                    "abs" => self.eval_abs(args),
                    // Multiple values (Common Lisp style)
                    "values" => self.eval_values(args),
                    "multiple-value-bind" => self.eval_multiple_value_bind(args),
                    // Dynamic variables (Common Lisp special variables)
                    "defvar" => self.eval_defvar(args),
                    // Macro system
                    "gensym" => self.eval_gensym(args),
                    "macroexpand" => self.eval_macroexpand(args),
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
                    "first" => self.eval_first(args),
                    "rest" => self.eval_rest(args),
                    "cons" => self.eval_cons(args),
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

        // Get parameters list (supports &rest)
        let params = self.parse_function_parameters(&args[1].value, "defun")?;

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

    /// (defmacro name (params...) body) - Define macro
    /// Macros are compile-time code transformers that receive unevaluated arguments
    fn eval_defmacro(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "defmacro".to_string(),
                reason: "Expected 3 arguments: name, parameters, body".to_string(),
            });
        }

        // Get macro name
        let macro_name = match &args[0].value {
            Expression::Variable(name) => name.clone(),
            _ => return Err(Error::ParseError("defmacro requires macro name".to_string())),
        };

        // Get parameters list (supports &rest)
        let params = self.parse_function_parameters(&args[1].value, "defmacro")?;

        // Create macro value
        let macro_value = Value::Macro {
            params,
            body: Arc::new(args[2].value.clone()),
            closure: Arc::new(std::collections::HashMap::new()),
        };

        // Define macro in environment
        self.env.define(macro_name, macro_value.clone());

        Ok(macro_value)
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

    /// (let* ((var val)...) body) - Sequential binding where each binding can reference previous ones
    fn eval_let_star(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "let*".to_string(),
                reason: "Expected at least 2 arguments: bindings and body".to_string(),
            });
        }

        // Parse bindings (same format as let)
        let bindings = match &args[0].value {
            Expression::ArrayLiteral(binding_pairs) => {
                let mut result = Vec::new();
                for pair in binding_pairs {
                    match pair {
                        Expression::ArrayLiteral(elements) if elements.len() == 2 => {
                            let var_name = match &elements[0] {
                                Expression::Variable(n) => n.clone(),
                                _ => return Err(Error::ParseError(
                                    "let* binding requires variable name".to_string(),
                                )),
                            };
                            result.push((var_name, &elements[1]));
                        }
                        _ => {
                            return Err(Error::ParseError(
                                "let* bindings must be pairs: (var value)".to_string(),
                            ))
                        }
                    }
                }
                result
            }
            _ => {
                return Err(Error::ParseError(
                    "let* requires bindings list: ((x 10) (y 20))".to_string(),
                ))
            }
        };

        // Create new scope
        self.env.enter_scope();

        // KEY DIFFERENCE: Evaluate and bind variables SEQUENTIALLY
        // Each binding can reference previously bound variables
        for (var_name, value_expr) in bindings {
            let value = self.evaluate_expression(value_expr)?;
            self.env.define(var_name, value);
            // Note: Variable is immediately available for next binding!
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

    /// (flet ((name (params) body)...) body) - Local function definitions
    /// Unlike labels, functions can't call themselves or each other
    fn eval_flet(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "flet".to_string(),
                reason: "Expected at least 2 arguments: function definitions and body".to_string(),
            });
        }

        // Parse function definitions (first argument should be array of function defs)
        let func_defs = match &args[0].value {
            Expression::ArrayLiteral(defs) => defs,
            _ => {
                return Err(Error::ParseError(
                    "flet requires function definitions list: ((name (params) body)...)".to_string(),
                ))
            }
        };

        // Parse each function definition
        let mut functions: Vec<(String, Vec<String>, Expression)> = Vec::new();

        for func_def in func_defs {
            match func_def {
                Expression::ArrayLiteral(parts) if parts.len() == 3 => {
                    // Extract name
                    let name = match &parts[0] {
                        Expression::Variable(n) => n.clone(),
                        _ => return Err(Error::ParseError(
                            "flet function definition requires name".to_string(),
                        )),
                    };

                    // Extract parameters
                    let params = self.parse_function_parameters(&parts[1], "flet")?;

                    // Extract body (clone it)
                    let body = parts[2].clone();

                    functions.push((name, params, body));
                }
                _ => {
                    return Err(Error::ParseError(
                        "flet function definitions must be: (name (params) body)".to_string(),
                    ))
                }
            }
        }

        // Create new scope for local functions
        self.env.enter_scope();

        // Bind functions in current environment (non-recursively)
        // Each function closes over the OUTER environment, not seeing other flet functions
        let outer_env = self.env.current_env_snapshot();

        for (name, params, body) in functions {
            let func_value = Value::Function {
                params,
                body: Arc::new(body),
                closure: Arc::new(outer_env.clone()),
            };
            self.env.define(name, func_value);
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

    /// (unless cond body...) - Inverted when (execute if condition is false)
    fn eval_unless(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "unless".to_string(),
                reason: "Expected at least condition".to_string(),
            });
        }

        let cond_val = self.evaluate_expression(&args[0].value)?;
        if !cond_val.is_truthy() {
            let mut last_val = Value::Null;
            for arg in &args[1..] {
                last_val = self.evaluate_expression(&arg.value)?;
            }
            Ok(last_val)
        } else {
            Ok(Value::Null)
        }
    }

    /// (cond (test1 result1) (test2 result2) ... (else default)) - Multi-way conditional
    fn eval_cond(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        for arg in args {
            // Each clause can be either an array literal [cond result] or a ToolCall (cond result)
            let (condition_expr, result_expr) = match &arg.value {
                Expression::ArrayLiteral(pair) => {
                    if pair.len() != 2 {
                        return Err(Error::ParseError(
                            "cond clause must have 2 elements: [condition result]".to_string(),
                        ));
                    }
                    (&pair[0], &pair[1])
                }
                Expression::ToolCall { name: _, args: clause_args } => {
                    // S-expression form: (condition result)
                    if clause_args.len() != 2 {
                        return Err(Error::ParseError(
                            "cond clause must have 2 elements: (condition result)".to_string(),
                        ));
                    }
                    (&clause_args[0].value, &clause_args[1].value)
                }
                _ => {
                    return Err(Error::ParseError(
                        "cond clauses must be lists or arrays: (condition result) or [condition result]".to_string(),
                    ));
                }
            };

            // Check for 'else' clause (always true)
            let is_else = if let Expression::Variable(v) = condition_expr {
                v == "else" || v == "true"
            } else {
                false
            };

            if is_else {
                return self.evaluate_expression(result_expr);
            }

            // Evaluate condition
            let cond_val = self.evaluate_expression(condition_expr)?;
            if cond_val.is_truthy() {
                return self.evaluate_expression(result_expr);
            }
        }

        // No condition matched
        Ok(Value::Null)
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

    /// (int? x) - Check if integer
    fn eval_int_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "int?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Int(_))))
    }

    /// (float? x) - Check if float
    fn eval_float_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "float?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Float(_))))
    }

    /// (number? x) - Check if number (int or float)
    fn eval_number_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "number?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Int(_) | Value::Float(_))))
    }

    /// (string? x) - Check if string
    fn eval_string_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "string?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::String(_))))
    }

    /// (bool? x) - Check if boolean
    fn eval_bool_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "bool?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Bool(_))))
    }

    /// (array? x) - Check if array
    fn eval_array_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "array?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Array(_))))
    }

    /// (object? x) - Check if object
    fn eval_object_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "object?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Object(_))))
    }

    /// (function? x) - Check if function
    fn eval_function_check(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "function?".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Function { .. })))
    }

    /// (assert condition "message") - Assert condition is true
    fn eval_assert(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "assert".to_string(),
                reason: format!("Expected 2 arguments (condition, message), got {}", args.len()),
            })?;
        }

        // Evaluate condition
        let condition = self.evaluate_expression(&args[0].value)?;
        let is_true = match condition {
            Value::Bool(b) => b,
            _ => return Err(Error::TypeError {
                expected: "bool".to_string(),
                got: format!("{:?}", condition),
            }),
        };

        if !is_true {
            // Evaluate message
            let message = self.evaluate_expression(&args[1].value)?;
            let message_str = match message {
                Value::String(s) => s,
                _ => format!("{:?}", message),
            };
            return Err(Error::AssertionFailed { message: message_str });
        }

        Ok(Value::Null)
    }

    /// (assert-type value predicate) - Assert value matches type predicate
    fn eval_assert_type(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "assert-type".to_string(),
                reason: format!("Expected 2 arguments (value, predicate), got {}", args.len()),
            })?;
        }

        // Evaluate value
        let value = self.evaluate_expression(&args[0].value)?;

        // Evaluate type predicate (should be a function call like (int? x))
        let predicate_result = self.evaluate_expression(&args[1].value)?;

        let is_valid = match predicate_result {
            Value::Bool(b) => b,
            _ => return Err(Error::TypeError {
                expected: "bool (type predicate)".to_string(),
                got: format!("{:?}", predicate_result),
            }),
        };

        if !is_valid {
            let type_name = match value {
                Value::Null => "null",
                Value::Bool(_) => "bool",
                Value::Int(_) => "int",
                Value::Float(_) => "float",
                Value::String(_) => "string",
                Value::Array(_) => "array",
                Value::Object(_) => "object",
                Value::Range { .. } => "range",
                Value::Function { .. } => "function",
                Value::Multiple(_) => "multiple-values",
                Value::Macro { .. } => "macro",
            };
            return Err(Error::AssertionFailed {
                message: format!("Type assertion failed: expected different type, got {}", type_name),
            });
        }

        Ok(Value::Null)
    }

    /// (try body (catch error-var handler) [(finally cleanup)])
    /// Error handling with optional finally block
    fn eval_try(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 || args.len() > 3 {
            return Err(Error::InvalidArguments {
                tool: "try".to_string(),
                reason: format!("Expected 2-3 arguments (body, catch [, finally]), got {}", args.len()),
            })?;
        }

        // Execute try body
        let try_result = self.evaluate_expression(&args[0].value);

        // Parse catch block: should be (catch error-var handler-body)
        let catch_arg = &args[1];
        let (error_var, catch_body) = match &catch_arg.value {
            Expression::ToolCall { name, args: arguments } if name == "catch" => {
                if arguments.len() != 2 {
                    return Err(Error::InvalidArguments {
                        tool: "try".to_string(),
                        reason: "catch requires 2 arguments: error-var and handler-body".to_string(),
                    })?;
                }
                // Extract error variable name
                let error_var = match &arguments[0].value {
                    Expression::Variable(name) => name.clone(),
                    _ => return Err(Error::InvalidArguments {
                        tool: "try".to_string(),
                        reason: "catch first argument must be a variable name".to_string(),
                    })?,
                };
                (error_var, &arguments[1].value)
            },
            _ => return Err(Error::InvalidArguments {
                tool: "try".to_string(),
                reason: "Second argument must be (catch error-var handler)".to_string(),
            })?,
        };

        // Execute catch block if try failed
        let result = match try_result {
            Ok(value) => Ok(value),
            Err(error) => {
                // Bind error to variable
                self.env.enter_scope();
                let error_str = format!("{}", error);
                let _ = self.env.set(&error_var, Value::String(error_str));

                // Execute catch handler
                let catch_result = self.evaluate_expression(catch_body);
                self.env.exit_scope();
                catch_result
            }
        };

        // Execute finally block if present
        if args.len() == 3 {
            let finally_arg = &args[2];
            match &finally_arg.value {
                Expression::ToolCall { name, args: arguments } if name == "finally" => {
                    if arguments.len() != 1 {
                        return Err(Error::InvalidArguments {
                            tool: "try".to_string(),
                            reason: "finally requires 1 argument: cleanup-body".to_string(),
                        })?;
                    }
                    // Execute finally block (ignore errors)
                    let _ = self.evaluate_expression(&arguments[0].value);
                },
                _ => return Err(Error::InvalidArguments {
                    tool: "try".to_string(),
                    reason: "Third argument must be (finally cleanup)".to_string(),
                })?,
            }
        }

        result
    }

    /// (error "message") - Throw an error with a message
    fn eval_error(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "error".to_string(),
                reason: format!("Expected 1 argument (message), got {}", args.len()),
            })?;
        }

        let message = self.evaluate_expression(&args[0].value)?;
        let message_str = match message {
            Value::String(s) => s,
            _ => format!("{:?}", message),
        };

        Err(Error::AssertionFailed { message: message_str })
    }

    /// (split string delimiter) - Split string by delimiter
    fn eval_split(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "split".to_string(),
                reason: format!("Expected 2 arguments (string, delimiter), got {}", args.len()),
            })?;
        }

        let string = self.evaluate_expression(&args[0].value)?;
        let delimiter = self.evaluate_expression(&args[1].value)?;

        let string_val = match string {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", string),
            }),
        };

        let delimiter_val = match delimiter {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", delimiter),
            }),
        };

        let parts: Vec<Value> = string_val
            .split(&delimiter_val)
            .map(|s| Value::String(s.to_string()))
            .collect();

        Ok(Value::Array(Arc::new(parts)))
    }

    /// (join array delimiter) - Join array elements with delimiter
    fn eval_join(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "join".to_string(),
                reason: format!("Expected 2 arguments (array, delimiter), got {}", args.len()),
            })?;
        }

        let array = self.evaluate_expression(&args[0].value)?;
        let delimiter = self.evaluate_expression(&args[1].value)?;

        let array_val = match array {
            Value::Array(ref arr) => arr.clone(),
            _ => return Err(Error::TypeError {
                expected: "array".to_string(),
                got: format!("{:?}", array),
            }),
        };

        let delimiter_val = match delimiter {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", delimiter),
            }),
        };

        let strings: Vec<String> = array_val
            .iter()
            .map(|v| match v {
                Value::String(s) => s.clone(),
                _ => format!("{:?}", v),
            })
            .collect();

        Ok(Value::String(strings.join(&delimiter_val)))
    }

    /// (replace string old new) - Replace all occurrences of old with new
    fn eval_replace(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "replace".to_string(),
                reason: format!("Expected 3 arguments (string, old, new), got {}", args.len()),
            })?;
        }

        let string = self.evaluate_expression(&args[0].value)?;
        let old = self.evaluate_expression(&args[1].value)?;
        let new = self.evaluate_expression(&args[2].value)?;

        let string_val = match string {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", string),
            }),
        };

        let old_val = match old {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", old),
            }),
        };

        let new_val = match new {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", new),
            }),
        };

        Ok(Value::String(string_val.replace(&old_val, &new_val)))
    }

    /// (trim string) - Remove leading and trailing whitespace
    fn eval_trim(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "trim".to_string(),
                reason: format!("Expected 1 argument (string), got {}", args.len()),
            })?;
        }

        let string = self.evaluate_expression(&args[0].value)?;
        let string_val = match string {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", string),
            }),
        };

        Ok(Value::String(string_val.trim().to_string()))
    }

    /// (upper string) - Convert string to uppercase
    fn eval_upper(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "upper".to_string(),
                reason: format!("Expected 1 argument (string), got {}", args.len()),
            })?;
        }

        let string = self.evaluate_expression(&args[0].value)?;
        let string_val = match string {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", string),
            }),
        };

        Ok(Value::String(string_val.to_uppercase()))
    }

    /// (lower string) - Convert string to lowercase
    fn eval_lower(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "lower".to_string(),
                reason: format!("Expected 1 argument (string), got {}", args.len()),
            })?;
        }

        let string = self.evaluate_expression(&args[0].value)?;
        let string_val = match string {
            Value::String(s) => s,
            _ => return Err(Error::TypeError {
                expected: "string".to_string(),
                got: format!("{:?}", string),
            }),
        };

        Ok(Value::String(string_val.to_lowercase()))
    }

    // =========================================================================
    // ADVANCED MATH OPERATIONS
    // =========================================================================

    /// (sqrt x) - Square root of a number
    fn eval_sqrt(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "sqrt".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }

        let val = self.evaluate_expression(&args[0].value)?;

        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => return Err(Error::TypeError {
                expected: "number (int or float)".to_string(),
                got: format!("{:?}", val),
            }),
        };

        if num < 0.0 {
            return Err(Error::InvalidArguments {
                tool: "sqrt".to_string(),
                reason: format!("Cannot take square root of negative number: {}", num),
            })?;
        }

        Ok(Value::Float(num.sqrt()))
    }

    /// (pow base exponent) - Raise base to exponent power
    fn eval_pow(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "pow".to_string(),
                reason: format!("Expected 2 arguments (base, exponent), got {}", args.len()),
            })?;
        }

        let base_val = self.evaluate_expression(&args[0].value)?;
        let exp_val = self.evaluate_expression(&args[1].value)?;

        let base = match base_val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => return Err(Error::TypeError {
                expected: "number (int or float)".to_string(),
                got: format!("{:?}", base_val),
            }),
        };

        let exponent = match exp_val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => return Err(Error::TypeError {
                expected: "number (int or float)".to_string(),
                got: format!("{:?}", exp_val),
            }),
        };

        let result = base.powf(exponent);

        // Check for overflow/invalid results
        if result.is_nan() {
            return Err(Error::InvalidArguments {
                tool: "pow".to_string(),
                reason: format!("Result is not a number (base={}, exponent={})", base, exponent),
            })?;
        }

        if result.is_infinite() {
            return Err(Error::InvalidArguments {
                tool: "pow".to_string(),
                reason: format!("Result is infinite (base={}, exponent={})", base, exponent),
            })?;
        }

        Ok(Value::Float(result))
    }

    /// (abs x) - Absolute value of a number
    fn eval_abs(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "abs".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }

        let val = self.evaluate_expression(&args[0].value)?;

        match val {
            Value::Int(i) => Ok(Value::Int(i.abs())),
            Value::Float(f) => Ok(Value::Float(f.abs())),
            _ => Err(Error::TypeError {
                expected: "number (int or float)".to_string(),
                got: format!("{:?}", val),
            }),
        }
    }

    // =========================================================================
    // MULTIPLE VALUES (Common Lisp)
    // =========================================================================

    /// (values ...) - Return multiple values
    /// In single-value context, only the first value is used
    fn eval_values(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Evaluate all arguments
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.evaluate_expression(&arg.value)?);
        }

        // Special case: (values) returns no values (null in single context)
        if values.is_empty() {
            return Ok(Value::Null);
        }

        // Special case: (values x) returns x directly (not wrapped)
        if values.len() == 1 {
            return Ok(values.into_iter().next().unwrap());
        }

        // Multiple values: wrap in Value::Multiple
        Ok(Value::multiple(values))
    }

    /// (multiple-value-bind (vars...) values-form body...)
    /// Destructure multiple values and bind to variables
    fn eval_multiple_value_bind(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "multiple-value-bind".to_string(),
                reason: format!(
                    "Expected at least 3 arguments (vars values-form body...), got {}",
                    args.len()
                ),
            })?;
        }

        // First argument must be an array of variable names
        let var_names = match &args[0].value {
            Expression::ArrayLiteral(items) => {
                let mut names = Vec::new();
                for item in items {
                    match item {
                        Expression::Variable(name) => names.push(name.clone()),
                        _ => {
                            return Err(Error::InvalidArguments {
                                tool: "multiple-value-bind".to_string(),
                                reason: "Variable list must contain only variable names"
                                    .to_string(),
                            })?
                        }
                    }
                }
                names
            }
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "multiple-value-bind".to_string(),
                    reason: "First argument must be an array of variable names".to_string(),
                })?
            }
        };

        // Second argument is the values-form to evaluate
        let values_result = self.evaluate_expression(&args[1].value)?;

        // Extract values from result (handle both Multiple and single values)
        let values = match values_result {
            Value::Multiple(vals) => vals.as_ref().clone(),
            single => vec![single],
        };

        // Enter new scope for bindings
        self.env.enter_scope();

        // Bind variables (extra values ignored, missing vars bound to null)
        for (i, var_name) in var_names.iter().enumerate() {
            let value = values.get(i).cloned().unwrap_or(Value::Null);
            let _ = self.env.set(var_name, value);
        }

        // Execute body expressions in sequence, return last
        let mut result = Value::Null;
        for i in 2..args.len() {
            result = self.evaluate_expression(&args[i].value)?;
        }

        self.env.exit_scope();

        Ok(result)
    }

    // =========================================================================
    // DYNAMIC VARIABLES (Common Lisp special variables)
    // =========================================================================

    /// (defvar *name* initial-value) - Define a dynamic (special) variable
    /// Convention: use *earmuffs* for dynamic variable names
    fn eval_defvar(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "defvar".to_string(),
                reason: format!("Expected 2 arguments (name value), got {}", args.len()),
            })?;
        }

        // First argument must be a variable name
        let var_name = match &args[0].value {
            Expression::Variable(name) => name.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "defvar".to_string(),
                    reason: "First argument must be a variable name".to_string(),
                })?
            }
        };

        // Evaluate the initial value
        let initial_value = self.evaluate_expression(&args[1].value)?;

        // Define in the dynamic environment
        self.env.defvar(var_name.clone(), initial_value.clone());

        // Return the defined value
        Ok(initial_value)
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

    /// (first coll) - Get first element of collection
    fn eval_first(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "first".to_string(),
                reason: "Expected 1 argument (collection)".to_string(),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        match val {
            Value::Array(ref arr) => arr.first().cloned().ok_or(Error::IndexOutOfBounds {
                index: 0,
                length: 0,
            }),
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: val.type_name(),
            }),
        }
    }

    /// (rest coll) - Get all elements except first
    fn eval_rest(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "rest".to_string(),
                reason: "Expected 1 argument (collection)".to_string(),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        match val {
            Value::Array(ref arr) => {
                if arr.is_empty() {
                    Ok(Value::Array(Arc::new(vec![])))
                } else {
                    Ok(Value::Array(Arc::new(arr[1..].to_vec())))
                }
            }
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: val.type_name(),
            }),
        }
    }

    /// (cons elem coll) - Prepend element to collection
    fn eval_cons(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "cons".to_string(),
                reason: "Expected 2 arguments (element, collection)".to_string(),
            });
        }

        let elem = self.evaluate_expression(&args[0].value)?;
        let coll = self.evaluate_expression(&args[1].value)?;

        match coll {
            Value::Array(ref arr) => {
                let mut new_arr = vec![elem];
                new_arr.extend(arr.iter().cloned());
                Ok(Value::Array(Arc::new(new_arr)))
            }
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: coll.type_name(),
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

                // Bind parameters (supports &rest)
                self.env.enter_scope();
                self.bind_function_parameters(&params, &evaluated_args, name)?;

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

    /// (gensym) or (gensym "prefix") - Generate unique symbol
    /// Used in macros to prevent variable capture (hygiene)
    fn eval_gensym(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        let prefix = if args.is_empty() {
            "G".to_string()
        } else {
            let prefix_val = self.evaluate_expression(&args[0].value)?;
            prefix_val.as_string()?.to_string()
        };

        let counter = self.gensym_counter.get();
        self.gensym_counter.set(counter + 1);

        Ok(Value::String(format!("{}__{}", prefix, counter)))
    }

    /// (macroexpand form) - Expand macro once (debugging tool)
    fn eval_macroexpand(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "macroexpand".to_string(),
                reason: "Expected 1 argument: form to expand".to_string(),
            });
        }

        // Try to expand the expression once
        match self.try_expand_macro(&args[0].value)? {
            Some(expanded) => {
                // Convert expanded expression back to a displayable value
                // For now, return a string representation
                Ok(Value::String(format!("{:?}", expanded)))
            }
            None => {
                // Not a macro call, return original
                Ok(Value::String(format!("{:?}", args[0].value)))
            }
        }
    }

    /// Try to expand a macro call once
    /// Returns Some(expanded_expr) if it's a macro call, None otherwise
    fn try_expand_macro(&mut self, expr: &Expression) -> Result<Option<Expression>> {
        match expr {
            Expression::ToolCall { name, args } => {
                // Check if this is a macro
                if let Ok(value) = self.env.get(name) {
                    if let Value::Macro { params, body, .. } = value {
                        // This is a macro! Expand it
                        return Ok(Some(self.expand_macro(&params, &body, args)?));
                    }
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    /// Expand a macro by binding unevaluated arguments to parameters
    /// and evaluating the macro body, which returns code
    fn expand_macro(
        &mut self,
        params: &[String],
        body: &Expression,
        args: &[crate::parser::Argument],
    ) -> Result<Expression> {
        // Save old environment
        let old_env = self.env.clone();

        // Bind parameters to UNEVALUATED arguments (supports &rest)
        // Convert args to expression values first
        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.expression_to_value(&arg.value)?);
        }
        self.bind_function_parameters(params, &arg_values, "macro")?;

        // Evaluate macro body (which generates code)
        let result_value = self.evaluate_expression(body)?;

        // Restore environment
        self.env = old_env;

        // Convert result back to an expression
        self.value_to_expression(&result_value)
    }

    /// Convert an expression to a value (for macro parameter binding)
    fn expression_to_value(&self, expr: &Expression) -> Result<Value> {
        // This is a simplified version - in full CL, expressions would be first-class
        // For now, we store them as strings or structured data
        match expr {
            Expression::IntLiteral(n) => Ok(Value::Int(*n)),
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
            Expression::NullLiteral => Ok(Value::Null),
            Expression::Variable(name) => Ok(Value::String(name.clone())),
            Expression::ArrayLiteral(exprs) => {
                let vals: Result<Vec<_>> = exprs.iter().map(|e| self.expression_to_value(e)).collect();
                Ok(Value::array(vals?))
            }
            _ => {
                // For complex expressions, represent as string (simplified)
                Ok(Value::String(format!("{:?}", expr)))
            }
        }
    }

    /// Convert a value back to an expression (for macro expansion result)
    fn value_to_expression(&self, value: &Value) -> Result<Expression> {
        match value {
            Value::Int(n) => Ok(Expression::IntLiteral(*n)),
            Value::Float(f) => Ok(Expression::FloatLiteral(*f)),
            Value::String(s) => {
                // Try to interpret as variable name if it's an identifier
                if s.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
                    Ok(Expression::Variable(s.clone()))
                } else {
                    Ok(Expression::StringLiteral(s.clone()))
                }
            }
            Value::Bool(b) => Ok(Expression::BoolLiteral(*b)),
            Value::Null => Ok(Expression::NullLiteral),
            Value::Array(arr) => {
                let exprs: Result<Vec<_>> = arr.iter().map(|v| self.value_to_expression(v)).collect();
                Ok(Expression::ArrayLiteral(exprs?))
            }
            _ => Err(Error::TypeError {
                expected: "simple value".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// Evaluate quasiquote expression (template with unquote/splice)
    fn eval_quasiquote(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::Quasiquote(inner) => {
                // Process the template, evaluating unquotes
                self.process_quasiquote_template(inner)
            }
            _ => Err(Error::ParseError("Expected quasiquote expression".to_string())),
        }
    }

    /// Process quasiquote template, handling unquote and unquote-splice
    fn process_quasiquote_template(&mut self, expr: &Expression) -> Result<Value> {
        match expr {
            Expression::Unquote(inner) => {
                // Evaluate the unquoted expression
                self.evaluate_expression(inner)
            }
            Expression::UnquoteSplice(inner) => {
                // Evaluate and expect an array to splice
                let val = self.evaluate_expression(inner)?;
                match val {
                    Value::Array(_) => Ok(val),
                    _ => Err(Error::TypeError {
                        expected: "array for unquote-splice".to_string(),
                        got: val.type_name(),
                    }),
                }
            }
            Expression::ArrayLiteral(elements) => {
                // Process each element, handling splicing
                let mut result = Vec::new();
                for elem in elements {
                    if let Expression::UnquoteSplice(inner) = elem {
                        // Splice array elements
                        let val = self.evaluate_expression(inner)?;
                        if let Value::Array(arr) = val {
                            result.extend(arr.iter().cloned());
                        } else {
                            return Err(Error::TypeError {
                                expected: "array for unquote-splice".to_string(),
                                got: val.type_name(),
                            });
                        }
                    } else {
                        // Regular element
                        result.push(self.process_quasiquote_template(elem)?);
                    }
                }
                Ok(Value::array(result))
            }
            Expression::ToolCall { name, args } => {
                // Process arguments
                let processed_args: Result<Vec<_>> = args
                    .iter()
                    .map(|arg| self.process_quasiquote_template(&arg.value))
                    .collect();
                let vals = processed_args?;

                // Create a tool call value (simplified - would need proper representation)
                let mut result = vec![Value::String(name.clone())];
                result.extend(vals);
                Ok(Value::array(result))
            }
            // For other expressions, convert to values literally
            _ => self.expression_to_value(expr),
        }
    }

    /// Parse function/macro parameters with &rest support
    /// Returns parameter list (last param may be "&rest" followed by varargs name)
    fn parse_function_parameters(&self, params_expr: &Expression, context: &str) -> Result<Vec<String>> {
        // In S-expression syntax, parameter lists are parsed as ToolCalls or ArrayLiterals
        let param_exprs = match params_expr {
            Expression::ArrayLiteral(exprs) => exprs,
            Expression::ToolCall { name, args } => {
                // Convert (name arg1 arg2) to [name, arg1, arg2]
                let mut exprs = vec![Expression::Variable(name.clone())];
                for arg in args {
                    exprs.push(arg.value.clone());
                }
                return self.parse_params_from_list(&exprs, context);
            }
            _ => return Err(Error::ParseError(format!("{}: requires parameter list", context))),
        };

        self.parse_params_from_list(param_exprs, context)
    }

    /// Helper to parse parameter list from expression vector
    fn parse_params_from_list(&self, param_exprs: &[Expression], context: &str) -> Result<Vec<String>> {
                let mut param_names = Vec::new();
                let mut found_rest = false;

                for (i, param_expr) in param_exprs.iter().enumerate() {
                    if let Expression::Variable(name) = param_expr {
                        // Check if this is &rest marker
                        if name == "&rest" {
                            if found_rest {
                                return Err(Error::ParseError(format!("{}: only one &rest allowed", context)));
                            }
                            if i == param_exprs.len() - 1 {
                                return Err(Error::ParseError(format!("{}: &rest must be followed by parameter name", context)));
                            }
                            found_rest = true;
                            param_names.push(name.clone());
                        } else if found_rest && param_names.last() == Some(&"&rest".to_string()) {
                            // This is the varargs parameter name after &rest
                            param_names.push(name.clone());
                        } else if found_rest {
                            return Err(Error::ParseError(format!("{}: &rest must be last parameter", context)));
                        } else {
                            param_names.push(name.clone());
                        }
                    } else {
                        return Err(Error::ParseError(format!("{}: parameters must be identifiers", context)));
                    }
                }

                Ok(param_names)
    }

    /// Bind function/macro parameters to arguments (supports &rest)
    fn bind_function_parameters(&mut self, params: &[String], args: &[Value], context: &str) -> Result<()> {
        // Check if we have &rest
        let rest_pos = params.iter().position(|p| p == "&rest");

        if let Some(rest_idx) = rest_pos {
            // We have &rest - collect varargs
            if rest_idx + 1 >= params.len() {
                return Err(Error::ParseError(format!("{}: &rest must be followed by parameter name", context)));
            }

            let required_count = rest_idx;
            let rest_param_name = &params[rest_idx + 1];

            // Check we have at least the required arguments
            if args.len() < required_count {
                return Err(Error::InvalidArguments {
                    tool: context.to_string(),
                    reason: format!("Expected at least {} arguments, got {}", required_count, args.len()),
                });
            }

            // Bind required parameters
            for i in 0..required_count {
                self.env.define(params[i].clone(), args[i].clone());
            }

            // Collect remaining arguments into array for rest parameter
            let rest_args: Vec<Value> = args[required_count..].to_vec();
            self.env.define(rest_param_name.clone(), Value::array(rest_args));

        } else {
            // No &rest - exact parameter count required
            if params.len() != args.len() {
                return Err(Error::InvalidArguments {
                    tool: context.to_string(),
                    reason: format!("Expected {} arguments, got {}", params.len(), args.len()),
                });
            }

            // Bind all parameters
            for (param, arg) in params.iter().zip(args.iter()) {
                self.env.define(param.clone(), arg.clone());
            }
        }

        Ok(())
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
