use crate::error::{Error, Result};
use crate::parser::{
    AccumulationClause, BinaryOp, ConditionClause, ExitClause, Expression, IterationClause,
    LoopData, Program, Statement, UnaryOp,
};
use crate::runtime::{Environment, Value};
use crate::tools::ToolRegistry;
use base64::Engine;
use sha2::{Digest, Sha256, Sha512};
use std::collections::HashMap;
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
    /// Lazy field access configuration
    lazy_field_config: std::cell::RefCell<LazyFieldConfig>,
    /// Execution trace for debugging (variable_name -> value)
    execution_trace: std::cell::RefCell<Vec<(String, Value)>>,
}

/// Configuration for lazy field access behavior
#[derive(Clone, Debug)]
struct LazyFieldConfig {
    /// Search strategy: true for breadth-first, false for depth-first
    breadth_first: bool,
    /// Strict mode: error on missing fields instead of returning null
    strict: bool,
    /// Maximum search depth to prevent infinite recursion
    max_depth: usize,
}

impl Default for LazyFieldConfig {
    fn default() -> Self {
        LazyFieldConfig {
            breadth_first: false, // Default to depth-first (current behavior)
            strict: false,        // Default to lenient (returns null)
            max_depth: 50,        // Reasonable default for nested structures
        }
    }
}

impl LispEvaluator {
    /// Creates a new LISP evaluator
    pub fn new() -> Self {
        LispEvaluator {
            env: Environment::new(),
            registry: Arc::new(ToolRegistry::new()),
            gensym_counter: std::cell::Cell::new(0),
            lazy_field_config: std::cell::RefCell::new(LazyFieldConfig::default()),
            execution_trace: std::cell::RefCell::new(Vec::new()),
        }
    }

    /// Creates a new LISP evaluator with custom tool registry
    pub fn with_registry(registry: ToolRegistry) -> Self {
        LispEvaluator {
            env: Environment::new(),
            registry: Arc::new(registry),
            gensym_counter: std::cell::Cell::new(0),
            lazy_field_config: std::cell::RefCell::new(LazyFieldConfig::default()),
            execution_trace: std::cell::RefCell::new(Vec::new()),
        }
    }

    /// Get the execution trace (variable assignments)
    pub fn get_execution_trace(&self) -> Vec<(String, Value)> {
        self.execution_trace.borrow().clone()
    }

    /// Clear the execution trace
    pub fn clear_execution_trace(&self) {
        self.execution_trace.borrow_mut().clear();
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
                self.env.set(name, val.clone())?;
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
                    "setf" => self.eval_setf(args),
                    "define" => self.eval_define(args),
                    "defun" => self.eval_defun(args),
                    "defn" => self.eval_defun(args), // Alias for defun
                    "defmacro" => self.eval_defmacro(args),
                    "const" => self.eval_const(args),
                    "let" => self.eval_let(args),
                    "let*" => self.eval_let_star(args),
                    "flet" => self.eval_flet(args),
                    "labels" => self.eval_labels(args),
                    "case" => self.eval_case(args),
                    "typecase" => self.eval_typecase(args),
                    "while" => self.eval_while(args),
                    "for" => self.eval_for(args),
                    "do" => self.eval_do(args),
                    "progn" => self.eval_do(args), // progn is same as do
                    "prog1" => self.eval_prog1(args),
                    "prog2" => self.eval_prog2(args),
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
                    "list?" => self.eval_array_check(args), // Common LISP: list? is same as array?
                    "object?" => self.eval_object_check(args),
                    "function?" => self.eval_function_check(args),
                    // Generic type checking (Python/JS style)
                    "typeof" => self.eval_typeof(args), // JS: typeof value
                    "type-of" => self.eval_typeof(args), // LISP: type-of
                    // Number predicates (Common LISP style)
                    "even?" => self.eval_even(args), // (even? 4) -> true
                    "evenp" => self.eval_even(args), // Common LISP: evenp
                    "odd?" => self.eval_odd(args),   // (odd? 3) -> true
                    "oddp" => self.eval_odd(args),   // Common LISP: oddp
                    "positive?" => self.eval_positive(args), // (positive? 5) -> true
                    "positivep" => self.eval_positive(args), // Common LISP
                    "negative?" => self.eval_negative(args), // (negative? -5) -> true
                    "negativep" => self.eval_negative(args), // Common LISP
                    "zero?" => self.eval_zero(args), // (zero? 0) -> true
                    "zerop" => self.eval_zero(args), // Common LISP: zerop
                    // Type conversions (AI compatibility - Python/JS style)
                    "int" => self.eval_to_int(args), // Python: int("42") -> 42
                    "integer" => self.eval_to_int(args), // Alias
                    "parse-int" => self.eval_to_int(args), // JS: parseInt("42")
                    "parseint" => self.eval_to_int(args), // JS: parseInt (lowercase)
                    "float" => self.eval_to_float(args), // Python: float("3.14") -> 3.14
                    "parse-float" => self.eval_to_float(args), // JS: parseFloat("3.14")
                    "parsefloat" => self.eval_to_float(args), // JS: parseFloat (lowercase)
                    "bool" => self.eval_to_bool(args), // Python: bool("true") -> True
                    // Assertions
                    "assert" => self.eval_assert(args),
                    "assert-type" => self.eval_assert_type(args),
                    // Cryptography and encoding
                    "base58-decode" => self.eval_base58_decode(args),
                    "base58-encode" => self.eval_base58_encode(args),
                    "base64-decode" => self.eval_base64_decode(args),
                    "base64-decode-raw" => self.eval_base64_decode_raw(args),
                    "base64-encode" => self.eval_base64_encode(args),
                    "hex-decode" => self.eval_hex_decode(args),
                    "hex-encode" => self.eval_hex_encode(args),
                    "sha256" => self.eval_sha256(args),
                    "sha512" => self.eval_sha512(args),
                    // Binary/byte operations for Borsh decoding
                    "byte-at" => self.eval_byte_at(args),
                    "parse-u64-le" => self.eval_parse_u64_le(args),
                    "hex-to-u64-le" => self.eval_hex_to_u64_le(args),
                    "bytes-to-hex" => self.eval_bytes_to_hex(args),
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
                    "expt" => self.eval_pow(args), // Common Lisp alias for pow
                    "exp" => self.eval_exp(args),  // e^x
                    "ln" => self.eval_ln(args),    // Natural logarithm
                    "abs" => self.eval_abs(args),
                    // Common Lisp arithmetic shortcuts
                    "1+" => self.eval_1_plus(args),
                    "1-" => self.eval_1_minus(args),
                    "mod" => self.eval_mod(args),
                    "rem" => self.eval_rem(args),
                    "gcd" => self.eval_gcd(args),
                    "lcm" => self.eval_lcm(args),
                    // Common Lisp list predicates
                    "atom" => self.eval_atom(args),
                    "consp" => self.eval_consp(args),
                    "listp" => self.eval_listp(args),
                    // Common Lisp bitwise operations
                    "logand" => self.eval_logand(args),
                    "logior" => self.eval_logior(args),
                    "logxor" => self.eval_logxor(args),
                    "lognot" => self.eval_lognot(args),
                    "ash" => self.eval_ash(args),
                    // Common Lisp list operations
                    "member" => self.eval_member(args),
                    "assoc" => self.eval_assoc(args),
                    "assoc-in" => self.eval_assoc_in(args),  // Set key in object (dynamic key)
                    "set-key" => self.eval_assoc_in(args),   // Alias for assoc-in
                    "set" => self.eval_object_set(args),     // set(obj, key, value) - like JS/Python
                    "elt" => self.eval_elt(args),
                    "subseq" => self.eval_subseq(args),
                    // Common Lisp string comparisons
                    "string=" => self.eval_string_eq(args),
                    "string<" => self.eval_string_lt(args),
                    "string>" => self.eval_string_gt(args),
                    "string-equal" => self.eval_string_eq(args), // Alternative name
                    "string-lessp" => self.eval_string_lt(args), // Alternative name
                    "string-greaterp" => self.eval_string_gt(args), // Alternative name
                    // Common Lisp map variants
                    "mapcar" => self.eval_mapcar(args),
                    "mapc" => self.eval_mapc(args),
                    // Common Lisp conditional filters
                    "remove-if" => self.eval_remove_if(args),
                    "remove-if-not" => self.eval_remove_if_not(args),
                    // Common Lisp variable mutation
                    "incf" => self.eval_incf(args),
                    "decf" => self.eval_decf(args),
                    // Trigonometric functions
                    "sin" => self.eval_sin(args),
                    "cos" => self.eval_cos(args),
                    "tan" => self.eval_tan(args),
                    "asin" => self.eval_asin(args),
                    "acos" => self.eval_acos(args),
                    "atan" => self.eval_atan(args),
                    "atan2" => self.eval_atan2(args),
                    // Rounding functions
                    "floor" => self.eval_floor(args),
                    "ceiling" => self.eval_ceiling(args),
                    "ceil" => self.eval_ceiling(args), // Alias
                    "round" => self.eval_round(args),
                    "truncate" => self.eval_truncate(args),
                    "trunc" => self.eval_truncate(args), // Alias
                    // Multiple values (Common Lisp style)
                    "values" => self.eval_values(args),
                    "multiple-value-bind" => self.eval_multiple_value_bind(args),
                    // Dynamic variables (Common Lisp special variables)
                    "defvar" => self.eval_defvar(args),
                    // Macro system
                    "gensym" => self.eval_gensym(args),
                    "macroexpand" => self.eval_macroexpand(args),
                    "eval" => self.eval_eval(args),
                    "length" => self.eval_length(args),
                    "count" => self.eval_length(args), // Alias for length - commonly expected
                    "last" => self.eval_last(args),
                    "range" => self.eval_range(args),
                    "min" => self.eval_min(args),
                    "max" => self.eval_max(args),
                    // Statistical functions (Python/NumPy style)
                    "mean" => self.eval_mean(args),         // Average
                    "average" => self.eval_mean(args),      // Alias
                    "avg" => self.eval_mean(args),          // SQL-style
                    "median" => self.eval_median(args),     // Median value
                    "mode" => self.eval_mode(args),         // Most common value
                    "product" => self.eval_product(args),   // Product of numbers
                    "std" => self.eval_stddev(args),        // Standard deviation
                    "stddev" => self.eval_stddev(args),     // Alias
                    "variance" => self.eval_variance(args), // Variance
                    // Math utilities
                    "sign" => self.eval_sign(args), // Sign of number (-1, 0, 1)
                    "clamp" => self.eval_clamp(args), // Clamp between min/max
                    "random" => self.eval_random(args), // Random number
                    "now" => self.eval_now(args),
                    "sleep" => self.eval_sleep(args),
                    "log" => self.eval_log(args),
                    "print" => self.eval_print(args), // Python/JS-style output
                    "println" => self.eval_println(args), // Python/JS-style output with newline
                    "map" => self.eval_map(args),
                    "filter" => self.eval_filter(args),
                    "reduce" => self.eval_reduce(args),
                    "sort" => self.eval_sort(args),
                    "group-by" => self.eval_group_by(args),
                    "aggregate" => self.eval_aggregate(args),
                    "sort-by" => self.eval_sort_by(args),
                    "str" => self.eval_str(args),
                    "format" => self.eval_format(args),
                    "slice" => self.eval_slice(args),
                    "keys" => self.eval_keys(args),
                    "object-values" => self.eval_object_values(args), // Python: dict.values()
                    "object-entries" => self.eval_object_entries(args), // Python: dict.items()
                    "entries" => self.eval_object_entries(args),      // JS: Object.entries()
                    "items" => self.eval_object_entries(args),        // Python: dict.items()
                    "merge" => self.eval_merge(args),
                    "put" => self.eval_put(args),           // Set object property: (put obj "key" val)
                    "get" => self.eval_get(args),
                    "get-path" => self.eval_get_path(args),
                    "discover" => self.eval_discover(args),
                    "lazy-config" => self.eval_lazy_config(args),
                    "first" => self.eval_first(args),
                    "head" => self.eval_first(args), // Alias for first (Haskell-style)
                    "rest" => self.eval_rest(args),
                    "tail" => self.eval_rest(args), // Alias for rest (Haskell-style)
                    "init" => self.eval_init(args), // All but last (Haskell-style)
                    "shift" => self.eval_shift(args), // Remove first element (JS-style)
                    "unshift" => self.eval_unshift(args), // Add to front (JS-style)
                    "nth" => self.eval_nth(args),
                    "cons" => self.eval_cons(args),
                    "append" => self.eval_append(args),
                    "concat" => self.eval_concatenate(args),      // Alias for concatenate
                    "concatenate" => self.eval_concatenate(args), // Polymorphic concat
                    // JSON operations (built-ins, not MCP tools!)
                    "parse-json" => self.eval_parse_json(args),
                    "json-stringify" => self.eval_json_stringify(args),
                    // Network operations (async)
                    "http-get" => self.eval_http_get(args),
                    "http-post" => self.eval_http_post(args),
                    "json-rpc" => self.eval_json_rpc(args),
                    // Streaming operations (real-time blockchain events)
                    "stream-connect" => self.eval_stream_connect(args),
                    "stream-poll" => self.eval_stream_poll(args),
                    "stream-wait" => self.eval_stream_wait(args),
                    "stream-close" => self.eval_stream_close(args),
                    "osvm-stream" => self.eval_osvm_stream(args),
                    // LINQ-style functional operations
                    "compact" => self.eval_compact(args),
                    "count-by" => self.eval_count_by(args),
                    "distinct" => self.eval_distinct(args),
                    "unique" => self.eval_distinct(args), // Alias for distinct (SQL-style)
                    "drop" => self.eval_drop(args),
                    "every" => self.eval_every(args),
                    "all" => self.eval_every(args), // Alias for every (JavaScript-style)
                    "find" => self.eval_find(args),
                    "find-index" => self.eval_find_index(args), // Find index matching predicate
                    "indexof" => self.eval_indexof(args),       // JS-style indexOf
                    "index-of" => self.eval_indexof(args),      // Lisp-style index-of
                    "contains" => self.eval_contains(args),     // Python-style contains
                    "string-contains" => self.eval_contains(args), // Explicit string-contains
                    "elem" => self.eval_contains(args),         // Haskell-style elem
                    "remove" => self.eval_remove(args),         // Remove element by value
                    "insert-at" => self.eval_insert_at(args),   // Insert at index
                    "flatten" => self.eval_flatten(args),
                    // "group-by" already handled above on line 186
                    "partition" => self.eval_partition(args),
                    "pluck" => self.eval_pluck(args),
                    "reverse" => self.eval_reverse(args),
                    "repeat" => self.eval_repeat(args), // Python: "x"*3, JS: "x".repeat(3)
                    "some" => self.eval_some(args),
                    "any" => self.eval_some(args), // Alias for some (JavaScript-style)
                    "take" => self.eval_take(args),
                    "zip" => self.eval_zip(args),
                    // String predicates (Python str methods)
                    "isdigit?" => self.eval_isdigit(args),
                    "is-digit?" => self.eval_isdigit(args),
                    "isalpha?" => self.eval_isalpha(args),
                    "is-alpha?" => self.eval_isalpha(args),
                    "isalnum?" => self.eval_isalnum(args),
                    "is-alnum?" => self.eval_isalnum(args),
                    "isspace?" => self.eval_isspace(args),
                    "is-space?" => self.eval_isspace(args),
                    "blank?" => self.eval_blank(args),
                    // Functional programming utilities
                    "apply" => self.eval_apply(args),
                    "compose" => self.eval_compose(args),
                    "pipe" => self.eval_pipe(args),
                    "partial" => self.eval_partial(args),
                    // Regex operations
                    "regex-match" => self.eval_regex_match(args),
                    "regex-replace" => self.eval_regex_replace(args),
                    "regex-split" => self.eval_regex_split(args),
                    "regex-find-all" => self.eval_regex_find_all(args),

                    // HIGH PRIORITY ALIASES - Python/JavaScript compatibility
                    "len" => self.eval_length(args), // Python len()
                    "includes" => self.eval_contains(args), // JavaScript includes()
                    "toLowerCase" | "tolowercase" => self.eval_to_lower_case(args), // JavaScript
                    "toUpperCase" | "touppercase" => self.eval_to_upper_case(args), // JavaScript
                    "charAt" | "charat" => self.eval_char_at(args), // JavaScript charAt()
                    "chr" => self.eval_chr(args),    // Python chr()
                    "ord" => self.eval_ord(args),    // Python ord()
                    "substring" => self.eval_substring(args), // JavaScript substring()

                    // MEDIUM PRIORITY ALIASES - LISP/Haskell compatibility
                    "cdr" => self.eval_rest(args), // LISP cdr
                    "foldl" | "fold-left" => self.eval_reduce(args), // Haskell foldl
                    "foldr" | "fold-right" => self.eval_reduce(args), // Haskell foldr
                    "lastIndexOf" | "lastindexof" => self.eval_last_index_of(args), // JavaScript

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
                // Keywords (starting with :) evaluate to themselves as strings
                if name.starts_with(':') {
                    Ok(Value::String(name.clone()))
                } else {
                    self.env.get(name)
                }
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

            Expression::Lambda { params, body } => {
                let closure = Arc::new(std::collections::HashMap::new());
                Ok(Value::Function {
                    params: params.clone(),
                    body: Arc::new((**body).clone()),
                    closure,
                    is_flet: false,
                })
            }

            Expression::Loop(loop_data) => self.eval_loop(loop_data),

            Expression::Catch { tag, body } => self.eval_catch(tag, body),

            Expression::Throw { tag, value } => self.eval_throw(tag, value),

            Expression::DestructuringBind {
                pattern,
                value,
                body,
            } => self.eval_destructuring_bind(pattern, value, body),

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

    /// (setf place value) - Generalized assignment
    /// Can set variables, array elements, object fields, etc.
    fn eval_setf(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "setf".to_string(),
                reason: "Expected 2 arguments: place and value".to_string(),
            });
        }

        // Evaluate the value first
        let value = self.evaluate_expression(&args[1].value)?;

        // Handle different types of places
        match &args[0].value {
            // Simple variable: (setf x 10)
            Expression::Variable(name) => {
                self.env.set(name, value.clone())?;
                Ok(value)
            }

            // Function call form (for generalized references)
            Expression::ToolCall {
                name,
                args: place_args,
            } => {
                match name.as_str() {
                    // (setf (first list) value) - set first element
                    "first" | "car" => {
                        if place_args.len() != 1 {
                            return Err(Error::InvalidArguments {
                                tool: "setf".to_string(),
                                reason: "first/car requires 1 argument".to_string(),
                            });
                        }

                        // Get the list
                        let list_val = self.evaluate_expression(&place_args[0].value)?;
                        if let Value::Array(arr) = list_val {
                            let mut new_arr = arr.to_vec();
                            if new_arr.is_empty() {
                                return Err(Error::InvalidArguments {
                                    tool: "setf".to_string(),
                                    reason: "Cannot set first of empty array".to_string(),
                                });
                            }
                            new_arr[0] = value.clone();

                            // Set the variable back
                            if let Expression::Variable(var_name) = &place_args[0].value {
                                self.env.set(var_name, Value::Array(Arc::new(new_arr)))?;
                            }
                            Ok(value)
                        } else {
                            Err(Error::TypeError {
                                expected: "array".to_string(),
                                got: list_val.type_name().to_string(),
                            })
                        }
                    }

                    // For now, other setf forms just fall back to regular set
                    _ => Err(Error::NotImplemented {
                        tool: format!("setf for {}", name),
                    }),
                }
            }

            _ => Err(Error::ParseError("setf requires valid place".to_string())),
        }
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
            _ => {
                return Err(Error::ParseError(
                    "define requires variable name".to_string(),
                ))
            }
        };

        let value = self.evaluate_expression(&args[1].value)?;
        self.env.define(var_name.clone(), value.clone());

        // Record in execution trace for debugging
        self.execution_trace.borrow_mut().push((var_name, value.clone()));

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
            _ => {
                return Err(Error::ParseError(
                    "defun requires function name".to_string(),
                ))
            }
        };

        // Get parameters list (supports &rest)
        let params = self.parse_function_parameters(&args[1].value, "defun")?;

        // Create function value
        let func_value = Value::Function {
            params,
            body: Arc::new(args[2].value.clone()),
            closure: Arc::new(std::collections::HashMap::new()),
            is_flet: false,
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
            _ => {
                return Err(Error::ParseError(
                    "defmacro requires macro name".to_string(),
                ))
            }
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
                                _ => {
                                    return Err(Error::ParseError(
                                        "let binding requires variable name".to_string(),
                                    ))
                                }
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

        // Parallel binding: Evaluate ALL values in outer scope BEFORE entering new scope
        let mut evaluated_bindings = Vec::new();
        for (var_name, value_expr) in bindings {
            let value = self.evaluate_expression(value_expr)?;
            evaluated_bindings.push((var_name, value));
        }

        // Create new scope
        self.env.enter_scope();

        // Bind all variables in new scope
        for (var_name, value) in evaluated_bindings {
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
                                _ => {
                                    return Err(Error::ParseError(
                                        "let* binding requires variable name".to_string(),
                                    ))
                                }
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
                    "flet requires function definitions list: ((name (params) body)...)"
                        .to_string(),
                ))
            }
        };

        // Parse each function definition
        let mut functions: Vec<(String, Vec<String>, Expression)> = Vec::new();

        for func_def in func_defs {
            match func_def {
                Expression::ArrayLiteral(parts) => {
                    if parts.len() == 3 {
                        // Extract name
                        let name = match &parts[0] {
                            Expression::Variable(n) => n.clone(),
                            _ => {
                                return Err(Error::ParseError(
                                    "flet function definition requires name".to_string(),
                                ))
                            }
                        };

                        // Extract parameters
                        let params = self.parse_function_parameters(&parts[1], "flet")?;

                        // Extract body (clone it)
                        let body = parts[2].clone();

                        functions.push((name, params, body));
                    } else {
                        return Err(Error::ParseError(format!(
                            "flet function definition must have 3 parts (name params body), got {}",
                            parts.len()
                        )));
                    }
                }
                _ => {
                    return Err(Error::ParseError(
                        "flet function definitions must be: (name (params) body)".to_string(),
                    ))
                }
            }
        }

        // Capture outer environment BEFORE creating flet scope
        // This ensures flet functions can't see themselves or each other
        let outer_env = self.env.current_env_snapshot();

        // Create new scope for local functions
        self.env.enter_scope();

        // Bind functions with closure over outer environment (non-recursively)
        for (name, params, body) in functions {
            let func_value = Value::Function {
                params,
                body: Arc::new(body),
                closure: Arc::new(outer_env.clone()),
                is_flet: true, // Mark as flet for isolated execution
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

    /// (labels ((name (params) body)...) body) - Recursive local function definitions
    /// Unlike flet, functions CAN call themselves and each other
    fn eval_labels(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "labels".to_string(),
                reason: "Expected at least 2 arguments: function definitions and body".to_string(),
            });
        }

        // Parse function definitions (first argument should be array of function defs)
        let func_defs = match &args[0].value {
            Expression::ArrayLiteral(defs) => defs,
            _ => {
                return Err(Error::ParseError(
                    "labels requires function definitions list: ((name (params) body)...)"
                        .to_string(),
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
                        _ => {
                            return Err(Error::ParseError(
                                "labels function definition requires name".to_string(),
                            ))
                        }
                    };

                    // Extract parameters
                    let params = self.parse_function_parameters(&parts[1], "labels")?;

                    // Extract body (clone it)
                    let body = parts[2].clone();

                    functions.push((name, params, body));
                }
                _ => {
                    return Err(Error::ParseError(
                        "labels function definitions must be: (name (params) body)".to_string(),
                    ))
                }
            }
        }

        // Create new scope for local functions
        self.env.enter_scope();

        // TWO-PASS BINDING for recursion:
        // Pass 1: Bind function names with placeholder values
        // This allows functions to see each other's names
        for (name, _, _) in &functions {
            self.env.define(name.clone(), Value::Null);
        }

        // Pass 2: Create actual function closures
        // Now each function's closure includes all the function names
        let labels_env = self.env.current_env_snapshot();

        for (name, params, body) in functions {
            let func_value = Value::Function {
                params,
                body: Arc::new(body),
                closure: Arc::new(labels_env.clone()),
                is_flet: false, // labels allows recursion
            };
            // Update the binding with the real function
            self.env.set(&name, func_value)?;
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

    /// (case expr (value result)... (else default)) - Pattern matching by value
    fn eval_case(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "case".to_string(),
                reason: "Expected at least 2 arguments: test expression and clauses".to_string(),
            });
        }

        // Evaluate the test expression
        let test_value = self.evaluate_expression(&args[0].value)?;

        // Process each clause
        for arg in &args[1..] {
            match &arg.value {
                Expression::ArrayLiteral(clause) if clause.len() == 2 => {
                    // Check if this is an else clause
                    if let Expression::Variable(var) = &clause[0] {
                        if var == "else" || var == "otherwise" || var == "t" {
                            // Else clause matches everything
                            return self.evaluate_expression(&clause[1]);
                        }
                    }

                    // Match pattern (can be single value or list of values)
                    let matches = match &clause[0] {
                        // Single value to match
                        Expression::Variable(_)
                        | Expression::IntLiteral(_)
                        | Expression::FloatLiteral(_)
                        | Expression::StringLiteral(_)
                        | Expression::BoolLiteral(_) => {
                            let pattern_value = self.evaluate_expression(&clause[0])?;
                            self.values_equal(&test_value, &pattern_value)
                        }
                        // Multiple values to match (any can match)
                        Expression::ArrayLiteral(patterns) => {
                            let mut any_match = false;
                            for pattern in patterns {
                                let pattern_value = self.evaluate_expression(pattern)?;
                                if self.values_equal(&test_value, &pattern_value) {
                                    any_match = true;
                                    break;
                                }
                            }
                            any_match
                        }
                        _ => {
                            let pattern_value = self.evaluate_expression(&clause[0])?;
                            self.values_equal(&test_value, &pattern_value)
                        }
                    };

                    if matches {
                        return self.evaluate_expression(&clause[1]);
                    }
                }
                _ => {
                    return Err(Error::ParseError(
                        "case clauses must be (pattern result) pairs".to_string(),
                    ))
                }
            }
        }

        // No match found and no else clause
        Ok(Value::Null)
    }

    /// (typecase expr (type result)... (else default)) - Pattern matching by type
    fn eval_typecase(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "typecase".to_string(),
                reason: "Expected at least 2 arguments: test expression and clauses".to_string(),
            });
        }

        // Evaluate the test expression
        let test_value = self.evaluate_expression(&args[0].value)?;
        let test_type = test_value.type_name();

        // Process each clause
        for arg in &args[1..] {
            match &arg.value {
                Expression::ArrayLiteral(clause) if clause.len() == 2 => {
                    // Array literal syntax: [type result]
                    if let Expression::Variable(var) = &clause[0] {
                        if var == "else" || var == "otherwise" || var == "t" {
                            return self.evaluate_expression(&clause[1]);
                        }
                    }

                    let type_match = match &clause[0] {
                        Expression::Variable(type_name) => self.type_matches(&test_type, type_name),
                        Expression::NullLiteral => {
                            // null literal in pattern position matches null type
                            test_type == "null"
                        }
                        Expression::ArrayLiteral(types) => {
                            let mut any_match = false;
                            for type_expr in types {
                                if let Expression::Variable(type_name) = type_expr {
                                    if self.type_matches(&test_type, type_name) {
                                        any_match = true;
                                        break;
                                    }
                                }
                            }
                            any_match
                        }
                        _ => false,
                    };

                    if type_match {
                        return self.evaluate_expression(&clause[1]);
                    }
                }
                Expression::ToolCall {
                    args: clause_args, ..
                } if clause_args.len() == 2 => {
                    // Parenthesized syntax: (type result)
                    if let Expression::Variable(var) = &clause_args[0].value {
                        if var == "else" || var == "otherwise" || var == "t" {
                            return self.evaluate_expression(&clause_args[1].value);
                        }
                    }

                    let type_match = match &clause_args[0].value {
                        Expression::Variable(type_name) => self.type_matches(&test_type, type_name),
                        Expression::NullLiteral => {
                            // null literal in pattern position matches null type
                            test_type == "null"
                        }
                        Expression::ArrayLiteral(types) => {
                            let mut any_match = false;
                            for type_expr in types {
                                if let Expression::Variable(type_name) = type_expr {
                                    if self.type_matches(&test_type, type_name) {
                                        any_match = true;
                                        break;
                                    }
                                }
                            }
                            any_match
                        }
                        _ => false,
                    };

                    if type_match {
                        return self.evaluate_expression(&clause_args[1].value);
                    }
                }
                _ => {
                    return Err(Error::ParseError(
                        "typecase clauses must be (type result) pairs".to_string(),
                    ))
                }
            }
        }

        // No match found and no else clause
        Ok(Value::Null)
    }

    /// Helper: Check if two values are equal (for case matching)
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => (x - y).abs() < f64::EPSILON,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }

    /// Helper: Check if a value type matches a type name
    fn type_matches(&self, value_type: &str, pattern_type: &str) -> bool {
        // Handle both singular and plural forms, and Common Lisp type names
        match pattern_type.to_lowercase().as_str() {
            "int" | "integer" | "number" => value_type == "int",
            "float" | "real" | "double" => value_type == "float",
            "string" | "str" => value_type == "string",
            "bool" | "boolean" => value_type == "bool",
            "array" | "list" | "vector" => value_type == "array",
            "object" | "hash" | "map" => value_type == "object",
            "null" | "nil" => value_type == "null",
            "function" | "fn" | "lambda" => value_type == "function",
            "macro" => value_type == "macro",
            _ => value_type == pattern_type,
        }
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
        // Get iteration limit from environment or use default (10M for streaming scripts)
        let max_iterations = std::env::var("OVSM_MAX_ITERATIONS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(10_000_000); // Default: 10 million iterations
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
                    "for syntax: (for (var collection) body...), var must be a variable name"
                        .to_string(),
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

        // DON'T create new scope - loops should share scope with parent
        // This allows set! to modify outer variables
        let mut last_val = Value::Null;
        for item in items {
            // Bind loop variable (this will shadow any existing variable with same name)
            self.env.define(var_name.clone(), item);

            // Execute body (args[2..] because args[0]=var, args[1]=collection)
            for arg in &args[2..] {
                last_val = self.evaluate_expression(&arg.value)?;
            }
        }

        Ok(last_val)
    }

    /// (do expr1 expr2 ... exprN) - Sequential execution
    fn eval_do(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        let mut last_val = Value::Null;
        for arg in args.iter() {
            last_val = self.evaluate_expression(&arg.value)?;
        }
        Ok(last_val)
    }

    /// (prog1 expr1 expr2 ...) - Evaluate all, return FIRST value
    fn eval_prog1(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }

        // Evaluate first expression and save its value
        let first_val = self.evaluate_expression(&args[0].value)?;

        // Evaluate remaining expressions (for side effects)
        for arg in &args[1..] {
            self.evaluate_expression(&arg.value)?;
        }

        // Return the first value
        Ok(first_val)
    }

    /// (prog2 expr1 expr2 expr3 ...) - Evaluate all, return SECOND value
    fn eval_prog2(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Ok(Value::Null);
        }

        // Evaluate first expression (for side effects)
        self.evaluate_expression(&args[0].value)?;

        // Evaluate second expression and save its value
        let second_val = self.evaluate_expression(&args[1].value)?;

        // Evaluate remaining expressions (for side effects)
        for arg in &args[2..] {
            self.evaluate_expression(&arg.value)?;
        }

        // Return the second value
        Ok(second_val)
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
                Expression::ToolCall {
                    name: _,
                    args: clause_args,
                } => {
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

    /// (typeof x) or (type-of x) - Return type as string
    /// Returns: "int", "float", "string", "boolean", "array", "object", "function", "null"
    fn eval_typeof(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "typeof".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            })?;
        }
        let val = self.evaluate_expression(&args[0].value)?;
        let type_str = match val {
            Value::Int(_) => "number", // JS-style: int and float both return "number"
            Value::Float(_) => "number", // JS-style
            Value::String(_) => "string",
            Value::Bool(_) => "boolean",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
            Value::Function { .. } => "function",
            Value::Null => "null",
            Value::Range { .. } => "range",
            Value::Multiple(_) => "multiple", // Common LISP multiple values
            Value::Macro { .. } => "macro",   // LISP macros
        };
        Ok(Value::String(type_str.to_string()))
    }

    /// (assert condition "message") - Assert condition is true
    fn eval_assert(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "assert".to_string(),
                reason: format!(
                    "Expected 2 arguments (condition, message), got {}",
                    args.len()
                ),
            })?;
        }

        // Evaluate condition
        let condition = self.evaluate_expression(&args[0].value)?;
        let is_true = match condition {
            Value::Bool(b) => b,
            _ => {
                return Err(Error::TypeError {
                    expected: "bool".to_string(),
                    got: format!("{:?}", condition),
                })
            }
        };

        if !is_true {
            // Evaluate message
            let message = self.evaluate_expression(&args[1].value)?;
            let message_str = match message {
                Value::String(s) => s,
                _ => format!("{:?}", message),
            };
            return Err(Error::AssertionFailed {
                message: message_str,
            });
        }

        Ok(Value::Null)
    }

    /// (assert-type value predicate) - Assert value matches type predicate
    fn eval_assert_type(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "assert-type".to_string(),
                reason: format!(
                    "Expected 2 arguments (value, predicate), got {}",
                    args.len()
                ),
            })?;
        }

        // Evaluate value
        let value = self.evaluate_expression(&args[0].value)?;

        // Evaluate type predicate (should be a function call like (int? x))
        let predicate_result = self.evaluate_expression(&args[1].value)?;

        let is_valid = match predicate_result {
            Value::Bool(b) => b,
            _ => {
                return Err(Error::TypeError {
                    expected: "bool (type predicate)".to_string(),
                    got: format!("{:?}", predicate_result),
                })
            }
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
                message: format!(
                    "Type assertion failed: expected different type, got {}",
                    type_name
                ),
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
                reason: format!(
                    "Expected 2-3 arguments (body, catch [, finally]), got {}",
                    args.len()
                ),
            })?;
        }

        // Execute try body
        let try_result = self.evaluate_expression(&args[0].value);

        // Parse catch block: accepts both (catch error-var handler-body) ToolCall
        // and Catch expression for compatibility with both try-catch and catch-throw
        let catch_arg = &args[1];
        let (error_var, catch_body) = match &catch_arg.value {
            // Case 1: ToolCall form: (catch e handler) - for try-catch error handling
            Expression::ToolCall {
                name,
                args: arguments,
            } if name == "catch" => {
                if arguments.len() != 2 {
                    return Err(Error::InvalidArguments {
                        tool: "try".to_string(),
                        reason: "catch requires 2 arguments: error-var and handler-body"
                            .to_string(),
                    })?;
                }
                // Extract error variable name
                let error_var = match &arguments[0].value {
                    Expression::Variable(name) => name.clone(),
                    _ => {
                        return Err(Error::InvalidArguments {
                            tool: "try".to_string(),
                            reason: "catch first argument must be a variable name".to_string(),
                        })?
                    }
                };
                (error_var, &arguments[1].value)
            }
            // Case 2: Catch expression form (from special parser)
            // Note: Catch has body as Vec<Expression>, use first expression
            Expression::Catch { tag, body } => {
                // Use the tag as the error variable name
                let error_var = match &**tag {
                    Expression::Variable(name) => name.clone(),
                    _ => "e".to_string(), // Default error var if tag is not a variable
                };
                // Get first body expression or use null
                let catch_expr = body.first().unwrap_or(&Expression::NullLiteral);
                (error_var, catch_expr)
            }
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "try".to_string(),
                    reason: "Second argument must be (catch error-var handler)".to_string(),
                })?
            }
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
                Expression::ToolCall {
                    name,
                    args: arguments,
                } if name == "finally" => {
                    if arguments.len() != 1 {
                        return Err(Error::InvalidArguments {
                            tool: "try".to_string(),
                            reason: "finally requires 1 argument: cleanup-body".to_string(),
                        })?;
                    }
                    // Execute finally block (ignore errors)
                    let _ = self.evaluate_expression(&arguments[0].value);
                }
                _ => {
                    return Err(Error::InvalidArguments {
                        tool: "try".to_string(),
                        reason: "Third argument must be (finally cleanup)".to_string(),
                    })?
                }
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

        Err(Error::AssertionFailed {
            message: message_str,
        })
    }

    /// (split string delimiter) - Split string by delimiter
    fn eval_split(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "split".to_string(),
                reason: format!(
                    "Expected 2 arguments (string, delimiter), got {}",
                    args.len()
                ),
            })?;
        }

        let string = self.evaluate_expression(&args[0].value)?;
        let delimiter = self.evaluate_expression(&args[1].value)?;

        let string_val = match string {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", string),
                })
            }
        };

        let delimiter_val = match delimiter {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", delimiter),
                })
            }
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
                reason: format!(
                    "Expected 2 arguments (array, delimiter), got {}",
                    args.len()
                ),
            })?;
        }

        let array = self.evaluate_expression(&args[0].value)?;
        let delimiter = self.evaluate_expression(&args[1].value)?;

        let array_val = match array {
            Value::Array(ref arr) => arr.clone(),
            _ => {
                return Err(Error::TypeError {
                    expected: "array".to_string(),
                    got: format!("{:?}", array),
                })
            }
        };

        let delimiter_val = match delimiter {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", delimiter),
                })
            }
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
                reason: format!(
                    "Expected 3 arguments (string, old, new), got {}",
                    args.len()
                ),
            })?;
        }

        let string = self.evaluate_expression(&args[0].value)?;
        let old = self.evaluate_expression(&args[1].value)?;
        let new = self.evaluate_expression(&args[2].value)?;

        let string_val = match string {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", string),
                })
            }
        };

        let old_val = match old {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", old),
                })
            }
        };

        let new_val = match new {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", new),
                })
            }
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
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", string),
                })
            }
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
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", string),
                })
            }
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
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: format!("{:?}", string),
                })
            }
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
            _ => {
                return Err(Error::TypeError {
                    expected: "number (int or float)".to_string(),
                    got: format!("{:?}", val),
                })
            }
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
            _ => {
                return Err(Error::TypeError {
                    expected: "number (int or float)".to_string(),
                    got: format!("{:?}", base_val),
                })
            }
        };

        let exponent = match exp_val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number (int or float)".to_string(),
                    got: format!("{:?}", exp_val),
                })
            }
        };

        let result = base.powf(exponent);

        // Check for overflow/invalid results
        if result.is_nan() {
            return Err(Error::InvalidArguments {
                tool: "pow".to_string(),
                reason: format!(
                    "Result is not a number (base={}, exponent={})",
                    base, exponent
                ),
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

    /// (exp x) - Exponential function (e^x)
    fn eval_exp(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "exp".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Float(num.exp()))
    }

    /// (ln x) - Natural logarithm
    fn eval_ln(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "ln".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        if num <= 0.0 {
            return Err(Error::InvalidArguments {
                tool: "ln".to_string(),
                reason: format!("Cannot take natural log of non-positive number: {}", num),
            });
        }

        Ok(Value::Float(num.ln()))
    }

    /// (sin x) - Sine function (radians)
    fn eval_sin(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "sin".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Float(num.sin()))
    }

    /// (cos x) - Cosine function (radians)
    fn eval_cos(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "cos".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Float(num.cos()))
    }

    /// (tan x) - Tangent function (radians)
    fn eval_tan(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "tan".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Float(num.tan()))
    }

    /// (asin x) - Arc sine (inverse sine) in radians
    fn eval_asin(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "asin".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        if !(-1.0..=1.0).contains(&num) {
            return Err(Error::InvalidArguments {
                tool: "asin".to_string(),
                reason: format!("Input must be in range [-1, 1], got {}", num),
            });
        }

        Ok(Value::Float(num.asin()))
    }

    /// (acos x) - Arc cosine (inverse cosine) in radians
    fn eval_acos(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "acos".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        if !(-1.0..=1.0).contains(&num) {
            return Err(Error::InvalidArguments {
                tool: "acos".to_string(),
                reason: format!("Input must be in range [-1, 1], got {}", num),
            });
        }

        Ok(Value::Float(num.acos()))
    }

    /// (atan x) - Arc tangent (inverse tangent) in radians
    fn eval_atan(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "atan".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Float(num.atan()))
    }

    /// (atan2 y x) - Two-argument arc tangent in radians
    fn eval_atan2(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "atan2".to_string(),
                reason: format!("Expected 2 arguments (y, x), got {}", args.len()),
            });
        }

        let y_val = self.evaluate_expression(&args[0].value)?;
        let x_val = self.evaluate_expression(&args[1].value)?;

        let y = match y_val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: y_val.type_name(),
                })
            }
        };

        let x = match x_val {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: x_val.type_name(),
                })
            }
        };

        Ok(Value::Float(y.atan2(x)))
    }

    /// (floor x) - Round down to nearest integer
    fn eval_floor(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "floor".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => return Ok(Value::Int(i)), // Already an integer
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Int(num.floor() as i64))
    }

    /// (ceiling x) - Round up to nearest integer
    fn eval_ceiling(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "ceiling".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => return Ok(Value::Int(i)), // Already an integer
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Int(num.ceil() as i64))
    }

    /// (round x) - Round to nearest integer
    fn eval_round(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "round".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => return Ok(Value::Int(i)), // Already an integer
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Int(num.round() as i64))
    }

    /// (truncate x) - Round towards zero
    fn eval_truncate(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "truncate".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => return Ok(Value::Int(i)), // Already an integer
            Value::Float(f) => f,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Int(num.trunc() as i64))
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
    // COMMON LISP ARITHMETIC SHORTCUTS
    // =========================================================================

    /// (1+ x) - Increment by 1 (Common Lisp)
    fn eval_1_plus(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "1+".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        match val {
            Value::Int(i) => Ok(Value::Int(i + 1)),
            Value::Float(f) => Ok(Value::Float(f + 1.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: val.type_name(),
            }),
        }
    }

    /// (1- x) - Decrement by 1 (Common Lisp)
    fn eval_1_minus(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "1-".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        match val {
            Value::Int(i) => Ok(Value::Int(i - 1)),
            Value::Float(f) => Ok(Value::Float(f - 1.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: val.type_name(),
            }),
        }
    }

    /// (mod x y) - Modulo operation (Common Lisp)
    fn eval_mod(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "mod".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let x = self.evaluate_expression(&args[0].value)?;
        let y = self.evaluate_expression(&args[1].value)?;

        match (&x, &y) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    return Err(Error::InvalidArguments {
                        tool: "mod".to_string(),
                        reason: "Division by zero".to_string(),
                    });
                }
                Ok(Value::Int(a.rem_euclid(*b)))
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.rem_euclid(*b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).rem_euclid(*b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.rem_euclid(*b as f64))),
            _ => Err(Error::TypeError {
                expected: "numbers".to_string(),
                got: format!("{}, {}", x.type_name(), y.type_name()),
            }),
        }
    }

    /// (rem x y) - Remainder operation (Common Lisp)
    fn eval_rem(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "rem".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let x = self.evaluate_expression(&args[0].value)?;
        let y = self.evaluate_expression(&args[1].value)?;

        match (&x, &y) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    return Err(Error::InvalidArguments {
                        tool: "rem".to_string(),
                        reason: "Division by zero".to_string(),
                    });
                }
                Ok(Value::Int(a % b))
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64) % b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a % (*b as f64))),
            _ => Err(Error::TypeError {
                expected: "numbers".to_string(),
                got: format!("{}, {}", x.type_name(), y.type_name()),
            }),
        }
    }

    /// (gcd a b ...) - Greatest common divisor (Common Lisp)
    fn eval_gcd(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }

        let mut result = 0i64;
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            let num = match val {
                Value::Int(i) => i.abs(),
                _ => {
                    return Err(Error::TypeError {
                        expected: "integer".to_string(),
                        got: val.type_name(),
                    })
                }
            };

            result = Self::gcd_impl(result, num);
        }

        Ok(Value::Int(result))
    }

    fn gcd_impl(mut a: i64, mut b: i64) -> i64 {
        while b != 0 {
            let temp = b;
            b = a % b;
            a = temp;
        }
        a.abs()
    }

    /// (lcm a b ...) - Least common multiple (Common Lisp)
    fn eval_lcm(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(1));
        }

        let mut result = 1i64;
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            let num = match val {
                Value::Int(i) => i.abs(),
                _ => {
                    return Err(Error::TypeError {
                        expected: "integer".to_string(),
                        got: val.type_name(),
                    })
                }
            };

            if num == 0 {
                return Ok(Value::Int(0));
            }

            result = Self::lcm_impl(result, num);
        }

        Ok(Value::Int(result))
    }

    fn lcm_impl(a: i64, b: i64) -> i64 {
        if a == 0 || b == 0 {
            return 0;
        }
        (a / Self::gcd_impl(a, b)) * b
    }

    // =========================================================================
    // COMMON LISP LIST PREDICATES
    // =========================================================================

    /// (atom x) - True if x is not a list (Common Lisp)
    fn eval_atom(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "atom".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(!matches!(val, Value::Array(_))))
    }

    /// (consp x) - True if x is a non-empty list (Common Lisp)
    fn eval_consp(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "consp".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        match val {
            Value::Array(ref arr) => Ok(Value::Bool(!arr.is_empty())),
            _ => Ok(Value::Bool(false)),
        }
    }

    /// (listp x) - True if x is a list or null (Common Lisp)
    fn eval_listp(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "listp".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        Ok(Value::Bool(matches!(val, Value::Array(_) | Value::Null)))
    }

    // =========================================================================
    // COMMON LISP BITWISE OPERATIONS
    // =========================================================================

    /// (logand a b ...) - Bitwise AND (Common Lisp)
    fn eval_logand(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(-1)); // Identity for AND
        }

        let mut result = -1i64;
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            let num = match val {
                Value::Int(i) => i,
                _ => {
                    return Err(Error::TypeError {
                        expected: "integer".to_string(),
                        got: val.type_name(),
                    })
                }
            };
            result &= num;
        }

        Ok(Value::Int(result))
    }

    /// (logior a b ...) - Bitwise OR (Common Lisp)
    fn eval_logior(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0)); // Identity for OR
        }

        let mut result = 0i64;
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            let num = match val {
                Value::Int(i) => i,
                _ => {
                    return Err(Error::TypeError {
                        expected: "integer".to_string(),
                        got: val.type_name(),
                    })
                }
            };
            result |= num;
        }

        Ok(Value::Int(result))
    }

    /// (logxor a b ...) - Bitwise XOR (Common Lisp)
    fn eval_logxor(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0)); // Identity for XOR
        }

        let mut result = 0i64;
        for arg in args {
            let val = self.evaluate_expression(&arg.value)?;
            let num = match val {
                Value::Int(i) => i,
                _ => {
                    return Err(Error::TypeError {
                        expected: "integer".to_string(),
                        got: val.type_name(),
                    })
                }
            };
            result ^= num;
        }

        Ok(Value::Int(result))
    }

    /// (lognot x) - Bitwise NOT (Common Lisp)
    fn eval_lognot(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "lognot".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let num = match val {
            Value::Int(i) => i,
            _ => {
                return Err(Error::TypeError {
                    expected: "integer".to_string(),
                    got: val.type_name(),
                })
            }
        };

        Ok(Value::Int(!num))
    }

    /// (ash x count) - Arithmetic shift (Common Lisp)
    fn eval_ash(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "ash".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let count = self.evaluate_expression(&args[1].value)?;

        let num = match val {
            Value::Int(i) => i,
            _ => {
                return Err(Error::TypeError {
                    expected: "integer".to_string(),
                    got: val.type_name(),
                })
            }
        };

        let shift = match count {
            Value::Int(i) => i,
            _ => {
                return Err(Error::TypeError {
                    expected: "integer".to_string(),
                    got: count.type_name(),
                })
            }
        };

        let result = if shift >= 0 {
            num.checked_shl(shift as u32).unwrap_or(0)
        } else {
            num >> (-shift).min(63)
        };

        Ok(Value::Int(result))
    }

    // =========================================================================
    // COMMON LISP LIST OPERATIONS
    // =========================================================================

    /// (member item list) - Find item in list, return tail or null (Common Lisp)
    fn eval_member(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "member".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let item = self.evaluate_expression(&args[0].value)?;
        let list_val = self.evaluate_expression(&args[1].value)?;
        let arr = list_val.as_array()?;

        for (i, elem) in arr.iter().enumerate() {
            if Self::values_are_equal(&item, elem) {
                return Ok(Value::Array(Arc::new(arr[i..].to_vec())));
            }
        }
        Ok(Value::Null)
    }

    fn values_are_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }

    /// (assoc key alist) - Find key in association list (Common Lisp)
    fn eval_assoc(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "assoc".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let key = self.evaluate_expression(&args[0].value)?;
        let alist_val = self.evaluate_expression(&args[1].value)?;
        let arr = alist_val.as_array()?;

        for elem in arr.iter() {
            if let Value::Array(pair) = elem {
                if !pair.is_empty() && Self::values_are_equal(&key, &pair[0]) {
                    return Ok(elem.clone());
                }
            }
        }
        Ok(Value::Null)
    }

    /// (assoc-in object key value) - Set a key in an object with a computed key
    /// Also aliased as set-key
    /// This allows dynamic key names from variables
    fn eval_assoc_in(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "assoc-in".to_string(),
                reason: "Expected 3 arguments: object, key, value".to_string(),
            });
        }

        let obj_val = self.evaluate_expression(&args[0].value)?;
        let key_val = self.evaluate_expression(&args[1].value)?;
        let new_val = self.evaluate_expression(&args[2].value)?;

        // Convert key to string
        let key_str = match key_val {
            Value::String(s) => s,
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            _ => key_val.as_string()?.to_string(),
        };

        // Create new object with the key set
        match obj_val {
            Value::Object(ref map) => {
                let mut new_map = map.as_ref().clone();
                new_map.insert(key_str, new_val);
                Ok(Value::Object(Arc::new(new_map)))
            }
            _ => Err(Error::TypeError {
                expected: "object".to_string(),
                got: obj_val.type_name(),
            }),
        }
    }

    /// (set object key value) - Set object property (like JavaScript/Python)
    /// Alias for assoc-in with same functionality
    /// This is the "everyone else" syntax you wanted
    fn eval_object_set(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Just delegate to assoc-in - it's the same operation
        self.eval_assoc_in(args)
    }

    /// (elt sequence index) - Get element at index (Common Lisp)
    fn eval_elt(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "elt".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let seq = self.evaluate_expression(&args[0].value)?;
        let index_val = self.evaluate_expression(&args[1].value)?;

        let index = match index_val {
            Value::Int(i) if i >= 0 => i as usize,
            Value::Int(i) => {
                return Err(Error::InvalidArguments {
                    tool: "elt".to_string(),
                    reason: format!("Index must be non-negative, got {}", i),
                })
            }
            _ => {
                return Err(Error::TypeError {
                    expected: "integer".to_string(),
                    got: index_val.type_name(),
                })
            }
        };

        match seq {
            Value::Array(arr) => {
                if index >= arr.len() {
                    return Err(Error::InvalidArguments {
                        tool: "elt".to_string(),
                        reason: format!(
                            "Index {} out of bounds for array of length {}",
                            index,
                            arr.len()
                        ),
                    });
                }
                Ok(arr[index].clone())
            }
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                if index >= chars.len() {
                    return Err(Error::InvalidArguments {
                        tool: "elt".to_string(),
                        reason: format!(
                            "Index {} out of bounds for string of length {}",
                            index,
                            chars.len()
                        ),
                    });
                }
                Ok(Value::String(chars[index].to_string()))
            }
            _ => Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: seq.type_name(),
            }),
        }
    }

    /// (subseq sequence start [end]) - Subsequence (Common Lisp)
    fn eval_subseq(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 || args.len() > 3 {
            return Err(Error::InvalidArguments {
                tool: "subseq".to_string(),
                reason: format!("Expected 2 or 3 arguments, got {}", args.len()),
            });
        }

        let seq = self.evaluate_expression(&args[0].value)?;
        let start_val = self.evaluate_expression(&args[1].value)?;

        let start = match start_val {
            Value::Int(i) if i >= 0 => i as usize,
            Value::Int(i) => {
                return Err(Error::InvalidArguments {
                    tool: "subseq".to_string(),
                    reason: format!("Start index must be non-negative, got {}", i),
                })
            }
            _ => {
                return Err(Error::TypeError {
                    expected: "integer".to_string(),
                    got: start_val.type_name(),
                })
            }
        };

        let end = if args.len() == 3 {
            let end_val = self.evaluate_expression(&args[2].value)?;
            match end_val {
                Value::Int(i) if i >= 0 => Some(i as usize),
                Value::Null => None,
                Value::Int(i) => {
                    return Err(Error::InvalidArguments {
                        tool: "subseq".to_string(),
                        reason: format!("End index must be non-negative, got {}", i),
                    })
                }
                _ => {
                    return Err(Error::TypeError {
                        expected: "integer or null".to_string(),
                        got: end_val.type_name(),
                    })
                }
            }
        } else {
            None
        };

        match seq {
            Value::Array(arr) => {
                let end = end.unwrap_or(arr.len());
                if start > arr.len() || end > arr.len() || start > end {
                    return Err(Error::InvalidArguments {
                        tool: "subseq".to_string(),
                        reason: format!(
                            "Invalid range [{}, {}) for array of length {}",
                            start,
                            end,
                            arr.len()
                        ),
                    });
                }
                Ok(Value::Array(Arc::new(arr[start..end].to_vec())))
            }
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                let end = end.unwrap_or(chars.len());
                if start > chars.len() || end > chars.len() || start > end {
                    return Err(Error::InvalidArguments {
                        tool: "subseq".to_string(),
                        reason: format!(
                            "Invalid range [{}, {}) for string of length {}",
                            start,
                            end,
                            chars.len()
                        ),
                    });
                }
                Ok(Value::String(chars[start..end].iter().collect()))
            }
            _ => Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: seq.type_name(),
            }),
        }
    }

    // =========================================================================
    // COMMON LISP STRING COMPARISONS
    // =========================================================================

    /// (string= a b) - String equality (Common Lisp)
    fn eval_string_eq(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "string=".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let a = self.evaluate_expression(&args[0].value)?;
        let b = self.evaluate_expression(&args[1].value)?;

        match (&a, &b) {
            (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1 == s2)),
            _ => Err(Error::TypeError {
                expected: "strings".to_string(),
                got: format!("{}, {}", a.type_name(), b.type_name()),
            }),
        }
    }

    /// (string< a b) - String less than (Common Lisp)
    fn eval_string_lt(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "string<".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let a = self.evaluate_expression(&args[0].value)?;
        let b = self.evaluate_expression(&args[1].value)?;

        match (&a, &b) {
            (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1 < s2)),
            _ => Err(Error::TypeError {
                expected: "strings".to_string(),
                got: format!("{}, {}", a.type_name(), b.type_name()),
            }),
        }
    }

    /// (string> a b) - String greater than (Common Lisp)
    fn eval_string_gt(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "string>".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let a = self.evaluate_expression(&args[0].value)?;
        let b = self.evaluate_expression(&args[1].value)?;

        match (&a, &b) {
            (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1 > s2)),
            _ => Err(Error::TypeError {
                expected: "strings".to_string(),
                got: format!("{}, {}", a.type_name(), b.type_name()),
            }),
        }
    }

    // =========================================================================
    // COMMON LISP MAP VARIANTS
    // =========================================================================

    /// (mapcar function list) - Map and return results (Common Lisp)
    fn eval_mapcar(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "mapcar".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let func = self.evaluate_expression(&args[0].value)?;
        let list_val = self.evaluate_expression(&args[1].value)?;
        let arr = list_val.as_array()?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "mapcar".to_string(),
                        reason: format!(
                            "Lambda must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                let mut results = Vec::with_capacity(arr.len());
                for elem in arr.iter() {
                    self.env.enter_scope();
                    self.env.define(params[0].clone(), elem.clone());
                    let result = self.evaluate_expression(&body)?;
                    self.env.exit_scope();
                    results.push(result);
                }
                Ok(Value::Array(Arc::new(results)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (mapc function list) - Map for side effects, return list (Common Lisp)
    fn eval_mapc(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "mapc".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let func = self.evaluate_expression(&args[0].value)?;
        let list_val = self.evaluate_expression(&args[1].value)?;
        let arr = list_val.as_array()?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "mapc".to_string(),
                        reason: format!(
                            "Lambda must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                for elem in arr.iter() {
                    self.env.enter_scope();
                    self.env.define(params[0].clone(), elem.clone());
                    self.evaluate_expression(&body)?;
                    self.env.exit_scope();
                }
                Ok(list_val) // Return original list
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    // =========================================================================
    // COMMON LISP CONDITIONAL FILTERS
    // =========================================================================

    /// (remove-if predicate list) - Remove matching elements (Common Lisp)
    fn eval_remove_if(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "remove-if".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let pred = self.evaluate_expression(&args[0].value)?;
        let list_val = self.evaluate_expression(&args[1].value)?;
        let arr = list_val.as_array()?;

        match pred {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "remove-if".to_string(),
                        reason: format!(
                            "Lambda must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                let mut results = Vec::new();
                for elem in arr.iter() {
                    self.env.enter_scope();
                    self.env.define(params[0].clone(), elem.clone());
                    let test_result = self.evaluate_expression(&body)?;
                    self.env.exit_scope();

                    if !test_result.is_truthy() {
                        results.push(elem.clone());
                    }
                }
                Ok(Value::Array(Arc::new(results)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: pred.type_name(),
            }),
        }
    }

    /// (remove-if-not predicate list) - Keep matching elements (Common Lisp)
    fn eval_remove_if_not(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "remove-if-not".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let pred = self.evaluate_expression(&args[0].value)?;
        let list_val = self.evaluate_expression(&args[1].value)?;
        let arr = list_val.as_array()?;

        match pred {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "remove-if-not".to_string(),
                        reason: format!(
                            "Lambda must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                let mut results = Vec::new();
                for elem in arr.iter() {
                    self.env.enter_scope();
                    self.env.define(params[0].clone(), elem.clone());
                    let test_result = self.evaluate_expression(&body)?;
                    self.env.exit_scope();

                    if test_result.is_truthy() {
                        results.push(elem.clone());
                    }
                }
                Ok(Value::Array(Arc::new(results)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: pred.type_name(),
            }),
        }
    }

    // =========================================================================
    // COMMON LISP VARIABLE MUTATION
    // =========================================================================

    /// (incf place [delta]) - Increment variable (Common Lisp)
    fn eval_incf(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() || args.len() > 2 {
            return Err(Error::InvalidArguments {
                tool: "incf".to_string(),
                reason: format!("Expected 1 or 2 arguments, got {}", args.len()),
            });
        }

        // Get variable name (must be a symbol/identifier in the arg)
        let var_name = match &args[0].value {
            Expression::Variable(name) => name.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "incf".to_string(),
                    reason: "First argument must be a variable name".to_string(),
                })
            }
        };

        // Get delta (default 1)
        let delta = if args.len() == 2 {
            self.evaluate_expression(&args[1].value)?
        } else {
            Value::Int(1)
        };

        // Get current value
        let current = self.env.get(&var_name)?;

        // Calculate new value
        let new_value = match (&current, &delta) {
            (Value::Int(i), Value::Int(d)) => Value::Int(i + d),
            (Value::Float(f), Value::Float(d)) => Value::Float(f + d),
            (Value::Int(i), Value::Float(d)) => Value::Float(*i as f64 + d),
            (Value::Float(f), Value::Int(d)) => Value::Float(f + (*d as f64)),
            _ => {
                return Err(Error::TypeError {
                    expected: "numbers".to_string(),
                    got: format!("{}, {}", current.type_name(), delta.type_name()),
                })
            }
        };

        // Update variable
        self.env.set(&var_name, new_value.clone())?;
        Ok(new_value)
    }

    /// (decf place [delta]) - Decrement variable (Common Lisp)
    fn eval_decf(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() || args.len() > 2 {
            return Err(Error::InvalidArguments {
                tool: "decf".to_string(),
                reason: format!("Expected 1 or 2 arguments, got {}", args.len()),
            });
        }

        // Get variable name
        let var_name = match &args[0].value {
            Expression::Variable(name) => name.clone(),
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "decf".to_string(),
                    reason: "First argument must be a variable name".to_string(),
                })
            }
        };

        // Get delta (default 1)
        let delta = if args.len() == 2 {
            self.evaluate_expression(&args[1].value)?
        } else {
            Value::Int(1)
        };

        // Get current value
        let current = self.env.get(&var_name)?;

        // Calculate new value
        let new_value = match (&current, &delta) {
            (Value::Int(i), Value::Int(d)) => Value::Int(i - d),
            (Value::Float(f), Value::Float(d)) => Value::Float(f - d),
            (Value::Int(i), Value::Float(d)) => Value::Float(*i as f64 - d),
            (Value::Float(f), Value::Int(d)) => Value::Float(f - (*d as f64)),
            _ => {
                return Err(Error::TypeError {
                    expected: "numbers".to_string(),
                    got: format!("{}, {}", current.type_name(), delta.type_name()),
                })
            }
        };

        // Update variable
        self.env.set(&var_name, new_value.clone())?;
        Ok(new_value)
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
            _ => {
                return Err(Error::TypeError {
                    expected: "array or string".to_string(),
                    got: val.type_name(),
                })
            }
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
            Value::Array(ref arr) => arr.last().cloned().ok_or(Error::IndexOutOfBounds {
                index: 0,
                length: 0,
            }),
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

    /// (nth coll index) - Get element at index
    fn eval_nth(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "nth".to_string(),
                reason: "Expected 2 arguments (collection, index)".to_string(),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let index_val = self.evaluate_expression(&args[1].value)?;

        let index = match index_val {
            Value::Int(i) => i as usize,
            _ => {
                return Err(Error::TypeError {
                    expected: "int".to_string(),
                    got: index_val.type_name(),
                })
            }
        };

        match val {
            Value::Array(ref arr) => arr.get(index).cloned().ok_or(Error::IndexOutOfBounds {
                index,
                length: arr.len(),
            }),
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

    /// (append arr1 arr2) - Concatenate two arrays
    fn eval_append(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "append".to_string(),
                reason: "Expected 2 arguments (array1, array2)".to_string(),
            });
        }

        let arr1_val = self.evaluate_expression(&args[0].value)?;
        let arr2_val = self.evaluate_expression(&args[1].value)?;

        match (arr1_val, arr2_val) {
            (Value::Array(ref arr1), Value::Array(ref arr2)) => {
                let mut new_arr = arr1.to_vec();
                new_arr.extend(arr2.iter().cloned());
                Ok(Value::Array(Arc::new(new_arr)))
            }
            (Value::Array(_), other) => Err(Error::TypeError {
                expected: "array".to_string(),
                got: other.type_name(),
            }),
            (other, _) => Err(Error::TypeError {
                expected: "array".to_string(),
                got: other.type_name(),
            }),
        }
    }

    /// (concatenate args...) - Polymorphic concatenation for strings and arrays
    /// - For strings: concatenates all strings together
    /// - For arrays: concatenates all arrays together
    /// - Variadic: accepts 1+ arguments
    fn eval_concatenate(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "concatenate".to_string(),
                reason: "Expected at least 1 argument".to_string(),
            });
        }

        // Evaluate first arg to determine type
        let first = self.evaluate_expression(&args[0].value)?;

        match first {
            Value::String(ref s) => {
                // String concatenation
                let mut result = s.clone();

                for arg in args.iter().skip(1) {
                    let val = self.evaluate_expression(&arg.value)?;
                    let s = val.as_string()?;
                    result.push_str(s);
                }

                Ok(Value::String(result))
            }
            Value::Array(ref arr) => {
                // Array concatenation
                let mut result = arr.to_vec();

                for arg in args.iter().skip(1) {
                    let val = self.evaluate_expression(&arg.value)?;
                    match val {
                        Value::Array(ref a) => {
                            result.extend(a.iter().cloned());
                        }
                        other => {
                            return Err(Error::TypeError {
                                expected: "array".to_string(),
                                got: other.type_name(),
                            });
                        }
                    }
                }

                Ok(Value::Array(Arc::new(result)))
            }
            other => Err(Error::TypeError {
                expected: "string or array".to_string(),
                got: other.type_name(),
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
            _ => {
                return Err(Error::TypeError {
                    expected: "int".to_string(),
                    got: start_val.type_name(),
                })
            }
        };

        let end = match end_val {
            Value::Int(n) => n,
            _ => {
                return Err(Error::TypeError {
                    expected: "int".to_string(),
                    got: end_val.type_name(),
                })
            }
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
                _ => {
                    return Err(Error::TypeError {
                        expected: "int".to_string(),
                        got: val.type_name(),
                    })
                }
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
                _ => {
                    return Err(Error::TypeError {
                        expected: "int".to_string(),
                        got: val.type_name(),
                    })
                }
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

    /// (sleep milliseconds) - Sleep for specified milliseconds
    fn eval_sleep(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "sleep".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let ms = match val {
            Value::Int(i) => i as u64,
            Value::Float(f) => f as u64,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        std::thread::sleep(std::time::Duration::from_millis(ms));
        Ok(Value::Null)
    }

    /// (base58-encode string) - Encode string to base58
    fn eval_base58_encode(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "base58-encode".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s.as_bytes().to_vec(),
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let encoded = bs58::encode(input).into_string();
        Ok(Value::String(encoded))
    }

    /// (base58-decode base58-string) - Decode base58 to string
    fn eval_base58_decode(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "base58-decode".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let decoded = bs58::decode(input)
            .into_vec()
            .map_err(|e| Error::ParseError(format!("Invalid base58: {}", e)))?;

        let result = String::from_utf8(decoded)
            .map_err(|e| Error::ParseError(format!("Invalid UTF-8 in decoded base58: {}", e)))?;

        Ok(Value::String(result))
    }

    /// (base64-encode string) - Encode string to base64
    fn eval_base64_encode(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "base64-encode".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s.as_bytes().to_vec(),
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let encoded = base64::engine::general_purpose::STANDARD.encode(&input);
        Ok(Value::String(encoded))
    }

    /// (base64-decode base64-string) - Decode base64 to string
    fn eval_base64_decode(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "base64-decode".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let decoded = base64::engine::general_purpose::STANDARD
            .decode(input)
            .map_err(|e| Error::ParseError(format!("Invalid base64: {}", e)))?;

        let result = String::from_utf8(decoded)
            .map_err(|e| Error::ParseError(format!("Invalid UTF-8 in decoded base64: {}", e)))?;

        Ok(Value::String(result))
    }

    /// (base64-decode-raw base64-string) - Decode base64 to hex string (for binary data)
    /// Returns hex representation, avoiding UTF-8 validation issues with binary data
    fn eval_base64_decode_raw(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "base64-decode-raw".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let decoded = base64::engine::general_purpose::STANDARD
            .decode(input)
            .map_err(|e| Error::ParseError(format!("Invalid base64: {}", e)))?;

        // Return as hex string to preserve binary data
        let hex_string = hex::encode(decoded);
        Ok(Value::String(hex_string))
    }

    /// (hex-encode string) - Encode string to hexadecimal
    fn eval_hex_encode(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "hex-encode".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s.as_bytes().to_vec(),
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let encoded = hex::encode(&input);
        Ok(Value::String(encoded))
    }

    /// (hex-decode hex-string) - Decode hexadecimal to string
    fn eval_hex_decode(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "hex-decode".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let decoded =
            hex::decode(input).map_err(|e| Error::ParseError(format!("Invalid hex: {}", e)))?;

        let result = String::from_utf8(decoded)
            .map_err(|e| Error::ParseError(format!("Invalid UTF-8 in decoded hex: {}", e)))?;

        Ok(Value::String(result))
    }

    /// (sha256 string) - Compute SHA-256 hash
    fn eval_sha256(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "sha256".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s.as_bytes().to_vec(),
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let mut hasher = Sha256::new();
        hasher.update(&input);
        let result = hasher.finalize();
        let hash_hex = hex::encode(result);

        Ok(Value::String(hash_hex))
    }

    /// (sha512 string) - Compute SHA-512 hash
    fn eval_sha512(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "sha512".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let input = match val {
            Value::String(s) => s.as_bytes().to_vec(),
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        let mut hasher = Sha512::new();
        hasher.update(&input);
        let result = hasher.finalize();
        let hash_hex = hex::encode(result);

        Ok(Value::String(hash_hex))
    }

    /// (byte-at string index) - Get byte value at index from string (for binary data)
    fn eval_byte_at(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "byte-at".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let string_val = self.evaluate_expression(&args[0].value)?;
        let index_val = self.evaluate_expression(&args[1].value)?;

        let s = match string_val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: string_val.type_name().to_string(),
                })
            }
        };

        let idx = match index_val {
            Value::Int(i) => i as usize,
            Value::Float(f) => f as usize,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: index_val.type_name().to_string(),
                })
            }
        };

        let bytes = s.as_bytes();
        if idx >= bytes.len() {
            return Ok(Value::Null);
        }

        Ok(Value::Int(bytes[idx] as i64))
    }

    /// (parse-u64-le bytes offset) - Parse little-endian u64 from bytes starting at offset
    fn eval_parse_u64_le(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "parse-u64-le".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let bytes_val = self.evaluate_expression(&args[0].value)?;
        let offset_val = self.evaluate_expression(&args[1].value)?;

        let s = match bytes_val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: bytes_val.type_name().to_string(),
                })
            }
        };

        let offset = match offset_val {
            Value::Int(i) => i as usize,
            Value::Float(f) => f as usize,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: offset_val.type_name().to_string(),
                })
            }
        };

        let bytes = s.as_bytes();
        if offset + 8 > bytes.len() {
            return Err(Error::RuntimeError(format!(
                "parse-u64-le: offset {} + 8 exceeds byte length {}",
                offset,
                bytes.len()
            )));
        }

        // Parse little-endian u64
        let mut buf = [0u8; 8];
        buf.copy_from_slice(&bytes[offset..offset + 8]);
        let value = u64::from_le_bytes(buf);

        Ok(Value::Int(value as i64))
    }

    /// (hex-to-u64-le hex-string offset) - Parse little-endian u64 from hex string
    /// offset is in bytes (each byte = 2 hex chars)
    fn eval_hex_to_u64_le(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "hex-to-u64-le".to_string(),
                reason: format!("Expected 2 arguments, got {}", args.len()),
            });
        }

        let hex_val = self.evaluate_expression(&args[0].value)?;
        let offset_val = self.evaluate_expression(&args[1].value)?;

        let hex_str = match hex_val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: hex_val.type_name().to_string(),
                })
            }
        };

        let offset = match offset_val {
            Value::Int(i) => i as usize,
            Value::Float(f) => f as usize,
            _ => {
                return Err(Error::TypeError {
                    expected: "number".to_string(),
                    got: offset_val.type_name().to_string(),
                })
            }
        };

        // Decode hex to bytes
        let bytes = hex::decode(&hex_str)
            .map_err(|e| Error::ParseError(format!("Invalid hex: {}", e)))?;

        // Check bounds (offset + 8 bytes)
        if offset + 8 > bytes.len() {
            return Err(Error::RuntimeError(format!(
                "hex-to-u64-le: offset {} + 8 exceeds decoded byte length {}",
                offset,
                bytes.len()
            )));
        }

        // Parse little-endian u64
        let mut buf = [0u8; 8];
        buf.copy_from_slice(&bytes[offset..offset + 8]);
        let value = u64::from_le_bytes(buf);

        Ok(Value::Int(value as i64))
    }

    /// (bytes-to-hex bytes) - Convert bytes string to hex string
    fn eval_bytes_to_hex(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "bytes-to-hex".to_string(),
                reason: format!("Expected 1 argument, got {}", args.len()),
            });
        }

        let val = self.evaluate_expression(&args[0].value)?;
        let s = match val {
            Value::String(s) => s,
            _ => {
                return Err(Error::TypeError {
                    expected: "string".to_string(),
                    got: val.type_name().to_string(),
                })
            }
        };

        Ok(Value::String(hex::encode(s.as_bytes())))
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

    /// (print value ...) - Print values (Python/JS style)
    fn eval_print(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        let mut output = String::new();
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                output.push(' ');
            }
            let val = self.evaluate_expression(&arg.value)?;
            output.push_str(&val.to_string());
        }
        print!("{}", output);
        std::io::Write::flush(&mut std::io::stdout()).ok();
        Ok(Value::Null)
    }

    /// (println value ...) - Print values with newline (Python/JS style)
    fn eval_println(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        let mut output = String::new();
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                output.push(' ');
            }
            let val = self.evaluate_expression(&arg.value)?;
            output.push_str(&val.to_string());
        }
        println!("{}", output);
        Ok(Value::Null)
    }

    /// (indexOf collection element) - Find index of element in collection
    fn eval_indexof(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "indexOf".to_string(),
                reason: "Expected 2 arguments: collection and element".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let needle = self.evaluate_expression(&args[1].value)?;

        match collection {
            Value::Array(ref arr) => {
                for (i, item) in arr.iter().enumerate() {
                    if item == &needle {
                        return Ok(Value::Int(i as i64));
                    }
                }
                Ok(Value::Int(-1)) // Not found
            }
            Value::String(ref s) => {
                let needle_str = needle.as_string()?;
                match s.find(needle_str) {
                    Some(idx) => Ok(Value::Int(idx as i64)),
                    None => Ok(Value::Int(-1)),
                }
            }
            _ => Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: collection.type_name(),
            }),
        }
    }

    /// (contains collection element) - Check if collection contains element
    fn eval_contains(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "contains".to_string(),
                reason: "Expected 2 arguments: collection and element".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let needle = self.evaluate_expression(&args[1].value)?;

        match collection {
            Value::Array(ref arr) => Ok(Value::Bool(arr.iter().any(|item| item == &needle))),
            Value::String(ref s) => {
                let needle_str = needle.as_string()?;
                Ok(Value::Bool(s.contains(needle_str)))
            }
            _ => Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: collection.type_name(),
            }),
        }
    }

    /// (init array) - All elements except last (Haskell-style)
    fn eval_init(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "init".to_string(),
                reason: "Expected 1 argument: array".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;

        match collection {
            Value::Array(ref arr) => {
                if arr.is_empty() {
                    return Ok(Value::Array(Arc::new(vec![])));
                }
                let init_arr = arr[..arr.len() - 1].to_vec();
                Ok(Value::Array(Arc::new(init_arr)))
            }
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: collection.type_name(),
            }),
        }
    }

    /// (shift array) - Remove and return first element (JS-style)
    fn eval_shift(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "shift".to_string(),
                reason: "Expected 1 argument: array".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;

        match collection {
            Value::Array(ref arr) => {
                if arr.is_empty() {
                    return Ok(Value::Null);
                }
                Ok(arr[0].clone())
            }
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: collection.type_name(),
            }),
        }
    }

    /// (unshift array element) - Add element to front (JS-style)
    fn eval_unshift(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "unshift".to_string(),
                reason: "Expected 2 arguments: array and element".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let element = self.evaluate_expression(&args[1].value)?;

        match collection {
            Value::Array(ref arr) => {
                let mut new_arr = vec![element];
                new_arr.extend_from_slice(arr);
                Ok(Value::Array(Arc::new(new_arr)))
            }
            _ => Err(Error::TypeError {
                expected: "array".to_string(),
                got: collection.type_name(),
            }),
        }
    }

    /// (int value) - Convert to integer (Python/JS style)
    /// Supports: int("42") -> 42, int(3.14) -> 3, int(true) -> 1
    fn eval_to_int(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "int".to_string(),
                reason: "Expected 1 argument: value to convert".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;

        match value {
            Value::Int(n) => Ok(Value::Int(n)),
            Value::Float(f) => Ok(Value::Int(f as i64)),
            Value::String(ref s) => {
                s.trim()
                    .parse::<i64>()
                    .map(Value::Int)
                    .map_err(|_| Error::TypeError {
                        expected: "valid integer string".to_string(),
                        got: format!("'{}'", s),
                    })
            }
            Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
            _ => Err(Error::TypeError {
                expected: "int, float, string, or bool".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (float value) - Convert to float (Python/JS style)
    /// Supports: float("3.14") -> 3.14, float(42) -> 42.0, float(true) -> 1.0
    fn eval_to_float(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "float".to_string(),
                reason: "Expected 1 argument: value to convert".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;

        match value {
            Value::Float(f) => Ok(Value::Float(f)),
            Value::Int(n) => Ok(Value::Float(n as f64)),
            Value::String(ref s) => {
                s.trim()
                    .parse::<f64>()
                    .map(Value::Float)
                    .map_err(|_| Error::TypeError {
                        expected: "valid float string".to_string(),
                        got: format!("'{}'", s),
                    })
            }
            Value::Bool(b) => Ok(Value::Float(if b { 1.0 } else { 0.0 })),
            _ => Err(Error::TypeError {
                expected: "int, float, string, or bool".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (bool value) - Convert to boolean (Python/JS style)
    /// Supports: bool("true") -> true, bool(0) -> false, bool("") -> false
    fn eval_to_bool(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "bool".to_string(),
                reason: "Expected 1 argument: value to convert".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;

        match value {
            Value::Bool(b) => Ok(Value::Bool(b)),
            Value::Int(n) => Ok(Value::Bool(n != 0)),
            Value::Float(f) => Ok(Value::Bool(f != 0.0)),
            Value::String(ref s) => {
                let trimmed = s.trim().to_lowercase();
                match trimmed.as_str() {
                    "true" | "t" | "yes" | "y" | "1" => Ok(Value::Bool(true)),
                    "false" | "f" | "no" | "n" | "0" | "" => Ok(Value::Bool(false)),
                    _ => Err(Error::TypeError {
                        expected: "boolean string (true/false/yes/no/1/0)".to_string(),
                        got: format!("'{}'", s),
                    }),
                }
            }
            Value::Null => Ok(Value::Bool(false)),
            Value::Array(ref arr) => Ok(Value::Bool(!arr.is_empty())),
            Value::Object(ref obj) => Ok(Value::Bool(!obj.is_empty())),
            _ => Ok(Value::Bool(true)), // Functions, ranges, etc. are truthy
        }
    }

    /// (even? n) - Check if number is even (Common LISP: evenp)
    fn eval_even(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "even?".to_string(),
                reason: "Expected 1 argument: number to check".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        match value {
            Value::Int(n) => Ok(Value::Bool(n % 2 == 0)),
            Value::Float(f) => Ok(Value::Bool((f as i64) % 2 == 0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (odd? n) - Check if number is odd (Common LISP: oddp)
    fn eval_odd(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "odd?".to_string(),
                reason: "Expected 1 argument: number to check".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        match value {
            Value::Int(n) => Ok(Value::Bool(n % 2 != 0)),
            Value::Float(f) => Ok(Value::Bool((f as i64) % 2 != 0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (positive? n) - Check if number is positive (Common LISP: plusp/positivep)
    fn eval_positive(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "positive?".to_string(),
                reason: "Expected 1 argument: number to check".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        match value {
            Value::Int(n) => Ok(Value::Bool(n > 0)),
            Value::Float(f) => Ok(Value::Bool(f > 0.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (negative? n) - Check if number is negative (Common LISP: minusp/negativep)
    fn eval_negative(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "negative?".to_string(),
                reason: "Expected 1 argument: number to check".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        match value {
            Value::Int(n) => Ok(Value::Bool(n < 0)),
            Value::Float(f) => Ok(Value::Bool(f < 0.0)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (zero? n) - Check if number is zero (Common LISP: zerop)
    fn eval_zero(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "zero?".to_string(),
                reason: "Expected 1 argument: number to check".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        match value {
            Value::Int(n) => Ok(Value::Bool(n == 0)),
            Value::Float(f) => Ok(Value::Bool(f.abs() < f64::EPSILON)),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: value.type_name(),
            }),
        }
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
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "map".to_string(),
                        reason: format!(
                            "Lambda must take exactly 1 parameter, got {}",
                            params.len()
                        ),
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
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "filter".to_string(),
                        reason: format!(
                            "Lambda must take exactly 1 parameter, got {}",
                            params.len()
                        ),
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
                reason: "Expected 3 arguments: collection, initial value, and reducer lambda"
                    .to_string(),
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
            Value::Function { params, body, .. } => {
                if params.len() != 2 {
                    return Err(Error::InvalidArguments {
                        tool: "reduce".to_string(),
                        reason: format!(
                            "Lambda must take exactly 2 parameters (accumulator, element), got {}",
                            params.len()
                        ),
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
            Value::Function { params, body, .. } => {
                if params.len() != 2 {
                    return Err(Error::InvalidArguments {
                        tool: "sort".to_string(),
                        reason: format!(
                            "Lambda must take exactly 2 parameters, got {}",
                            params.len()
                        ),
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

    /// (format destination control-string &rest args)
    /// Common Lisp-style string formatting
    /// Destination: nil = return string, t = print and return nil
    /// Control directives: ~A (any), ~D (decimal), ~% (newline), ~~ (tilde)
    fn eval_format(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "format".to_string(),
                reason: "Expected at least 2 arguments: destination and control-string".to_string(),
            });
        }

        // Evaluate destination (nil or t)
        let dest = self.evaluate_expression(&args[0].value)?;

        // Get control string
        let control_val = self.evaluate_expression(&args[1].value)?;
        let control_string = control_val.as_string()?;

        // Evaluate remaining arguments for substitution
        let mut format_args = Vec::new();
        for arg in &args[2..] {
            format_args.push(self.evaluate_expression(&arg.value)?);
        }

        // Process control string
        let mut result = String::new();
        let mut chars = control_string.chars().peekable();
        let mut arg_index = 0;

        while let Some(ch) = chars.next() {
            if ch == '~' {
                // Process directive
                if let Some(&next_ch) = chars.peek() {
                    chars.next(); // Consume directive character
                    match next_ch {
                        'A' | 'a' => {
                            // ~A - Aesthetic (any value)
                            if arg_index < format_args.len() {
                                result.push_str(
                                    &self.value_to_format_string(&format_args[arg_index]),
                                );
                                arg_index += 1;
                            }
                        }
                        'D' | 'd' => {
                            // ~D - Decimal integer
                            if arg_index < format_args.len() {
                                if let Value::Int(n) = format_args[arg_index] {
                                    result.push_str(&n.to_string());
                                } else {
                                    result.push_str(
                                        &self.value_to_format_string(&format_args[arg_index]),
                                    );
                                }
                                arg_index += 1;
                            }
                        }
                        '%' => {
                            // ~% - Newline
                            result.push('\n');
                        }
                        '~' => {
                            // ~~ - Literal tilde
                            result.push('~');
                        }
                        _ => {
                            // Unknown directive, just include it
                            result.push('~');
                            result.push(next_ch);
                        }
                    }
                } else {
                    result.push('~');
                }
            } else {
                result.push(ch);
            }
        }

        // Return based on destination
        match dest {
            Value::Null => Ok(Value::String(result)),
            Value::Bool(true) => {
                // Print and return nil
                println!("{}", result);
                Ok(Value::Null)
            }
            _ => Ok(Value::String(result)),
        }
    }

    /// Helper to convert value to string for format
    fn value_to_format_string(&self, val: &Value) -> String {
        match val {
            Value::String(s) => s.clone(),
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Null => "null".to_string(),
            Value::Array(arr) => {
                let items: Vec<String> =
                    arr.iter().map(|v| self.value_to_format_string(v)).collect();
                format!("[{}]", items.join(", "))
            }
            _ => format!("{}", val),
        }
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
                reason: format!(
                    "Invalid slice bounds: start={}, end={}, len={}",
                    start,
                    end,
                    array.len()
                ),
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

        let keys: Vec<Value> = obj.keys().map(|k| Value::String(k.clone())).collect();

        Ok(Value::Array(Arc::new(keys)))
    }

    /// (object-values obj) - Get all values from object (Python: dict.values())
    fn eval_object_values(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "object-values".to_string(),
                reason: "Expected 1 argument: object".to_string(),
            });
        }

        let obj_val = self.evaluate_expression(&args[0].value)?;
        let obj = obj_val.as_object()?;

        let values: Vec<Value> = obj.values().cloned().collect();

        Ok(Value::Array(Arc::new(values)))
    }

    /// (object-entries obj) - Get key-value pairs (Python: dict.items(), JS: Object.entries())
    fn eval_object_entries(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "object-entries".to_string(),
                reason: "Expected 1 argument: object".to_string(),
            });
        }

        let obj_val = self.evaluate_expression(&args[0].value)?;
        let obj = obj_val.as_object()?;

        let entries: Vec<Value> = obj
            .iter()
            .map(|(k, v)| Value::Array(Arc::new(vec![Value::String(k.clone()), v.clone()])))
            .collect();

        Ok(Value::Array(Arc::new(entries)))
    }

    /// merge(obj1, obj2, ...) - Merge objects left-to-right (later values override earlier)
    fn eval_merge(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "merge".to_string(),
                reason: "Expected at least 1 object argument".to_string(),
            });
        }

        // Start with empty map
        let mut result = std::collections::HashMap::new();

        // Merge each object from left to right
        for arg in args {
            let obj_val = self.evaluate_expression(&arg.value)?;
            let obj = obj_val.as_object()?;

            // Insert/override keys from this object
            for (key, value) in obj.iter() {
                result.insert(key.clone(), value.clone());
            }
        }

        Ok(Value::Object(Arc::new(result)))
    }

    /// put(obj, key, value) - Set object property with dynamic key
    /// Returns new object with property set (immutable operation)
    /// Example: (put {:a 1} "b" 2) → {:a 1, :b 2}
    fn eval_put(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "put".to_string(),
                reason: "Expected 3 arguments: object, key, value".to_string(),
            });
        }

        // Get the object
        let obj_val = self.evaluate_expression(&args[0].value)?;
        let obj = obj_val.as_object()?;

        // Get the key (convert to string)
        let key_val = self.evaluate_expression(&args[1].value)?;
        let key = match key_val {
            Value::String(s) => s,
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            _ => return Err(Error::TypeError {
                expected: "string or number for key".to_string(),
                got: key_val.type_name(),
            }),
        };

        // Get the value
        let value = self.evaluate_expression(&args[2].value)?;

        // Create new object with property set
        let mut result = obj.clone();
        result.insert(key, value);

        Ok(Value::Object(Arc::new(result)))
    }

    /// get(collection, key/index) - Safely get from object (by key) or array (by index)
    /// Returns null if not found
    fn eval_get(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "get".to_string(),
                reason: "Expected 2 arguments: collection, key/index".to_string(),
            });
        }

        let collection_val = self.evaluate_expression(&args[0].value)?;
        let accessor_val = self.evaluate_expression(&args[1].value)?;

        // Check if we're accessing an array by numeric index
        match &collection_val {
            Value::Array(arr) => {
                // Array indexing: second argument must be an integer
                let idx = accessor_val.as_int().map_err(|_| Error::InvalidArguments {
                    tool: "get".to_string(),
                    reason: "Array index must be an integer".to_string(),
                })? as usize;

                if idx >= arr.len() {
                    // Return null for out-of-bounds (Ruby-like behavior)
                    return Ok(Value::Null);
                }

                Ok(arr[idx].clone())
            }
            Value::Object(_) => {
                // Object key access: second argument must be a string
                let obj = collection_val.as_object()?;
                let key_str = accessor_val.as_string()?;

                // Strip leading colon from keywords (e.g., ":age" -> "age")
                let key = if key_str.starts_with(':') {
                    &key_str[1..]
                } else {
                    key_str
                };

                // Try direct access first
                if let Some(value) = obj.get(key) {
                    return Ok(value.clone());
                }

                // Get config for lazy field access
                let config = self.lazy_field_config.borrow();
                let strict = config.strict;
                let max_depth = config.max_depth;
                let breadth_first = config.breadth_first;
                drop(config); // Release borrow before recursive search

                // If not found, recursively search nested objects (lazy field access)
                if let Some(value) =
                    self.recursive_field_search_with_config(obj, key, 0, max_depth, breadth_first)
                {
                    return Ok(value);
                }

                // Handle strict mode
                if strict {
                    return Err(Error::InvalidArguments {
                        tool: "get".to_string(),
                        reason: format!("Field '{}' not found in object (strict mode enabled)", key),
                    });
                }

                Ok(Value::Null)
            }
            _ => Err(Error::TypeError {
                expected: "object or array".to_string(),
                got: format!("{:?}", collection_val),
            }),
        }
    }

    /// Recursively search for a field with configuration options
    fn recursive_field_search_with_config(
        &self,
        obj: &std::collections::HashMap<String, Value>,
        key: &str,
        current_depth: usize,
        max_depth: usize,
        breadth_first: bool,
    ) -> Option<Value> {
        // Check depth limit
        if current_depth >= max_depth {
            return None;
        }

        if breadth_first {
            // Breadth-first search
            self.breadth_first_search(obj, key, current_depth, max_depth)
        } else {
            // Depth-first search (original behavior)
            self.depth_first_search(obj, key, current_depth, max_depth)
        }
    }

    /// Depth-first search implementation
    fn depth_first_search(
        &self,
        obj: &std::collections::HashMap<String, Value>,
        key: &str,
        current_depth: usize,
        max_depth: usize,
    ) -> Option<Value> {
        // Depth-first search through nested objects
        for (_field_name, field_value) in obj.iter() {
            match field_value {
                Value::Object(nested_obj) => {
                    // Check if this nested object has the key
                    if let Some(value) = nested_obj.get(key) {
                        return Some(value.clone());
                    }
                    // Recursively search deeper
                    if let Some(value) =
                        self.depth_first_search(nested_obj, key, current_depth + 1, max_depth)
                    {
                        return Some(value);
                    }
                }
                _ => continue,
            }
        }
        None
    }

    /// Breadth-first search implementation
    fn breadth_first_search(
        &self,
        obj: &std::collections::HashMap<String, Value>,
        key: &str,
        current_depth: usize,
        max_depth: usize,
    ) -> Option<Value> {
        use std::collections::VecDeque;

        // Queue of (object, depth) to search
        let mut queue: VecDeque<(&std::collections::HashMap<String, Value>, usize)> =
            VecDeque::new();
        queue.push_back((obj, current_depth));

        while let Some((current_obj, depth)) = queue.pop_front() {
            // Check depth limit
            if depth >= max_depth {
                continue;
            }

            // First, check all direct children for the key
            for (_field_name, field_value) in current_obj.iter() {
                if let Value::Object(nested_obj) = field_value {
                    if let Some(value) = nested_obj.get(key) {
                        return Some(value.clone());
                    }
                }
            }

            // Then, add all nested objects to queue for next level
            for (_field_name, field_value) in current_obj.iter() {
                if let Value::Object(nested_obj) = field_value {
                    queue.push_back((nested_obj.as_ref(), depth + 1));
                }
            }
        }

        None
    }

    /// get-path(object, key) - Get value with path information
    /// Returns {:value <value> :path [<path components>]}
    fn eval_get_path(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "get-path".to_string(),
                reason: "Expected 2 arguments: object, key".to_string(),
            });
        }

        let obj_val = self.evaluate_expression(&args[0].value)?;
        let obj = obj_val.as_object()?;

        let key_val = self.evaluate_expression(&args[1].value)?;
        let key_str = key_val.as_string()?;

        // Strip leading colon from keywords
        let key = if key_str.starts_with(':') {
            &key_str[1..]
        } else {
            key_str
        };

        // Try direct access first
        if let Some(value) = obj.get(key) {
            let mut result = std::collections::HashMap::new();
            result.insert("value".to_string(), value.clone());
            result.insert("path".to_string(), Value::Array(Arc::new(vec![])));
            return Ok(Value::Object(Arc::new(result)));
        }

        // If not found, search with path tracking
        if let Some((value, path)) = self.recursive_field_search_with_path(obj, key, &[]) {
            let mut result = std::collections::HashMap::new();
            result.insert("value".to_string(), value);
            result.insert(
                "path".to_string(),
                Value::Array(Arc::new(
                    path.iter().map(|s| Value::String(s.to_string())).collect(),
                )),
            );
            return Ok(Value::Object(Arc::new(result)));
        }

        // Return null value with empty path
        let mut result = std::collections::HashMap::new();
        result.insert("value".to_string(), Value::Null);
        result.insert("path".to_string(), Value::Array(Arc::new(vec![])));
        Ok(Value::Object(Arc::new(result)))
    }

    /// Helper for get-path: recursive search that tracks the path
    fn recursive_field_search_with_path(
        &self,
        obj: &std::collections::HashMap<String, Value>,
        key: &str,
        current_path: &[String],
    ) -> Option<(Value, Vec<String>)> {
        for (field_name, field_value) in obj.iter() {
            match field_value {
                Value::Object(nested_obj) => {
                    // Check if this nested object has the key
                    if let Some(value) = nested_obj.get(key) {
                        let mut path = current_path.to_vec();
                        path.push(field_name.clone());
                        return Some((value.clone(), path));
                    }
                    // Recursively search deeper
                    let mut new_path = current_path.to_vec();
                    new_path.push(field_name.clone());
                    if let Some(result) =
                        self.recursive_field_search_with_path(nested_obj, key, &new_path)
                    {
                        return Some(result);
                    }
                }
                _ => continue,
            }
        }
        None
    }

    /// discover(object) - List all available fields in object and nested objects
    /// Returns array of field names or array of {:field <name> :path [<path>]} if :with-paths true
    fn eval_discover(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "discover".to_string(),
                reason: "Expected at least 1 argument: object".to_string(),
            });
        }

        let obj_val = self.evaluate_expression(&args[0].value)?;
        let obj = obj_val.as_object()?;

        // Check for :with-paths option
        let with_paths = args.len() > 1 && {
            if let Ok(opt_val) = self.evaluate_expression(&args[1].value) {
                if let Ok(opt_str) = opt_val.as_string() {
                    opt_str == ":with-paths" || opt_str == "with-paths"
                } else {
                    false
                }
            } else {
                false
            }
        };

        let mut fields = Vec::new();
        self.discover_fields(obj, &[], &mut fields, with_paths);

        if with_paths {
            // Return array of {:field "name" :path ["a", "b"]}
            let result: Vec<Value> = fields
                .into_iter()
                .map(|(field, path)| {
                    let mut obj = std::collections::HashMap::new();
                    obj.insert("field".to_string(), Value::String(field));
                    obj.insert(
                        "path".to_string(),
                        Value::Array(Arc::new(
                            path.iter().map(|s| Value::String(s.to_string())).collect(),
                        )),
                    );
                    Value::Object(Arc::new(obj))
                })
                .collect();
            Ok(Value::Array(Arc::new(result)))
        } else {
            // Return simple array of field names
            let result: Vec<Value> = fields
                .into_iter()
                .map(|(field, _)| Value::String(field))
                .collect();
            Ok(Value::Array(Arc::new(result)))
        }
    }

    /// Helper for discover: recursively collect all field names
    fn discover_fields(
        &self,
        obj: &std::collections::HashMap<String, Value>,
        current_path: &[String],
        fields: &mut Vec<(String, Vec<String>)>,
        _with_paths: bool,
    ) {
        for (field_name, field_value) in obj.iter() {
            // Add this field
            fields.push((field_name.clone(), current_path.to_vec()));

            // Recursively discover nested fields
            if let Value::Object(nested_obj) = field_value {
                let mut new_path = current_path.to_vec();
                new_path.push(field_name.clone());
                self.discover_fields(nested_obj, &new_path, fields, _with_paths);
            }
        }
    }

    /// lazy-config(option, value) - Configure lazy field access behavior
    /// Options: :strict (bool), :breadth-first (bool), :max-depth (number)
    fn eval_lazy_config(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "lazy-config".to_string(),
                reason: "Expected 2 arguments: option, value".to_string(),
            });
        }

        let option_val = self.evaluate_expression(&args[0].value)?;
        let option_str = option_val.as_string()?;
        let option = if option_str.starts_with(':') {
            &option_str[1..]
        } else {
            option_str
        };

        let value_val = self.evaluate_expression(&args[1].value)?;

        let mut config = self.lazy_field_config.borrow_mut();

        match option {
            "strict" => {
                let strict = value_val.as_bool()?;
                config.strict = strict;
                Ok(Value::Bool(strict))
            }
            "breadth-first" => {
                let breadth_first = value_val.as_bool()?;
                config.breadth_first = breadth_first;
                Ok(Value::Bool(breadth_first))
            }
            "max-depth" => {
                let max_depth = value_val.as_int()? as usize;
                config.max_depth = max_depth;
                Ok(Value::Int(max_depth as i64))
            }
            _ => Err(Error::InvalidArguments {
                tool: "lazy-config".to_string(),
                reason: format!(
                    "Unknown option: {}. Valid options: :strict, :breadth-first, :max-depth",
                    option
                ),
            }),
        }
    }

    // ========================================
    // JSON Operations (Built-in Functions)
    // ========================================

    /// parse-json - Parse a JSON string into OVSM values
    /// Usage: (parse-json {:json "{"a": 1, "b": [2,3]}"})
    fn eval_parse_json(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "parse-json".to_string(),
                reason: "Expected 1 argument: {:json string}".to_string(),
            });
        }

        // Support both object form {:json "..."} and direct string
        let json_str = match self.evaluate_expression(&args[0].value)? {
            Value::Object(obj) => {
                // Object form: (parse-json {:json "..."})
                obj.get("json")
                    .ok_or_else(|| Error::InvalidArguments {
                        tool: "parse-json".to_string(),
                        reason: "Object must have 'json' field".to_string(),
                    })?
                    .as_string()?
                    .to_string()
            }
            Value::String(s) => {
                // Direct string form: (parse-json "...")
                s.to_string()
            }
            _ => {
                return Err(Error::InvalidArguments {
                    tool: "parse-json".to_string(),
                    reason: "Expected object with json field or string".to_string(),
                })
            }
        };

        // Parse JSON string into serde_json::Value
        let json_value: serde_json::Value =
            serde_json::from_str(&json_str).map_err(|e| Error::ToolExecutionError {
                tool: "json-parse".to_string(),
                reason: format!("Failed to parse JSON: {}", e),
            })?;

        // Convert serde_json::Value to OVSM Value
        Ok(self.json_to_value(json_value))
    }

    /// json-stringify - Convert OVSM value to JSON string
    /// Usage: (json-stringify {:value data :pretty true})
    fn eval_json_stringify(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "json-stringify".to_string(),
                reason: "Expected 1 argument: {:value data} or direct value".to_string(),
            });
        }

        let (value, pretty) = match self.evaluate_expression(&args[0].value)? {
            Value::Object(obj) => {
                // Object form: (json-stringify {:value ... :pretty true})
                let val = obj
                    .get("value")
                    .ok_or_else(|| Error::InvalidArguments {
                        tool: "json-stringify".to_string(),
                        reason: "Object must have 'value' field".to_string(),
                    })?
                    .clone();
                let pretty = obj
                    .get("pretty")
                    .and_then(|v| v.as_bool().ok())
                    .unwrap_or(false);
                (val, pretty)
            }
            v => {
                // Direct form: (json-stringify data)
                (v, false)
            }
        };

        // Convert OVSM Value to serde_json::Value
        let json_value = self.value_to_json(value)?;

        // Stringify with optional pretty printing
        let json_str = if pretty {
            serde_json::to_string_pretty(&json_value)
        } else {
            serde_json::to_string(&json_value)
        }
        .map_err(|e| Error::ToolExecutionError {
            tool: "json-stringify".to_string(),
            reason: format!("Failed to stringify JSON: {}", e),
        })?;

        Ok(Value::String(json_str))
    }

    /// Helper: Convert serde_json::Value to OVSM Value
    fn json_to_value(&self, json: serde_json::Value) -> Value {
        use serde_json::Value as JV;
        match json {
            JV::Null => Value::Null,
            JV::Bool(b) => Value::Bool(b),
            JV::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Int(i)
                } else if let Some(f) = n.as_f64() {
                    Value::Float(f)
                } else {
                    Value::Float(n.as_f64().unwrap_or(0.0))
                }
            }
            JV::String(s) => Value::String(s),
            JV::Array(arr) => Value::Array(Arc::new(
                arr.into_iter().map(|v| self.json_to_value(v)).collect(),
            )),
            JV::Object(map) => {
                let mut obj = HashMap::new();
                for (k, v) in map {
                    obj.insert(k, self.json_to_value(v));
                }
                Value::Object(Arc::new(obj))
            }
        }
    }

    /// Helper: Convert OVSM Value to serde_json::Value
    fn value_to_json(&self, value: Value) -> Result<serde_json::Value> {
        use serde_json::Value as JV;
        Ok(match value {
            Value::Null => JV::Null,
            Value::Bool(b) => JV::Bool(b),
            Value::Int(i) => JV::Number(serde_json::Number::from(i)),
            Value::Float(f) => serde_json::Number::from_f64(f)
                .map(JV::Number)
                .unwrap_or(JV::Null),
            Value::String(s) => JV::String(s.to_string()),
            Value::Array(arr) => {
                let mut json_arr = Vec::new();
                for item in arr.iter() {
                    json_arr.push(self.value_to_json(item.clone())?);
                }
                JV::Array(json_arr)
            }
            Value::Object(obj) => {
                let mut json_obj = serde_json::Map::new();
                for (k, v) in obj.iter() {
                    json_obj.insert(k.clone(), self.value_to_json(v.clone())?);
                }
                JV::Object(json_obj)
            }
            Value::Function { .. } => {
                return Err(Error::InvalidOperation {
                    op: "json-conversion".to_string(),
                    left_type: "function".to_string(),
                    right_type: "json".to_string(),
                })
            }
            Value::Range { .. } => {
                return Err(Error::InvalidOperation {
                    op: "json-conversion".to_string(),
                    left_type: "range".to_string(),
                    right_type: "json".to_string(),
                })
            }
            Value::Multiple(_) => {
                return Err(Error::InvalidOperation {
                    op: "json-conversion".to_string(),
                    left_type: "multiple-values".to_string(),
                    right_type: "json".to_string(),
                })
            }
            Value::Macro { .. } => {
                return Err(Error::InvalidOperation {
                    op: "json-conversion".to_string(),
                    left_type: "macro".to_string(),
                    right_type: "json".to_string(),
                })
            }
        })
    }

    // ========================================
    // Network Operations
    // ========================================

    /// (http-get url [headers]) - Make HTTP GET request
    fn eval_http_get(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        use crate::tools::stdlib::network;

        // Evaluate arguments
        let mut eval_args = Vec::new();
        for arg in args {
            eval_args.push(self.evaluate_expression(&arg.value)?);
        }

        // Call async function using block_in_place to avoid nested runtime error
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(network::http_get(&eval_args))
        })
    }

    /// (http-post url body [headers]) - Make HTTP POST request
    fn eval_http_post(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        use crate::tools::stdlib::network;

        let mut eval_args = Vec::new();
        for arg in args {
            eval_args.push(self.evaluate_expression(&arg.value)?);
        }

        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(network::http_post(&eval_args))
        })
    }

    /// (json-rpc url method [params]) - Make JSON-RPC call
    fn eval_json_rpc(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        use crate::tools::stdlib::network;

        let mut eval_args = Vec::new();
        for arg in args {
            eval_args.push(self.evaluate_expression(&arg.value)?);
        }

        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(network::json_rpc(&eval_args))
        })
    }

    // ========================================
    // LINQ-Style Functional Operations
    // ========================================

    /// (find collection predicate) - Find first element matching predicate
    fn eval_find(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "find".to_string(),
                reason: "Expected 2 arguments: collection and predicate".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get predicate function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "find".to_string(),
                        reason: format!(
                            "Predicate must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

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

                    // Return first matching element
                    if val.is_truthy() {
                        return Ok(elem.clone());
                    }
                }

                // No match found
                Ok(Value::Null)
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (distinct collection) - Remove duplicate elements
    fn eval_distinct(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "distinct".to_string(),
                reason: "Expected 1 argument: collection".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        let mut seen = std::collections::HashSet::new();
        let mut result = Vec::new();

        for elem in array.iter() {
            // Create a string representation for hashing
            let key = format!("{:?}", elem);
            if seen.insert(key) {
                result.push(elem.clone());
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }

    /// (flatten nested-array) - Flatten nested arrays one level
    fn eval_flatten(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "flatten".to_string(),
                reason: "Expected 1 argument: nested array".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        let mut result = Vec::new();

        for elem in array.iter() {
            match elem {
                Value::Array(inner) => {
                    // Flatten one level
                    for inner_elem in inner.iter() {
                        result.push(inner_elem.clone());
                    }
                }
                _ => {
                    // Non-array elements are kept as-is
                    result.push(elem.clone());
                }
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }

    /// (reverse collection) - Reverse array order
    fn eval_reverse(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "reverse".to_string(),
                reason: "Expected 1 argument: collection or string".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;

        // Handle both arrays and strings
        match collection {
            Value::Array(ref arr) => {
                let mut result = arr.to_vec();
                result.reverse();
                Ok(Value::Array(Arc::new(result)))
            }
            Value::String(ref s) => {
                let reversed: String = s.chars().rev().collect();
                Ok(Value::String(reversed))
            }
            _ => Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: collection.type_name(),
            }),
        }
    }

    /// (repeat value n) - Repeat string or array N times (Python: "x"*3, JS: "x".repeat(3))
    fn eval_repeat(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "repeat".to_string(),
                reason: "Expected 2 arguments: value and count".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let count_val = self.evaluate_expression(&args[1].value)?;
        let count = count_val.as_int()? as usize;

        match value {
            Value::String(ref s) => {
                let repeated = s.repeat(count);
                Ok(Value::String(repeated))
            }
            Value::Array(ref arr) => {
                let mut result = Vec::with_capacity(arr.len() * count);
                for _ in 0..count {
                    result.extend_from_slice(arr);
                }
                Ok(Value::Array(Arc::new(result)))
            }
            _ => Err(Error::TypeError {
                expected: "string or array".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (some collection predicate) - Check if any element matches
    fn eval_some(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "some".to_string(),
                reason: "Expected 2 arguments: collection and predicate".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get predicate function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "some".to_string(),
                        reason: format!(
                            "Predicate must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

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

                    // Return true if any match
                    if val.is_truthy() {
                        return Ok(Value::Bool(true));
                    }
                }

                // No match found
                Ok(Value::Bool(false))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (every collection predicate) - Check if all elements match
    fn eval_every(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "every".to_string(),
                reason: "Expected 2 arguments: collection and predicate".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get predicate function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "every".to_string(),
                        reason: format!(
                            "Predicate must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

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

                    // Return false if any don't match
                    if !val.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                }

                // All matched
                Ok(Value::Bool(true))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (partition collection predicate) - Split into matching and not-matching
    fn eval_partition(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "partition".to_string(),
                reason: "Expected 2 arguments: collection and predicate".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get predicate function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "partition".to_string(),
                        reason: format!(
                            "Predicate must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                let mut matching = Vec::new();
                let mut not_matching = Vec::new();

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

                    // Partition based on predicate result
                    if val.is_truthy() {
                        matching.push(elem.clone());
                    } else {
                        not_matching.push(elem.clone());
                    }
                }

                // Return [matching-array, not-matching-array]
                Ok(Value::Array(Arc::new(vec![
                    Value::Array(Arc::new(matching)),
                    Value::Array(Arc::new(not_matching)),
                ])))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (take collection n) - Take first N elements
    fn eval_take(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "take".to_string(),
                reason: "Expected 2 arguments: n and collection".to_string(),
            });
        }

        // FIXED: Swap argument order to match standard LISP convention: (take n collection)
        let n_val = self.evaluate_expression(&args[0].value)?;
        let n = match n_val {
            Value::Int(i) => {
                if i < 0 {
                    return Err(Error::InvalidArguments {
                        tool: "take".to_string(),
                        reason: "n must be non-negative".to_string(),
                    });
                }
                i as usize
            }
            _ => {
                return Err(Error::TypeError {
                    expected: "int".to_string(),
                    got: n_val.type_name(),
                });
            }
        };

        let collection = self.evaluate_expression(&args[1].value)?;
        let array = collection.as_array()?;

        let result: Vec<Value> = array.iter().take(n).cloned().collect();

        Ok(Value::Array(Arc::new(result)))
    }

    /// (drop collection n) - Skip first N elements
    fn eval_drop(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "drop".to_string(),
                reason: "Expected 2 arguments: collection and n".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        let n_val = self.evaluate_expression(&args[1].value)?;
        let n = match n_val {
            Value::Int(i) => {
                if i < 0 {
                    return Err(Error::InvalidArguments {
                        tool: "drop".to_string(),
                        reason: "n must be non-negative".to_string(),
                    });
                }
                i as usize
            }
            _ => {
                return Err(Error::TypeError {
                    expected: "int".to_string(),
                    got: n_val.type_name(),
                });
            }
        };

        let result: Vec<Value> = array.iter().skip(n).cloned().collect();

        Ok(Value::Array(Arc::new(result)))
    }

    /// (zip array1 array2) - Combine two arrays element-wise
    fn eval_zip(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "zip".to_string(),
                reason: "Expected 2 arguments: array1 and array2".to_string(),
            });
        }

        let array1_val = self.evaluate_expression(&args[0].value)?;
        let array1 = array1_val.as_array()?;

        let array2_val = self.evaluate_expression(&args[1].value)?;
        let array2 = array2_val.as_array()?;

        let mut result = Vec::new();
        let min_len = std::cmp::min(array1.len(), array2.len());

        for i in 0..min_len {
            let pair = vec![array1[i].clone(), array2[i].clone()];
            result.push(Value::Array(Arc::new(pair)));
        }

        Ok(Value::Array(Arc::new(result)))
    }

    /// (compact collection) - Remove null values
    fn eval_compact(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "compact".to_string(),
                reason: "Expected 1 argument: collection".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        let result: Vec<Value> = array
            .iter()
            .filter(|elem| !matches!(elem, Value::Null))
            .cloned()
            .collect();

        Ok(Value::Array(Arc::new(result)))
    }

    /// (pluck collection property-name) - Extract property from array of objects
    fn eval_pluck(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "pluck".to_string(),
                reason: "Expected 2 arguments: collection and property-name".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        let prop_val = self.evaluate_expression(&args[1].value)?;
        let prop_name = prop_val.as_string()?;

        // Strip leading colon from keywords
        let prop = if prop_name.starts_with(':') {
            &prop_name[1..]
        } else {
            prop_name
        };

        let mut result = Vec::new();

        for elem in array.iter() {
            match elem {
                Value::Object(obj) => {
                    let val = obj.get(prop).cloned().unwrap_or(Value::Null);
                    result.push(val);
                }
                _ => {
                    // Non-object elements yield null
                    result.push(Value::Null);
                }
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }

    /// (group-by collection key-fn) - Group elements by key function
    fn eval_group_by(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "group-by".to_string(),
                reason: "Expected 2 arguments: collection and key-fn".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get key function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "group-by".to_string(),
                        reason: format!(
                            "Key function must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                let mut groups: std::collections::HashMap<String, Vec<Value>> =
                    std::collections::HashMap::new();

                // Apply key function to each element
                for elem in array.iter() {
                    // Create new scope for lambda execution
                    self.env.enter_scope();

                    // Bind parameter
                    self.env.define(params[0].clone(), elem.clone());

                    // Evaluate key function
                    let key_val = self.evaluate_expression(&body)?;

                    // Exit scope
                    self.env.exit_scope();

                    // Convert key to string
                    let key = match key_val {
                        Value::String(s) => s,
                        Value::Int(i) => i.to_string(),
                        Value::Float(f) => f.to_string(),
                        Value::Bool(b) => b.to_string(),
                        _ => format!("{:?}", key_val),
                    };

                    groups.entry(key).or_default().push(elem.clone());
                }

                // Convert groups to object with arrays
                let mut result_map = std::collections::HashMap::new();
                for (key, values) in groups {
                    result_map.insert(key, Value::Array(Arc::new(values)));
                }

                Ok(Value::Object(Arc::new(result_map)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (aggregate groups agg-fn) - Aggregate grouped data with aggregation function
    fn eval_aggregate(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "aggregate".to_string(),
                reason: "Expected 2 arguments: groups and aggregation-fn".to_string(),
            });
        }

        // Evaluate groups (should be object from group-by)
        let groups = self.evaluate_expression(&args[0].value)?;
        let groups_obj = groups.as_object()?;

        // Get aggregation function
        let agg_fn = self.evaluate_expression(&args[1].value)?;

        match agg_fn {
            Value::Function { params, body, .. } => {
                if params.len() != 2 {
                    return Err(Error::InvalidArguments {
                        tool: "aggregate".to_string(),
                        reason: format!("Aggregation function must take exactly 2 parameters (key, values), got {}", params.len()),
                    });
                }

                // Aggregate each group
                let mut result = Vec::new();

                for (key, values) in groups_obj.iter() {
                    // Create scope for aggregation function
                    self.env.enter_scope();
                    self.env
                        .define(params[0].clone(), Value::String(key.clone()));
                    self.env.define(params[1].clone(), values.clone());

                    // Evaluate aggregation function
                    let aggregated = self.evaluate_expression(&body)?;

                    self.env.exit_scope();

                    result.push(aggregated);
                }

                Ok(Value::Array(Arc::new(result)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: agg_fn.type_name(),
            }),
        }
    }

    /// (sort-by collection key-fn) - Sort collection by key function result
    fn eval_sort_by(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 || args.len() > 3 {
            return Err(Error::InvalidArguments {
                tool: "sort-by".to_string(),
                reason: "Expected 2-3 arguments: collection, key-fn, and optional :desc flag"
                    .to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get key function
        let key_fn = self.evaluate_expression(&args[1].value)?;

        // Check for :desc flag
        let descending = if args.len() == 3 {
            let flag = self.evaluate_expression(&args[2].value)?;
            match flag {
                Value::String(s) if s == ":desc" => true,
                Value::Bool(b) => b,
                _ => false,
            }
        } else {
            false
        };

        match key_fn {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "sort-by".to_string(),
                        reason: format!(
                            "Key function must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                // Create vector of (element, key) pairs
                let mut pairs = Vec::new();

                for elem in array.iter() {
                    // Create scope for key function
                    self.env.enter_scope();
                    self.env.define(params[0].clone(), elem.clone());

                    // Evaluate key function to get sort key
                    let key = self.evaluate_expression(&body)?;

                    self.env.exit_scope();

                    pairs.push((elem.clone(), key));
                }

                // Sort by keys
                pairs.sort_by(|a, b| {
                    let cmp = match (&a.1, &b.1) {
                        (Value::Int(x), Value::Int(y)) => x.cmp(y),
                        (Value::Float(x), Value::Float(y)) => {
                            x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                        }
                        (Value::String(x), Value::String(y)) => x.cmp(y),
                        _ => std::cmp::Ordering::Equal,
                    };

                    if descending {
                        cmp.reverse()
                    } else {
                        cmp
                    }
                });

                // Extract sorted elements
                let sorted: Vec<Value> = pairs.into_iter().map(|(elem, _)| elem).collect();

                Ok(Value::Array(Arc::new(sorted)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: key_fn.type_name(),
            }),
        }
    }

    /// (count-by collection key-fn) - Count occurrences by key function
    fn eval_count_by(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "count-by".to_string(),
                reason: "Expected 2 arguments: collection and key-fn".to_string(),
            });
        }

        // Evaluate collection
        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        // Get key function
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, .. } => {
                if params.len() != 1 {
                    return Err(Error::InvalidArguments {
                        tool: "count-by".to_string(),
                        reason: format!(
                            "Key function must take exactly 1 parameter, got {}",
                            params.len()
                        ),
                    });
                }

                let mut counts: std::collections::HashMap<String, i64> =
                    std::collections::HashMap::new();

                // Apply key function to each element
                for elem in array.iter() {
                    // Create new scope for lambda execution
                    self.env.enter_scope();

                    // Bind parameter
                    self.env.define(params[0].clone(), elem.clone());

                    // Evaluate key function
                    let key_val = self.evaluate_expression(&body)?;

                    // Exit scope
                    self.env.exit_scope();

                    // Convert key to string
                    let key = match key_val {
                        Value::String(s) => s,
                        Value::Int(i) => i.to_string(),
                        Value::Float(f) => f.to_string(),
                        Value::Bool(b) => b.to_string(),
                        _ => format!("{:?}", key_val),
                    };

                    *counts.entry(key).or_insert(0) += 1;
                }

                // Convert counts to object with int values
                let mut result_map = std::collections::HashMap::new();
                for (key, count) in counts {
                    result_map.insert(key, Value::Int(count));
                }

                Ok(Value::Object(Arc::new(result_map)))
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// Evaluate a regular tool call
    fn eval_tool_call(&mut self, name: &str, args: &[crate::parser::Argument]) -> Result<Value> {
        // Check if this is a user-defined function first
        if let Ok(func_val) = self.env.get(name) {
            if let Value::Function {
                params,
                body,
                closure,
                is_flet,
            } = func_val
            {
                // This is a function call!

                // Evaluate arguments - handle both positional and keyword arguments
                let mut evaluated_args = Vec::new();
                for arg in args {
                    // If this is a keyword argument, include the keyword name with colon prefix
                    if let Some(ref keyword_name) = arg.name {
                        // Ensure keyword has colon prefix
                        let kw = if keyword_name.starts_with(':') {
                            keyword_name.clone()
                        } else {
                            format!(":{}", keyword_name)
                        };
                        evaluated_args.push(Value::String(kw));
                    }
                    // Add the argument value
                    let val = self.evaluate_expression(&arg.value)?;
                    evaluated_args.push(val);
                }

                // For flet functions, use isolated execution
                // This prevents recursion by isolating from parent scopes
                if is_flet {
                    // Save current environment
                    let saved_env = self.env.clone();

                    // Create new isolated environment with only closure variables
                    self.env = Environment::new();
                    for (var_name, var_value) in closure.iter() {
                        self.env.define(var_name.clone(), var_value.clone());
                    }

                    // Bind parameters
                    self.bind_function_parameters(&params, &evaluated_args, name)?;

                    // Evaluate function body
                    let result = self.evaluate_expression(&body); // Explicit deref

                    // Restore original environment
                    self.env = saved_env;

                    return result;
                } else {
                    // For regular defun functions (empty closure), use normal scope chain
                    self.env.enter_scope();

                    // Bind parameters
                    self.bind_function_parameters(&params, &evaluated_args, name)?;

                    // Evaluate function body
                    let result = self.evaluate_expression(&body); // Explicit deref

                    // Exit function scope
                    self.env.exit_scope();

                    return result;
                }
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
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) < r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l < (r as f64))),
                (l, r) => Err(Error::InvalidOperation {
                    op: "less than".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::Gt => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l > r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) > r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l > (r as f64))),
                (l, r) => Err(Error::InvalidOperation {
                    op: "greater than".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::LtEq => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l <= r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) <= r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l <= (r as f64))),
                (l, r) => Err(Error::InvalidOperation {
                    op: "less than or equal".to_string(),
                    left_type: l.type_name(),
                    right_type: r.type_name(),
                }),
            },

            BinaryOp::GtEq => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l >= r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) >= r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l >= (r as f64))),
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

    /// (eval expr) - Evaluate an expression at runtime
    /// Evaluates the result of evaluating the argument
    fn eval_eval(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::InvalidArguments {
                tool: "eval".to_string(),
                reason: "Expected 1 argument: expression to evaluate".to_string(),
            });
        }

        // First, evaluate the argument to get an expression
        let value = self.evaluate_expression(&args[0].value)?;

        // Convert the value back to an expression and evaluate it
        // For now, we'll use a simple approach: parse strings
        match value {
            Value::String(s) => {
                // Try to parse and evaluate the string as OVSM code
                use crate::lexer::SExprScanner;
                use crate::parser::SExprParser;
                let mut scanner = SExprScanner::new(&s);
                let tokens = scanner.scan_tokens()?;
                let mut parser = SExprParser::new(tokens);
                let program = parser.parse()?;

                // Execute the parsed program
                let mut result = Value::Null;
                for stmt in &program.statements {
                    if let crate::parser::Statement::Expression(expr) = stmt {
                        result = self.evaluate_expression(expr)?;
                    }
                }
                Ok(result)
            }
            // For other types, just return them as-is (already evaluated)
            other => Ok(other),
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
                let vals: Result<Vec<_>> =
                    exprs.iter().map(|e| self.expression_to_value(e)).collect();
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
                if s.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
                {
                    Ok(Expression::Variable(s.clone()))
                } else {
                    Ok(Expression::StringLiteral(s.clone()))
                }
            }
            Value::Bool(b) => Ok(Expression::BoolLiteral(*b)),
            Value::Null => Ok(Expression::NullLiteral),
            Value::Array(arr) => {
                let exprs: Result<Vec<_>> =
                    arr.iter().map(|v| self.value_to_expression(v)).collect();
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
            _ => Err(Error::ParseError(
                "Expected quasiquote expression".to_string(),
            )),
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
    fn parse_function_parameters(
        &self,
        params_expr: &Expression,
        context: &str,
    ) -> Result<Vec<String>> {
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
            _ => {
                return Err(Error::ParseError(format!(
                    "{}: requires parameter list",
                    context
                )))
            }
        };

        self.parse_params_from_list(param_exprs, context)
    }

    /// Helper to parse parameter list from expression vector
    /// Supports: required, &optional, &rest, &key parameters
    /// Format: ["req1", "req2", "&optional", "opt1", "default1", "&rest", "args", "&key", "key1", "default1"]
    fn parse_params_from_list(
        &self,
        param_exprs: &[Expression],
        context: &str,
    ) -> Result<Vec<String>> {
        let mut param_names = Vec::new();
        let mut section = "required"; // required, optional, rest, key
        let mut i = 0;

        while i < param_exprs.len() {
            let param_expr = &param_exprs[i];

            // Check for section markers
            if let Expression::Variable(name) = param_expr {
                match name.as_str() {
                    "&optional" => {
                        if section != "required" {
                            return Err(Error::ParseError(format!(
                                "{}: &optional must come before &rest and &key",
                                context
                            )));
                        }
                        section = "optional";
                        param_names.push(name.clone());
                        i += 1;
                        continue;
                    }
                    "&rest" => {
                        if section == "key" {
                            return Err(Error::ParseError(format!(
                                "{}: &rest must come before &key",
                                context
                            )));
                        }
                        if i == param_exprs.len() - 1 {
                            return Err(Error::ParseError(format!(
                                "{}: &rest must be followed by parameter name",
                                context
                            )));
                        }
                        section = "rest";
                        param_names.push(name.clone());
                        i += 1;
                        // Next item must be the rest parameter name
                        if let Expression::Variable(rest_name) = &param_exprs[i] {
                            param_names.push(rest_name.clone());
                            i += 1;
                            continue;
                        } else {
                            return Err(Error::ParseError(format!(
                                "{}: &rest must be followed by parameter name",
                                context
                            )));
                        }
                    }
                    "&key" => {
                        section = "key";
                        param_names.push(name.clone());
                        i += 1;
                        continue;
                    }
                    _ => {}
                }
            }

            // Handle parameters based on current section
            match section {
                "required" => {
                    if let Expression::Variable(name) = param_expr {
                        param_names.push(name.clone());
                    } else {
                        return Err(Error::ParseError(format!(
                            "{}: required parameters must be identifiers",
                            context
                        )));
                    }
                }
                "optional" | "key" => {
                    // Can be either: variable (with null default) or (variable default-expr)
                    match param_expr {
                        Expression::Variable(name) => {
                            // Parameter without explicit default
                            param_names.push(name.clone());
                            param_names.push("null".to_string()); // Default to null
                        }
                        Expression::ArrayLiteral(list) => {
                            // (param-name default-value)
                            if list.len() != 2 {
                                return Err(Error::ParseError(format!(
                                    "{}: {} parameter default must be (name default)",
                                    context, section
                                )));
                            }
                            if let Expression::Variable(name) = &list[0] {
                                param_names.push(name.clone());
                                // Serialize default expression
                                let default_val = self.expression_to_value(&list[1])?;
                                param_names.push(self.serialize_default_value(&default_val)?);
                            } else {
                                return Err(Error::ParseError(format!(
                                    "{}: {} parameter name must be identifier",
                                    context, section
                                )));
                            }
                        }
                        Expression::ToolCall { name, args } => {
                            // Handle (param-name default-value) as ToolCall
                            if args.len() != 1 {
                                return Err(Error::ParseError(format!(
                                    "{}: {} parameter default must be (name default)",
                                    context, section
                                )));
                            }
                            param_names.push(name.clone());
                            // Serialize default expression
                            let default_val = self.expression_to_value(&args[0].value)?;
                            param_names.push(self.serialize_default_value(&default_val)?);
                        }
                        _ => {
                            return Err(Error::ParseError(format!(
                                "{}: {} parameters must be identifiers or (name default)",
                                context, section
                            )));
                        }
                    }
                }
                "rest" => {
                    // Already handled in &rest case above
                    return Err(Error::ParseError(format!(
                        "{}: unexpected parameter after &rest",
                        context
                    )));
                }
                _ => unreachable!(),
            }

            i += 1;
        }

        Ok(param_names)
    }

    /// Serialize a default value for storage in parameter list
    fn serialize_default_value(&self, value: &Value) -> Result<String> {
        match value {
            Value::Int(n) => Ok(n.to_string()),
            Value::Float(f) => Ok(f.to_string()),
            Value::String(s) => Ok(format!(
                "\"{}\"",
                s.replace('\\', "\\\\").replace('"', "\\\"")
            )),
            Value::Bool(b) => Ok(b.to_string()),
            Value::Null => Ok("null".to_string()),
            Value::Array(arr) => {
                let items: Result<Vec<_>> = arr
                    .iter()
                    .map(|v| self.serialize_default_value(v))
                    .collect();
                Ok(format!("[{}]", items?.join(" ")))
            }
            Value::Object(obj) => {
                let mut pairs = Vec::new();
                for (k, v) in obj.iter() {
                    pairs.push(format!(":{}  {}", k, self.serialize_default_value(v)?));
                }
                Ok(format!("{{{}}}", pairs.join(" ")))
            }
            _ => Err(Error::ParseError(format!(
                "Cannot use {} as default parameter value",
                value.type_name()
            ))),
        }
    }

    /// Bind function/macro parameters to arguments
    /// Supports: required, &optional, &rest, &key parameters
    fn bind_function_parameters(
        &mut self,
        params: &[String],
        args: &[Value],
        context: &str,
    ) -> Result<()> {
        // Find section boundaries
        let optional_pos = params.iter().position(|p| p == "&optional");
        let rest_pos = params.iter().position(|p| p == "&rest");
        let key_pos = params.iter().position(|p| p == "&key");

        // Calculate section ranges
        let required_end = optional_pos
            .or(rest_pos)
            .or(key_pos)
            .unwrap_or(params.len());
        let optional_start = optional_pos.map(|p| p + 1);
        let optional_end = optional_pos.and_then(|_op| rest_pos.or(key_pos).or(Some(params.len())));
        let rest_idx = rest_pos;
        let key_start = key_pos.map(|p| p + 1);

        // Required parameters
        let required_params: Vec<&String> = params[..required_end].iter().collect();
        let required_count = required_params.len();

        // Check minimum arguments (required params must be provided)
        if args.len() < required_count {
            return Err(Error::InvalidArguments {
                tool: context.to_string(),
                reason: format!(
                    "Expected at least {} arguments, got {}",
                    required_count,
                    args.len()
                ),
            });
        }

        // Bind required parameters
        for i in 0..required_count {
            self.env.define(required_params[i].clone(), args[i].clone());
        }

        let mut arg_idx = required_count;

        // Bind optional parameters
        if let (Some(opt_start), Some(opt_end)) = (optional_start, optional_end) {
            let mut i = opt_start;
            while i < opt_end {
                let param_name = &params[i];
                let default_str = &params[i + 1];

                if arg_idx < args.len() {
                    // Check if this arg is a keyword (starts with :)
                    let is_keyword =
                        matches!(&args[arg_idx], Value::String(s) if s.starts_with(':'));

                    if !is_keyword {
                        // Use provided argument
                        self.env.define(param_name.clone(), args[arg_idx].clone());
                        arg_idx += 1;
                    } else {
                        // Keyword argument - use default for optional param
                        let default_val = self.parse_default_value(default_str)?;
                        self.env.define(param_name.clone(), default_val);
                    }
                } else {
                    // Use default value
                    let default_val = self.parse_default_value(default_str)?;
                    self.env.define(param_name.clone(), default_val);
                }

                i += 2; // Skip param name and default
            }
        }

        // Handle &rest parameter
        let rest_param_name = if let Some(rest_idx) = rest_idx {
            if rest_idx + 1 < params.len() {
                Some(params[rest_idx + 1].clone())
            } else {
                return Err(Error::ParseError(format!(
                    "{}: &rest must be followed by parameter name",
                    context
                )));
            }
        } else {
            None
        };

        // Calculate how many args go into &rest (before keyword args start)
        let (rest_args, keyword_start_idx) = if rest_param_name.is_some() {
            let mut rest_end = arg_idx;
            // Find where keyword args start
            while rest_end < args.len() {
                if let Value::String(s) = &args[rest_end] {
                    if s.starts_with(':') {
                        break;
                    }
                }
                rest_end += 1;
            }
            (args[arg_idx..rest_end].to_vec(), rest_end)
        } else {
            (Vec::new(), arg_idx)
        };

        // Parse keyword arguments (if &key present) - start after rest args
        let keyword_args = if key_pos.is_some() {
            self.parse_keyword_args(args, keyword_start_idx)?
        } else {
            std::collections::HashMap::new()
        };

        // Bind &rest parameter if present
        if let Some(rest_name) = rest_param_name {
            self.env.define(rest_name, Value::array(rest_args.clone()));
        }

        // Bind keyword parameters
        if let Some(key_start_idx) = key_start {
            let mut i = key_start_idx;
            while i < params.len() {
                let param_name = &params[i];
                let default_str = &params[i + 1];

                // Check if keyword was provided in args
                let key_name = format!(":{}", param_name);
                if let Some(val) = keyword_args.get(&key_name) {
                    self.env.define(param_name.clone(), val.clone());
                } else {
                    // Use default value
                    let default_val = self.parse_default_value(default_str)?;
                    self.env.define(param_name.clone(), default_val);
                }

                i += 2; // Skip param name and default
            }
        }

        // If we don't have &rest or &key, check for exact arg count
        if rest_pos.is_none()
            && key_pos.is_none()
            && optional_pos.is_none()
            && args.len() != required_count
        {
            return Err(Error::InvalidArguments {
                tool: context.to_string(),
                reason: format!("Expected {} arguments, got {}", required_count, args.len()),
            });
        }

        Ok(())
    }

    /// Parse default value from serialized string
    fn parse_default_value(&mut self, default_str: &str) -> Result<Value> {
        // Handle simple literals
        if default_str == "null" {
            return Ok(Value::Null);
        }
        if default_str == "true" {
            return Ok(Value::Bool(true));
        }
        if default_str == "false" {
            return Ok(Value::Bool(false));
        }
        if let Ok(n) = default_str.parse::<i64>() {
            return Ok(Value::Int(n));
        }
        if let Ok(f) = default_str.parse::<f64>() {
            return Ok(Value::Float(f));
        }
        if default_str.starts_with('"') && default_str.ends_with('"') {
            // String literal
            let s = &default_str[1..default_str.len() - 1];
            let unescaped = s.replace("\\\"", "\"").replace("\\\\", "\\");
            return Ok(Value::String(unescaped));
        }
        if default_str.starts_with('[') && default_str.ends_with(']') {
            // Array literal - simplified parsing (TODO: full parser support)
            // For now, return empty array as placeholder
            return Ok(Value::array(Vec::new()));
        }
        if default_str.starts_with('{') && default_str.ends_with('}') {
            // Object literal - simplified parsing (TODO: full parser support)
            // For now, return empty object as placeholder
            use std::collections::HashMap;
            return Ok(Value::object(HashMap::new()));
        }

        // If nothing matched, default to null
        Ok(Value::Null)
    }

    /// Parse keyword arguments from args slice starting at start_idx
    /// Returns map of keyword names (with :) to their values
    fn parse_keyword_args(
        &self,
        args: &[Value],
        start_idx: usize,
    ) -> Result<std::collections::HashMap<String, Value>> {
        use std::collections::HashMap;
        let mut keyword_args = HashMap::new();
        let mut i = start_idx;

        while i < args.len() {
            // Check for keyword
            if let Value::String(key) = &args[i] {
                if key.starts_with(':') {
                    // Next value should be the argument
                    if i + 1 >= args.len() {
                        return Err(Error::InvalidArguments {
                            tool: "keyword arguments".to_string(),
                            reason: format!("Keyword {} missing value", key),
                        });
                    }
                    keyword_args.insert(key.clone(), args[i + 1].clone());
                    i += 2;
                } else {
                    // Not a keyword - stop parsing
                    break;
                }
            } else {
                // Not a string - stop parsing
                break;
            }
        }

        Ok(keyword_args)
    }

    // ========================================================================
    // Catch/Throw - Non-Local Exits (Common Lisp)
    // ========================================================================

    /// Evaluate (catch tag body...) expression
    /// Establishes an exit point for throw
    fn eval_catch(&mut self, tag_expr: &Expression, body: &[Expression]) -> Result<Value> {
        // Evaluate the tag (usually a quoted symbol)
        let tag_value = self.evaluate_expression(tag_expr)?;
        let tag_string = tag_value.to_string();

        // Execute body expressions
        let mut result = Value::Null;
        for expr in body {
            match self.evaluate_expression(expr) {
                Ok(val) => result = val,
                Err(Error::ThrowValue { tag, value }) => {
                    // Check if this throw is for us
                    if tag == tag_string {
                        // Caught! Return the thrown value
                        return Ok(*value);
                    } else {
                        // Not our tag, re-throw it
                        return Err(Error::ThrowValue { tag, value });
                    }
                }
                Err(e) => return Err(e), // Other errors propagate normally
            }
        }

        Ok(result)
    }

    /// Evaluate (throw tag value) expression
    /// Performs non-local exit to matching catch
    fn eval_throw(&mut self, tag_expr: &Expression, value_expr: &Expression) -> Result<Value> {
        // Evaluate tag and value
        let tag_value = self.evaluate_expression(tag_expr)?;
        let value = self.evaluate_expression(value_expr)?;

        // Create throw error to unwind stack
        Err(Error::ThrowValue {
            tag: tag_value.to_string(),
            value: Box::new(value),
        })
    }

    /// Evaluate (destructuring-bind pattern value body...) expression
    /// Pattern matching for variable binding
    fn eval_destructuring_bind(
        &mut self,
        pattern: &Expression,
        value_expr: &Expression,
        body: &[Expression],
    ) -> Result<Value> {
        // Evaluate the value expression
        let value = self.evaluate_expression(value_expr)?;

        // Push new scope for bindings
        self.env.enter_scope();

        // Perform pattern matching and binding
        self.destructure_pattern(pattern, &value)?;

        // Evaluate body expressions
        let mut result = Value::Null;
        for expr in body {
            result = self.evaluate_expression(expr)?;
        }

        // Pop scope
        self.env.exit_scope();

        Ok(result)
    }

    /// Recursively match pattern against value and bind variables
    fn destructure_pattern(&mut self, pattern: &Expression, value: &Value) -> Result<()> {
        match pattern {
            // Simple variable binding
            Expression::Variable(name) => {
                // Special handling for &rest marker
                if name.starts_with('&') {
                    return Err(Error::ParseError(format!(
                        "Unexpected lambda list keyword in pattern: {}",
                        name
                    )));
                }
                self.env.define(name.clone(), value.clone());
                Ok(())
            }

            // Parenthesized list pattern (a b c) or function call pattern
            Expression::ToolCall { name: _, args } => self.destructure_list_pattern(args, value),

            // Array literal pattern [a b c] (treated like list)
            Expression::ArrayLiteral(pattern_elements) => {
                if let Value::Array(arr) = value {
                    // Check for &rest
                    let mut rest_idx = None;
                    for (i, elem) in pattern_elements.iter().enumerate() {
                        if let Expression::Variable(name) = elem {
                            if name == "&rest" {
                                rest_idx = Some(i);
                                break;
                            }
                        }
                    }

                    if let Some(rest_pos) = rest_idx {
                        // With &rest: bind required elements, then rest
                        if arr.len() < rest_pos {
                            return Err(Error::ParseError(format!(
                                "Not enough elements: expected at least {}, got {}",
                                rest_pos,
                                arr.len()
                            )));
                        }

                        // Bind required elements
                        for (pattern_elem, val) in
                            pattern_elements.iter().take(rest_pos).zip(arr.iter())
                        {
                            self.destructure_pattern(pattern_elem, val)?;
                        }

                        // Bind &rest variable
                        if rest_pos + 1 < pattern_elements.len() {
                            if let Expression::Variable(rest_var) = &pattern_elements[rest_pos + 1]
                            {
                                let rest_values = arr[rest_pos..].to_vec();
                                self.env
                                    .define(rest_var.clone(), Value::Array(Arc::new(rest_values)));
                            }
                        }
                    } else {
                        // Without &rest: exact length match
                        if pattern_elements.len() != arr.len() {
                            return Err(Error::ParseError(format!(
                                "Pattern length mismatch: expected {}, got {}",
                                pattern_elements.len(),
                                arr.len()
                            )));
                        }

                        for (pattern_elem, val) in pattern_elements.iter().zip(arr.iter()) {
                            self.destructure_pattern(pattern_elem, val)?;
                        }
                    }
                    Ok(())
                } else {
                    Err(Error::TypeError {
                        expected: "Array".to_string(),
                        got: format!("{:?}", value),
                    })
                }
            }

            _ => Err(Error::ParseError(format!(
                "Invalid pattern in destructuring-bind: {:?}",
                pattern
            ))),
        }
    }

    /// Destructure list pattern with support for &rest
    fn destructure_list_pattern(
        &mut self,
        pattern_args: &[crate::parser::Argument],
        value: &Value,
    ) -> Result<()> {
        // Extract pattern variable names
        let mut pattern_vars = Vec::new();
        let mut rest_idx = None;

        for (i, arg) in pattern_args.iter().enumerate() {
            if let Expression::Variable(name) = &arg.value {
                if name == "&rest" {
                    rest_idx = Some(i);
                    break;
                }
                pattern_vars.push(name.clone());
            } else {
                // Nested pattern
                pattern_vars.push(String::new()); // placeholder
            }
        }

        // Get array values
        let arr = if let Value::Array(arr) = value {
            arr.clone()
        } else {
            return Err(Error::TypeError {
                expected: "Array".to_string(),
                got: format!("{:?}", value),
            });
        };

        // Check length constraints
        if let Some(rest_pos) = rest_idx {
            // With &rest: need at least (rest_pos) elements
            if arr.len() < rest_pos {
                return Err(Error::ParseError(format!(
                    "Not enough elements to destructure: expected at least {}, got {}",
                    rest_pos,
                    arr.len()
                )));
            }

            // Bind required elements
            for (i, arg) in pattern_args.iter().enumerate().take(rest_pos) {
                self.destructure_pattern(&arg.value, &arr[i])?;
            }

            // Bind &rest variable (next after &rest keyword)
            if rest_pos + 1 < pattern_args.len() {
                if let Expression::Variable(rest_var) = &pattern_args[rest_pos + 1].value {
                    let rest_values = arr[rest_pos..].to_vec();
                    self.env
                        .define(rest_var.clone(), Value::Array(Arc::new(rest_values)));
                }
            }
        } else {
            // Without &rest: exact length match
            if pattern_vars.len() != arr.len() {
                return Err(Error::ParseError(format!(
                    "Pattern length mismatch: expected {}, got {}",
                    pattern_vars.len(),
                    arr.len()
                )));
            }

            // Bind each element
            for (i, arg) in pattern_args.iter().enumerate() {
                self.destructure_pattern(&arg.value, &arr[i])?;
            }
        }

        Ok(())
    }

    // ========================================================================
    // Loop Macro Evaluator (Common Lisp)
    // ========================================================================

    /// Evaluate loop expression
    fn eval_loop(&mut self, loop_data: &LoopData) -> Result<Value> {
        // 1. Create new scope for loop
        self.env.enter_scope();

        // 2. Initialize accumulator based on accumulation type
        let mut accumulator = match &loop_data.accumulation {
            Some(AccumulationClause::Sum(_)) => Value::Int(0),
            Some(AccumulationClause::Collect(_)) => Value::Array(Arc::new(Vec::new())),
            Some(AccumulationClause::Count(_)) => Value::Int(0),
            None => Value::Null,
        };

        // 3. Generate iteration values
        let iteration_values = self.generate_iteration_values(&loop_data.iteration)?;
        let var_name = self.get_iteration_var_name(&loop_data.iteration);

        // 4. Execute loop
        for value in iteration_values {
            // Bind iteration variable
            self.env.define(var_name.clone(), value.clone());

            // Check early exit conditions
            if let Some(early_exit) = &loop_data.early_exit {
                if self.should_exit_loop(early_exit)? {
                    break;
                }
            }

            // Check conditional execution
            if !self.check_loop_condition(&loop_data.condition)? {
                continue;
            }

            // Execute accumulation or body
            if let Some(accum) = &loop_data.accumulation {
                accumulator = self.perform_accumulation(accum, &var_name, accumulator)?;
            } else {
                // Execute body expressions
                for expr in &loop_data.body {
                    self.evaluate_expression(expr)?;
                }
            }
        }

        // 5. Exit scope and return accumulator
        self.env.exit_scope();
        Ok(accumulator)
    }

    /// Generate iteration values from iteration clause
    fn generate_iteration_values(&mut self, iteration: &IterationClause) -> Result<Vec<Value>> {
        match iteration {
            IterationClause::Numeric {
                var: _,
                from,
                to,
                by,
                downfrom,
                below,
            } => {
                let from_val = self.evaluate_expression(from)?;
                let to_val = self.evaluate_expression(to)?;
                let by_val = if let Some(by_expr) = by {
                    self.evaluate_expression(by_expr)?
                } else {
                    Value::Int(1)
                };

                let start = match from_val {
                    Value::Int(n) => n,
                    Value::Float(f) => f as i64,
                    _ => {
                        return Err(Error::TypeError {
                            expected: "number".to_string(),
                            got: format!("{:?}", from_val),
                        })
                    }
                };

                let end = match to_val {
                    Value::Int(n) => n,
                    Value::Float(f) => f as i64,
                    _ => {
                        return Err(Error::TypeError {
                            expected: "number".to_string(),
                            got: format!("{:?}", to_val),
                        })
                    }
                };

                let step = match by_val {
                    Value::Int(n) => n,
                    Value::Float(f) => f as i64,
                    _ => {
                        return Err(Error::TypeError {
                            expected: "number".to_string(),
                            got: format!("{:?}", by_val),
                        })
                    }
                };

                if step == 0 {
                    return Err(Error::InvalidArguments {
                        tool: "loop".to_string(),
                        reason: "Loop 'by' step cannot be zero".to_string(),
                    });
                }

                let mut values = Vec::new();

                if *downfrom {
                    // Counting down
                    let mut i = start;
                    while if *below { i > end } else { i >= end } {
                        values.push(Value::Int(i));
                        i -= step;
                    }
                } else {
                    // Counting up
                    let mut i = start;
                    while if *below { i < end } else { i <= end } {
                        values.push(Value::Int(i));
                        i += step;
                    }
                }

                Ok(values)
            }
            IterationClause::Collection { collection, .. } => {
                let coll = self.evaluate_expression(collection)?;
                match coll {
                    Value::Array(arr) => {
                        Ok(Arc::try_unwrap(arr).unwrap_or_else(|arc| (*arc).clone()))
                    }
                    Value::String(s) => {
                        // Iterate over characters
                        Ok(s.chars().map(|c| Value::String(c.to_string())).collect())
                    }
                    _ => Err(Error::TypeError {
                        expected: "array or string".to_string(),
                        got: format!("{:?}", coll),
                    }),
                }
            }
        }
    }

    /// Get iteration variable name from iteration clause
    fn get_iteration_var_name(&self, iteration: &IterationClause) -> String {
        match iteration {
            IterationClause::Numeric { var, .. } => var.clone(),
            IterationClause::Collection { var, .. } => var.clone(),
        }
    }

    /// Check if loop should exit early
    fn should_exit_loop(&mut self, exit: &ExitClause) -> Result<bool> {
        match exit {
            ExitClause::While(test) => {
                let val = self.evaluate_expression(test)?;
                Ok(!val.is_truthy())
            }
            ExitClause::Until(test) => {
                let val = self.evaluate_expression(test)?;
                Ok(val.is_truthy())
            }
        }
    }

    /// Check loop condition (when/unless)
    fn check_loop_condition(&mut self, condition: &Option<ConditionClause>) -> Result<bool> {
        match condition {
            Some(ConditionClause::When(test)) => {
                let val = self.evaluate_expression(test)?;
                Ok(val.is_truthy())
            }
            Some(ConditionClause::Unless(test)) => {
                let val = self.evaluate_expression(test)?;
                Ok(!val.is_truthy())
            }
            None => Ok(true),
        }
    }

    /// Perform accumulation (sum/collect/count)
    fn perform_accumulation(
        &mut self,
        accum: &AccumulationClause,
        var_name: &str,
        current: Value,
    ) -> Result<Value> {
        match accum {
            AccumulationClause::Sum(expr) => {
                let val = if let Some(e) = expr {
                    self.evaluate_expression(e)?
                } else {
                    self.env.get(var_name)?
                };

                match (current, val) {
                    (Value::Int(sum), Value::Int(n)) => Ok(Value::Int(sum + n)),
                    (Value::Float(sum), Value::Float(n)) => Ok(Value::Float(sum + n)),
                    (Value::Int(sum), Value::Float(n)) => Ok(Value::Float(sum as f64 + n)),
                    (Value::Float(sum), Value::Int(n)) => Ok(Value::Float(sum + n as f64)),
                    (curr, val) => Err(Error::TypeError {
                        expected: "number".to_string(),
                        got: format!("sum operands: {:?} and {:?}", curr, val),
                    }),
                }
            }
            AccumulationClause::Collect(expr) => {
                let val = if let Some(e) = expr {
                    self.evaluate_expression(e)?
                } else {
                    self.env.get(var_name)?
                };

                if let Value::Array(arr) = current {
                    let mut vec = Arc::try_unwrap(arr).unwrap_or_else(|arc| (*arc).clone());
                    vec.push(val);
                    Ok(Value::Array(Arc::new(vec)))
                } else {
                    Err(Error::ParseError(
                        "Internal error: collect accumulator should be array".to_string(),
                    ))
                }
            }
            AccumulationClause::Count(expr) => {
                let val = if let Some(e) = expr {
                    self.evaluate_expression(e)?
                } else {
                    Value::Bool(true)
                };

                if val.is_truthy() {
                    if let Value::Int(count) = current {
                        Ok(Value::Int(count + 1))
                    } else {
                        Err(Error::ParseError(
                            "Internal error: count accumulator should be int".to_string(),
                        ))
                    }
                } else {
                    Ok(current)
                }
            }
        }
    }
    // ============================================================================
    // STATISTICAL FUNCTIONS (NumPy/Pandas style)
    // ============================================================================

    /// (mean collection) - Calculate mean/average
    fn eval_mean(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "mean".to_string(),
                reason: "Expected 1 argument: collection of numbers".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        if array.is_empty() {
            return Ok(Value::Float(0.0));
        }

        let mut sum = 0.0;
        for val in array.iter() {
            sum += val.as_float()?;
        }

        Ok(Value::Float(sum / array.len() as f64))
    }

    /// (median collection) - Calculate median value
    fn eval_median(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "median".to_string(),
                reason: "Expected 1 argument: collection of numbers".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        if array.is_empty() {
            return Ok(Value::Float(0.0));
        }

        let mut numbers: Vec<f64> = array
            .iter()
            .map(|v| v.as_float())
            .collect::<Result<Vec<_>>>()?;

        numbers.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let mid = numbers.len() / 2;
        if numbers.len().is_multiple_of(2) {
            Ok(Value::Float((numbers[mid - 1] + numbers[mid]) / 2.0))
        } else {
            Ok(Value::Float(numbers[mid]))
        }
    }

    /// (mode collection) - Find most common value
    fn eval_mode(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "mode".to_string(),
                reason: "Expected 1 argument: collection".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        if array.is_empty() {
            return Ok(Value::Null);
        }

        let mut counts = std::collections::HashMap::new();
        for val in array.iter() {
            *counts.entry(format!("{:?}", val)).or_insert(0) += 1;
        }

        let (_, max_count) = counts
            .iter()
            .max_by_key(|(_, &count)| count)
            .ok_or_else(|| Error::TypeError {
                expected: "non-empty collection".to_string(),
                got: "empty".to_string(),
            })?;

        // Return first value with max count
        for val in array.iter() {
            if counts.get(&format!("{:?}", val)) == Some(max_count) {
                return Ok(val.clone());
            }
        }

        Ok(Value::Null)
    }

    /// (product collection) - Calculate product of numbers
    fn eval_product(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "product".to_string(),
                reason: "Expected 1 argument: collection of numbers".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        let mut product = 1.0;
        let mut is_int = true;

        for val in array.iter() {
            match val {
                Value::Int(n) => product *= *n as f64,
                Value::Float(f) => {
                    product *= f;
                    is_int = false;
                }
                _ => {
                    return Err(Error::TypeError {
                        expected: "number".to_string(),
                        got: val.type_name(),
                    })
                }
            }
        }

        if is_int && product.fract() == 0.0 {
            Ok(Value::Int(product as i64))
        } else {
            Ok(Value::Float(product))
        }
    }

    /// (variance collection) - Calculate variance
    fn eval_variance(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "variance".to_string(),
                reason: "Expected 1 argument: collection of numbers".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;

        if array.len() < 2 {
            return Ok(Value::Float(0.0));
        }

        // Calculate mean
        let mut sum = 0.0;
        for val in array.iter() {
            sum += val.as_float()?;
        }
        let mean = sum / array.len() as f64;

        // Calculate variance
        let mut variance = 0.0;
        for val in array.iter() {
            let diff = val.as_float()? - mean;
            variance += diff * diff;
        }
        variance /= array.len() as f64;

        Ok(Value::Float(variance))
    }

    /// (stddev collection) - Calculate standard deviation
    fn eval_stddev(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        let variance = self.eval_variance(args)?;
        let var_val = variance.as_float()?;
        Ok(Value::Float(var_val.sqrt()))
    }

    // ============================================================================
    // MATH UTILITIES
    // ============================================================================

    /// (sign n) - Return sign of number (-1, 0, 1)
    fn eval_sign(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "sign".to_string(),
                reason: "Expected 1 argument: number".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        match value {
            Value::Int(n) => Ok(Value::Int(if n > 0 {
                1
            } else if n < 0 {
                -1
            } else {
                0
            })),
            Value::Float(f) => Ok(Value::Int(if f > 0.0 {
                1
            } else if f < 0.0 {
                -1
            } else {
                0
            })),
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: value.type_name(),
            }),
        }
    }

    /// (clamp value min max) - Clamp value between min and max
    fn eval_clamp(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "clamp".to_string(),
                reason: "Expected 3 arguments: value, min, max".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let min_val = self.evaluate_expression(&args[1].value)?;
        let max_val = self.evaluate_expression(&args[2].value)?;

        match (&value, &min_val, &max_val) {
            (Value::Int(v), Value::Int(min), Value::Int(max)) => Ok(Value::Int(*v.clamp(min, max))),
            (Value::Float(v), Value::Float(min), Value::Float(max)) => {
                Ok(Value::Float(v.clamp(*min, *max)))
            }
            _ => {
                let v = value.as_float()?;
                let min = min_val.as_float()?;
                let max = max_val.as_float()?;
                Ok(Value::Float(v.clamp(min, max)))
            }
        }
    }

    /// (random) - Generate random number between 0 and 1
    fn eval_random(&mut self, _args: &[crate::parser::Argument]) -> Result<Value> {
        use std::collections::hash_map::RandomState;
        use std::hash::BuildHasher;

        // Simple pseudo-random using current time + hashstate
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();

        let state = RandomState::new();

        let hash = state.hash_one(now);
        let random = (hash as f64) / (u64::MAX as f64);

        Ok(Value::Float(random))
    }

    // ============================================================================
    // STRING PREDICATES (Python str methods)
    // ============================================================================

    /// (isdigit? s) - Check if all characters are digits
    fn eval_isdigit(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "isdigit?".to_string(),
                reason: "Expected 1 argument: string".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let s = value.as_string()?;
        Ok(Value::Bool(
            !s.is_empty() && s.chars().all(|c| c.is_numeric()),
        ))
    }

    /// (isalpha? s) - Check if all characters are alphabetic
    fn eval_isalpha(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "isalpha?".to_string(),
                reason: "Expected 1 argument: string".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let s = value.as_string()?;
        Ok(Value::Bool(
            !s.is_empty() && s.chars().all(|c| c.is_alphabetic()),
        ))
    }

    /// (isalnum? s) - Check if all characters are alphanumeric
    fn eval_isalnum(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "isalnum?".to_string(),
                reason: "Expected 1 argument: string".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let s = value.as_string()?;
        Ok(Value::Bool(
            !s.is_empty() && s.chars().all(|c| c.is_alphanumeric()),
        ))
    }

    /// (isspace? s) - Check if all characters are whitespace
    fn eval_isspace(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "isspace?".to_string(),
                reason: "Expected 1 argument: string".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let s = value.as_string()?;
        Ok(Value::Bool(
            !s.is_empty() && s.chars().all(|c| c.is_whitespace()),
        ))
    }

    /// (blank? s) - Check if string is empty or only whitespace
    fn eval_blank(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "blank?".to_string(),
                reason: "Expected 1 argument: string".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let s = value.as_string()?;
        Ok(Value::Bool(s.trim().is_empty()))
    }

    // ============================================================================
    // ARRAY ADVANCED OPERATIONS
    // ============================================================================

    /// (find-index collection predicate) - Find index of first matching element
    fn eval_find_index(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "find-index".to_string(),
                reason: "Expected 2 arguments: collection and predicate".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let array = collection.as_array()?;
        let func = self.evaluate_expression(&args[1].value)?;

        match func {
            Value::Function { params, body, .. } => {
                for (i, elem) in array.iter().enumerate() {
                    self.env.enter_scope();
                    if !params.is_empty() {
                        let _ = self.env.set(&params[0], elem.clone());
                    }

                    let result = self.evaluate_expression(&body)?;
                    self.env.exit_scope();

                    if let Value::Bool(true) = result {
                        return Ok(Value::Int(i as i64));
                    }
                }
                Ok(Value::Int(-1)) // Not found
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (remove collection element) - Remove all occurrences of element
    fn eval_remove(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "remove".to_string(),
                reason: "Expected 2 arguments: collection and element".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let element = self.evaluate_expression(&args[1].value)?;
        let array = collection.as_array()?;

        let result: Vec<Value> = array.iter().filter(|&v| v != &element).cloned().collect();

        Ok(Value::Array(Arc::new(result)))
    }

    /// (insert-at collection index element) - Insert element at index
    fn eval_insert_at(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "insert-at".to_string(),
                reason: "Expected 3 arguments: collection, index, element".to_string(),
            });
        }

        let collection = self.evaluate_expression(&args[0].value)?;
        let index_val = self.evaluate_expression(&args[1].value)?;
        let element = self.evaluate_expression(&args[2].value)?;

        let array = collection.as_array()?;
        let index = index_val.as_int()? as usize;

        let mut result = array.to_vec();
        if index > result.len() {
            return Err(Error::TypeError {
                expected: format!("index 0-{}", result.len()),
                got: format!("{}", index),
            });
        }

        result.insert(index, element);
        Ok(Value::Array(Arc::new(result)))
    }

    // ============================================================================
    // FUNCTIONAL PROGRAMMING UTILITIES
    // ============================================================================

    /// (apply function list) - Apply function to argument list
    fn eval_apply(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "apply".to_string(),
                reason: "Expected 2 arguments: function and argument list".to_string(),
            });
        }

        let func = self.evaluate_expression(&args[0].value)?;
        let arg_list = self.evaluate_expression(&args[1].value)?;
        let array = arg_list.as_array()?;

        match func {
            Value::Function { params, body, .. } => {
                self.env.enter_scope();

                for (i, param) in params.iter().enumerate() {
                    if i < array.len() {
                        let _ = self.env.set(param, array[i].clone());
                    }
                }

                let result = self.evaluate_expression(&body)?;
                self.env.exit_scope();

                Ok(result)
            }
            _ => Err(Error::TypeError {
                expected: "function".to_string(),
                got: func.type_name(),
            }),
        }
    }

    /// (compose f g) - Function composition: (compose f g)(x) = f(g(x))
    fn eval_compose(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "compose".to_string(),
                reason: "Expected 2 arguments: two functions to compose".to_string(),
            });
        }

        let _f = self.evaluate_expression(&args[0].value)?;
        let _g = self.evaluate_expression(&args[1].value)?;

        // For now, return a placeholder - full implementation would require storing closures
        Err(Error::TypeError {
            expected: "compose not yet fully implemented".to_string(),
            got: "use nested calls instead".to_string(),
        })
    }

    /// (pipe value ...functions) - Apply functions in sequence (Unix pipe-style)
    fn eval_pipe(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "pipe".to_string(),
                reason: "Expected at least 2 arguments: initial value and functions".to_string(),
            });
        }

        let mut result = self.evaluate_expression(&args[0].value)?;

        for arg in &args[1..] {
            let func = self.evaluate_expression(&arg.value)?;

            match func {
                Value::Function { params, body, .. } => {
                    self.env.enter_scope();
                    if !params.is_empty() {
                        let _ = self.env.set(&params[0], result.clone());
                    }
                    result = self.evaluate_expression(&body)?;
                    self.env.exit_scope();
                }
                _ => {
                    return Err(Error::TypeError {
                        expected: "function".to_string(),
                        got: func.type_name(),
                    })
                }
            }
        }

        Ok(result)
    }

    /// (partial function ...args) - Partial function application
    fn eval_partial(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "partial".to_string(),
                reason: "Expected at least 2 arguments: function and partial arguments".to_string(),
            });
        }

        // For now, return placeholder - full implementation requires closure storage
        Err(Error::TypeError {
            expected: "partial not yet fully implemented".to_string(),
            got: "use lambda instead".to_string(),
        })
    }

    // ============================================================================
    // REGEX OPERATIONS
    // ============================================================================

    /// (regex-match pattern string) - Check if string matches regex pattern
    fn eval_regex_match(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "regex-match".to_string(),
                reason: "Expected 2 arguments: pattern and string".to_string(),
            });
        }

        let pattern_val = self.evaluate_expression(&args[0].value)?;
        let pattern = pattern_val.as_string()?.to_string();
        let text_val = self.evaluate_expression(&args[1].value)?;
        let text = text_val.as_string()?.to_string();

        match regex::Regex::new(&pattern) {
            Ok(re) => Ok(Value::Bool(re.is_match(&text))),
            Err(e) => Err(Error::TypeError {
                expected: "valid regex pattern".to_string(),
                got: format!("invalid regex: {}", e),
            }),
        }
    }

    /// (regex-replace pattern string replacement) - Replace matches with replacement
    fn eval_regex_replace(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::InvalidArguments {
                tool: "regex-replace".to_string(),
                reason: "Expected 3 arguments: pattern, string, replacement".to_string(),
            });
        }

        let pattern_val = self.evaluate_expression(&args[0].value)?;
        let pattern = pattern_val.as_string()?.to_string();
        let text_val = self.evaluate_expression(&args[1].value)?;
        let text = text_val.as_string()?.to_string();
        let repl_val = self.evaluate_expression(&args[2].value)?;
        let replacement = repl_val.as_string()?.to_string();

        match regex::Regex::new(&pattern) {
            Ok(re) => {
                let result = re.replace_all(&text, replacement.as_str()).to_string();
                Ok(Value::String(result))
            }
            Err(e) => Err(Error::TypeError {
                expected: "valid regex pattern".to_string(),
                got: format!("invalid regex: {}", e),
            }),
        }
    }

    /// (regex-split pattern string) - Split string by regex pattern
    fn eval_regex_split(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "regex-split".to_string(),
                reason: "Expected 2 arguments: pattern and string".to_string(),
            });
        }

        let pattern_val = self.evaluate_expression(&args[0].value)?;
        let pattern = pattern_val.as_string()?.to_string();
        let text_val = self.evaluate_expression(&args[1].value)?;
        let text = text_val.as_string()?.to_string();

        match regex::Regex::new(&pattern) {
            Ok(re) => {
                let parts: Vec<Value> = re
                    .split(&text)
                    .map(|s| Value::String(s.to_string()))
                    .collect();
                Ok(Value::Array(Arc::new(parts)))
            }
            Err(e) => Err(Error::TypeError {
                expected: "valid regex pattern".to_string(),
                got: format!("invalid regex: {}", e),
            }),
        }
    }

    /// (regex-find-all pattern string) - Find all matches
    fn eval_regex_find_all(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "regex-find-all".to_string(),
                reason: "Expected 2 arguments: pattern and string".to_string(),
            });
        }
        let pattern_val = self.evaluate_expression(&args[0].value)?;
        let pattern = pattern_val.as_string()?.to_string();
        let text_val = self.evaluate_expression(&args[1].value)?;
        let text = text_val.as_string()?.to_string();

        match regex::Regex::new(&pattern) {
            Ok(re) => {
                let matches: Vec<Value> = re
                    .find_iter(&text)
                    .map(|m| Value::String(m.as_str().to_string()))
                    .collect();
                Ok(Value::Array(Arc::new(matches)))
            }
            Err(e) => Err(Error::TypeError {
                expected: "valid regex pattern".to_string(),
                got: format!("invalid regex: {}", e),
            }),
        }
    }

    // =========================================================================
    // HIGH PRIORITY ALIASES - Python/JavaScript Compatibility
    // =========================================================================

    /// (toLowerCase string) - Convert string to lowercase (JavaScript style)
    fn eval_to_lower_case(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "toLowerCase".to_string(),
                reason: "Expected 1 argument: string".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let s = value.as_string()?;
        Ok(Value::String(s.to_lowercase()))
    }

    /// (toUpperCase string) - Convert string to uppercase (JavaScript style)
    fn eval_to_upper_case(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "toUpperCase".to_string(),
                reason: "Expected 1 argument: string".to_string(),
            });
        }

        let value = self.evaluate_expression(&args[0].value)?;
        let s = value.as_string()?;
        Ok(Value::String(s.to_uppercase()))
    }

    /// (charAt string index) - Get character at index (JavaScript style)
    fn eval_char_at(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "charAt".to_string(),
                reason: "Expected 2 arguments: string and index".to_string(),
            });
        }

        let string_val = self.evaluate_expression(&args[0].value)?;
        let s = string_val.as_string()?;

        let index_val = self.evaluate_expression(&args[1].value)?;
        let index = index_val.as_int()? as usize;

        // Get character at index (handle multi-byte UTF-8)
        let ch = s.chars().nth(index);

        if let Some(ch) = ch {
            Ok(Value::String(ch.to_string()))
        } else {
            // JavaScript returns empty string for out-of-bounds
            Ok(Value::String(String::new()))
        }
    }

    /// (chr code) - Convert character code to character (Python style)
    fn eval_chr(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "chr".to_string(),
                reason: "Expected 1 argument: character code (integer)".to_string(),
            });
        }

        let code_val = self.evaluate_expression(&args[0].value)?;
        let code = code_val.as_int()?;

        // Validate Unicode range
        let ch = char::from_u32(code as u32).ok_or_else(|| Error::TypeError {
            expected: "valid Unicode code point (0-0x10FFFF)".to_string(),
            got: format!("{}", code),
        })?;

        Ok(Value::String(ch.to_string()))
    }

    /// (ord character) - Convert character to code (Python style)
    fn eval_ord(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ord".to_string(),
                reason: "Expected 1 argument: character (string of length 1)".to_string(),
            });
        }

        let char_val = self.evaluate_expression(&args[0].value)?;
        let s = char_val.as_string()?;

        if s.chars().count() != 1 {
            return Err(Error::InvalidArguments {
                tool: "ord".to_string(),
                reason: format!(
                    "Expected single character, got string of length {}",
                    s.chars().count()
                ),
            });
        }

        let ch = s.chars().next().unwrap();
        Ok(Value::Int(ch as i64))
    }

    /// (substring string start [end]) - Extract substring (JavaScript style)
    fn eval_substring(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "substring".to_string(),
                reason: "Expected 2-3 arguments: string, start, [end]".to_string(),
            });
        }

        let string_val = self.evaluate_expression(&args[0].value)?;
        let s = string_val.as_string()?;

        let start_val = self.evaluate_expression(&args[1].value)?;
        let start = start_val.as_int()? as usize;

        let chars: Vec<char> = s.chars().collect();
        let len = chars.len();

        // Clamp start to string length
        let start = start.min(len);

        let end = if args.len() >= 3 {
            let end_val = self.evaluate_expression(&args[2].value)?;
            (end_val.as_int()? as usize).min(len)
        } else {
            len
        };

        // JavaScript substring swaps start/end if start > end
        let (start, end) = if start > end {
            (end, start)
        } else {
            (start, end)
        };

        let result: String = chars[start..end].iter().collect();
        Ok(Value::String(result))
    }

    /// (lastIndexOf collection item) - Find last occurrence of item (JavaScript style)
    fn eval_last_index_of(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::InvalidArguments {
                tool: "lastIndexOf".to_string(),
                reason: "Expected 2 arguments: collection and item".to_string(),
            });
        }

        let collection_val = self.evaluate_expression(&args[0].value)?;
        let item_val = self.evaluate_expression(&args[1].value)?;

        match collection_val {
            Value::Array(ref arr) => {
                // Search from end to beginning
                for (i, val) in arr.iter().enumerate().rev() {
                    if self.values_equal(val, &item_val) {
                        return Ok(Value::Int(i as i64));
                    }
                }
                Ok(Value::Int(-1)) // Not found
            }
            Value::String(ref s) => {
                let search = item_val.as_string()?;
                if let Some(pos) = s.rfind(search) {
                    Ok(Value::Int(pos as i64))
                } else {
                    Ok(Value::Int(-1))
                }
            }
            _ => Err(Error::TypeError {
                expected: "array or string".to_string(),
                got: collection_val.type_name(),
            }),
        }
    }

    // =========================================================================
    // STREAMING OPERATIONS (Real-time blockchain events)
    // =========================================================================

    /// (stream-connect url &key programs tokens accounts event-types success-only)
    fn eval_stream_connect(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Evaluate all arguments to Values
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.evaluate_expression(&arg.value)?);
        }

        // Call the streaming function with evaluated arguments
        crate::runtime::streaming::stream_connect(&evaluated_args)
    }

    /// (stream-poll stream-id &key limit)
    fn eval_stream_poll(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Evaluate all arguments to Values
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.evaluate_expression(&arg.value)?);
        }

        // Call the streaming function with evaluated arguments
        crate::runtime::streaming::stream_poll(&evaluated_args)
    }

    /// (stream-wait stream-id &key timeout)
    fn eval_stream_wait(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Evaluate all arguments to Values
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.evaluate_expression(&arg.value)?);
        }

        // Call the streaming function with evaluated arguments
        crate::runtime::streaming::stream_wait(&evaluated_args)
    }

    /// (stream-close stream-id)
    fn eval_stream_close(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Evaluate all arguments to Values
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.evaluate_expression(&arg.value)?);
        }

        // Call the streaming function with evaluated arguments
        crate::runtime::streaming::stream_close(&evaluated_args)
    }

    /// (osvm-stream &key alias programs tokens) - Spawn internal stream server and connect
    /// This is a convenience function that combines server spawning + stream-connect
    /// The server automatically terminates when the script ends
    fn eval_osvm_stream(&mut self, args: &[crate::parser::Argument]) -> Result<Value> {
        // Evaluate all arguments to Values
        let mut evaluated_args = Vec::new();
        for arg in args {
            evaluated_args.push(self.evaluate_expression(&arg.value)?);
        }

        // Call the streaming helper
        crate::runtime::streaming::osvm_stream(&evaluated_args)
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
