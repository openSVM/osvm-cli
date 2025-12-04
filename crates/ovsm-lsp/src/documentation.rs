//! Documentation for OVSM built-in functions and special forms
//!
//! This module provides hover documentation for the LSP server.
//! Each built-in has a signature, description, and examples.

use std::collections::HashMap;
use std::sync::LazyLock;

/// Documentation entry for a built-in function or special form
#[derive(Debug, Clone)]
pub struct DocEntry {
    /// Function signature
    pub signature: &'static str,
    /// Short description
    pub description: &'static str,
    /// Usage examples
    pub examples: &'static [&'static str],
    /// Category for grouping
    pub category: DocCategory,
}

/// Categories for documentation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocCategory {
    /// Special forms (if, define, let, etc.)
    SpecialForm,
    /// Control flow (while, for, cond)
    ControlFlow,
    /// Arithmetic operations
    Arithmetic,
    /// Comparison operations
    Comparison,
    /// Logical operations
    Logical,
    /// String operations
    String,
    /// List/Array operations
    Collection,
    /// Type predicates
    TypePredicate,
    /// Type conversion
    TypeConversion,
    /// Math functions
    Math,
    /// Bitwise operations
    Bitwise,
    /// I/O and debugging
    IO,
}

/// Get documentation for a symbol
pub fn get_documentation(symbol: &str) -> Option<&'static DocEntry> {
    DOCUMENTATION.get(symbol)
}

/// Get all documentation entries
pub fn all_documentation() -> impl Iterator<Item = (&'static str, &'static DocEntry)> {
    DOCUMENTATION.iter().map(|(k, v)| (*k, v))
}

/// Format documentation as Markdown for hover display
pub fn format_hover(entry: &DocEntry) -> String {
    let mut result = String::new();

    // Signature in code block
    result.push_str("```ovsm\n");
    result.push_str(entry.signature);
    result.push_str("\n```\n\n");

    // Description
    result.push_str(entry.description);
    result.push_str("\n\n");

    // Examples
    if !entry.examples.is_empty() {
        result.push_str("**Examples:**\n```ovsm\n");
        for example in entry.examples {
            result.push_str(example);
            result.push('\n');
        }
        result.push_str("```\n");
    }

    result
}

/// Static documentation database
static DOCUMENTATION: LazyLock<HashMap<&'static str, DocEntry>> = LazyLock::new(|| {
    let mut docs = HashMap::new();

    // ========================================================================
    // Special Forms
    // ========================================================================

    docs.insert("define", DocEntry {
        signature: "(define name value)",
        description: "Defines a new variable with the given name and value. The variable can be reassigned later using `set!`.",
        examples: &[
            "(define x 10)",
            "(define greeting \"Hello, World!\")",
            "(define pi 3.14159)",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert("set!", DocEntry {
        signature: "(set! name new-value)",
        description: "Mutates an existing variable. The variable must have been previously defined with `define`. Note: Cannot be used for field access; use `merge` for objects.",
        examples: &[
            "(define count 0)",
            "(set! count (+ count 1))",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert("if", DocEntry {
        signature: "(if condition then-expr else-expr)",
        description: "Conditional expression. Evaluates `condition`; if true, returns `then-expr`, otherwise returns `else-expr`. Both branches must be provided.",
        examples: &[
            "(if (> x 0) \"positive\" \"non-positive\")",
            "(if (null? data) \"empty\" (length data))",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert("let", DocEntry {
        signature: "(let ((var1 val1) (var2 val2) ...) body...)",
        description: "Creates local variable bindings. All bindings are evaluated in parallel (bindings cannot reference each other). The body expressions are evaluated in sequence, returning the last value.",
        examples: &[
            "(let ((x 10) (y 20)) (+ x y))",
            "(let ((name \"Alice\")) (concat \"Hello, \" name))",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert("let*", DocEntry {
        signature: "(let* ((var1 val1) (var2 val2) ...) body...)",
        description: "Creates local variable bindings sequentially. Each binding can reference previously defined bindings in the same let* form.",
        examples: &[
            "(let* ((x 10) (y (+ x 5))) (+ x y))",
            "(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert("lambda", DocEntry {
        signature: "(lambda (param1 param2 ...) body)",
        description: "Creates an anonymous function. Parameters can include `&optional` for optional params, `&rest` for variadic params, and `&key` for keyword params.",
        examples: &[
            "(lambda (x) (* x x))",
            "(lambda (x y) (+ x y))",
            "(lambda (&optional (n 10)) (* n 2))",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert("do", DocEntry {
        signature: "(do expr1 expr2 ... exprN)",
        description: "Evaluates expressions in sequence and returns the value of the last expression. Useful for side effects.",
        examples: &[
            "(do (log :message \"Starting\") (+ 1 2))",
            "(do (set! x 10) (set! y 20) (+ x y))",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert(
        "progn",
        DocEntry {
            signature: "(progn expr1 expr2 ... exprN)",
            description:
                "Alias for `do`. Evaluates expressions in sequence and returns the last value.",
            examples: &["(progn (define x 1) (+ x 1))"],
            category: DocCategory::SpecialForm,
        },
    );

    docs.insert("const", DocEntry {
        signature: "(const NAME value)",
        description: "Defines a constant that cannot be reassigned. By convention, constant names are UPPERCASE.",
        examples: &[
            "(const PI 3.14159)",
            "(const MAX_RETRIES 3)",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert("defun", DocEntry {
        signature: "(defun name (params...) body)",
        description: "Defines a named function. The function is bound to `name` in the current environment.",
        examples: &[
            "(defun square (x) (* x x))",
            "(defun greet (name) (concat \"Hello, \" name))",
        ],
        category: DocCategory::SpecialForm,
    });

    docs.insert(
        "defn",
        DocEntry {
            signature: "(defn name (params...) body)",
            description: "Alias for `defun`. Defines a named function.",
            examples: &["(defn double (x) (* x 2))"],
            category: DocCategory::SpecialForm,
        },
    );

    // ========================================================================
    // Control Flow
    // ========================================================================

    docs.insert(
        "while",
        DocEntry {
            signature: "(while condition body...)",
            description:
                "Executes body expressions repeatedly while condition is true. Returns null.",
            examples: &["(while (< i 10) (log :value i) (set! i (+ i 1)))"],
            category: DocCategory::ControlFlow,
        },
    );

    docs.insert(
        "for",
        DocEntry {
            signature: "(for (var collection) body...)",
            description: "Iterates over a collection, binding each element to `var`. Returns null.",
            examples: &[
                "(for (item [1 2 3]) (log :value item))",
                "(for (char \"hello\") (log :char char))",
            ],
            category: DocCategory::ControlFlow,
        },
    );

    docs.insert("when", DocEntry {
        signature: "(when condition body...)",
        description: "Executes body expressions only if condition is true. Returns the last body value or null.",
        examples: &[
            "(when (> x 0) (log :message \"positive\"))",
        ],
        category: DocCategory::ControlFlow,
    });

    docs.insert(
        "unless",
        DocEntry {
            signature: "(unless condition body...)",
            description:
                "Executes body expressions only if condition is false. Opposite of `when`.",
            examples: &["(unless (null? data) (process data))"],
            category: DocCategory::ControlFlow,
        },
    );

    docs.insert("cond", DocEntry {
        signature: "(cond (test1 result1) (test2 result2) ... (else default))",
        description: "Multi-way conditional. Tests each condition in order; returns the result of the first true test. The `else` clause provides a default.",
        examples: &[
            "(cond ((< x 0) \"negative\") ((= x 0) \"zero\") (else \"positive\"))",
        ],
        category: DocCategory::ControlFlow,
    });

    docs.insert("case", DocEntry {
        signature: "(case expr (value1 result1) (value2 result2) ... (else default))",
        description: "Pattern matching by value. Compares expr against each value and returns the matching result.",
        examples: &[
            "(case color ((\"red\") \"stop\") ((\"green\") \"go\") (else \"caution\"))",
        ],
        category: DocCategory::ControlFlow,
    });

    docs.insert("loop", DocEntry {
        signature: "(loop for var from/in ... [when/unless test] [sum/collect/count expr])",
        description: "Common Lisp-style loop macro. Supports numeric iteration, collection iteration, filtering, and accumulation.",
        examples: &[
            "(loop for i from 1 to 10 sum i)",
            "(loop for x in [1 2 3 4] when (evenp x) collect (* x x))",
            "(loop for i from 1 to 100 count (oddp i))",
        ],
        category: DocCategory::ControlFlow,
    });

    docs.insert("catch", DocEntry {
        signature: "(catch 'tag body...)",
        description: "Establishes a catch point for non-local exit. If a `throw` with matching tag occurs in body, control returns here with the thrown value.",
        examples: &[
            "(catch 'done (process-items) (throw 'done result))",
        ],
        category: DocCategory::ControlFlow,
    });

    docs.insert("throw", DocEntry {
        signature: "(throw 'tag value)",
        description: "Performs non-local exit to a matching `catch`. Returns `value` from the catch expression.",
        examples: &[
            "(throw 'done \"finished\")",
        ],
        category: DocCategory::ControlFlow,
    });

    // ========================================================================
    // Arithmetic
    // ========================================================================

    docs.insert(
        "+",
        DocEntry {
            signature: "(+ num1 num2 ...)",
            description: "Adds numbers. Variadic: accepts any number of arguments.",
            examples: &["(+ 1 2)       ;; => 3", "(+ 1 2 3 4)   ;; => 10"],
            category: DocCategory::Arithmetic,
        },
    );

    docs.insert("-", DocEntry {
        signature: "(- num1 num2 ...)",
        description: "Subtracts numbers. With one argument, returns negation. With multiple, subtracts from left to right.",
        examples: &[
            "(- 5)         ;; => -5",
            "(- 10 3)      ;; => 7",
            "(- 10 3 2)    ;; => 5",
        ],
        category: DocCategory::Arithmetic,
    });

    docs.insert(
        "*",
        DocEntry {
            signature: "(* num1 num2 ...)",
            description: "Multiplies numbers. Variadic: accepts any number of arguments.",
            examples: &["(* 2 3)       ;; => 6", "(* 2 3 4)     ;; => 24"],
            category: DocCategory::Arithmetic,
        },
    );

    docs.insert("/", DocEntry {
        signature: "(/ num1 num2 ...)",
        description: "Divides numbers. With one argument, returns reciprocal. Division by zero raises an error.",
        examples: &[
            "(/ 10 2)      ;; => 5",
            "(/ 100 2 5)   ;; => 10",
        ],
        category: DocCategory::Arithmetic,
    });

    docs.insert(
        "%",
        DocEntry {
            signature: "(% dividend divisor)",
            description: "Returns the remainder of division (modulo operation).",
            examples: &["(% 10 3)      ;; => 1", "(% 17 5)      ;; => 2"],
            category: DocCategory::Arithmetic,
        },
    );

    docs.insert("mod", DocEntry {
        signature: "(mod dividend divisor)",
        description: "Returns the modulus (always positive remainder). Different from `%` for negative numbers.",
        examples: &[
            "(mod 10 3)    ;; => 1",
            "(mod -10 3)   ;; => 2",
        ],
        category: DocCategory::Arithmetic,
    });

    docs.insert(
        "rem",
        DocEntry {
            signature: "(rem dividend divisor)",
            description: "Returns the remainder with the sign of the dividend.",
            examples: &["(rem 10 3)    ;; => 1", "(rem -10 3)   ;; => -1"],
            category: DocCategory::Arithmetic,
        },
    );

    // ========================================================================
    // Comparison
    // ========================================================================

    docs.insert(
        "=",
        DocEntry {
            signature: "(= val1 val2)",
            description:
                "Equality comparison. In OVSM LISP, `=` is used for comparison (not assignment).",
            examples: &["(= 1 1)       ;; => true", "(= \"a\" \"b\") ;; => false"],
            category: DocCategory::Comparison,
        },
    );

    docs.insert(
        "==",
        DocEntry {
            signature: "(== val1 val2)",
            description: "Equality comparison. Same as `=`.",
            examples: &["(== x 10)"],
            category: DocCategory::Comparison,
        },
    );

    docs.insert(
        "!=",
        DocEntry {
            signature: "(!= val1 val2)",
            description: "Inequality comparison. Returns true if values are not equal.",
            examples: &["(!= 1 2)      ;; => true"],
            category: DocCategory::Comparison,
        },
    );

    docs.insert(
        "<",
        DocEntry {
            signature: "(< val1 val2)",
            description: "Less than comparison.",
            examples: &["(< 1 2)       ;; => true"],
            category: DocCategory::Comparison,
        },
    );

    docs.insert(
        ">",
        DocEntry {
            signature: "(> val1 val2)",
            description: "Greater than comparison.",
            examples: &["(> 2 1)       ;; => true"],
            category: DocCategory::Comparison,
        },
    );

    docs.insert(
        "<=",
        DocEntry {
            signature: "(<= val1 val2)",
            description: "Less than or equal comparison.",
            examples: &["(<= 1 1)      ;; => true"],
            category: DocCategory::Comparison,
        },
    );

    docs.insert(
        ">=",
        DocEntry {
            signature: "(>= val1 val2)",
            description: "Greater than or equal comparison.",
            examples: &["(>= 2 2)      ;; => true"],
            category: DocCategory::Comparison,
        },
    );

    // ========================================================================
    // Logical
    // ========================================================================

    docs.insert(
        "and",
        DocEntry {
            signature: "(and expr1 expr2 ...)",
            description:
                "Logical AND. Short-circuits: returns first false value or the last value.",
            examples: &[
                "(and true true)   ;; => true",
                "(and true false)  ;; => false",
            ],
            category: DocCategory::Logical,
        },
    );

    docs.insert(
        "or",
        DocEntry {
            signature: "(or expr1 expr2 ...)",
            description: "Logical OR. Short-circuits: returns first true value or the last value.",
            examples: &[
                "(or false true)   ;; => true",
                "(or false false)  ;; => false",
            ],
            category: DocCategory::Logical,
        },
    );

    docs.insert(
        "not",
        DocEntry {
            signature: "(not expr)",
            description: "Logical NOT. Returns true if expr is false/null, false otherwise.",
            examples: &[
                "(not true)        ;; => false",
                "(not null)        ;; => true",
            ],
            category: DocCategory::Logical,
        },
    );

    // ========================================================================
    // Type Predicates
    // ========================================================================

    docs.insert(
        "null?",
        DocEntry {
            signature: "(null? value)",
            description: "Returns true if value is null.",
            examples: &[
                "(null? null)      ;; => true",
                "(null? 0)         ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert(
        "empty?",
        DocEntry {
            signature: "(empty? collection)",
            description: "Returns true if collection (array/string) is empty.",
            examples: &[
                "(empty? [])       ;; => true",
                "(empty? \"\")       ;; => true",
                "(empty? [1 2 3])  ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert(
        "evenp",
        DocEntry {
            signature: "(evenp n)",
            description: "Returns true if n is an even number.",
            examples: &[
                "(evenp 4)         ;; => true",
                "(evenp 5)         ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert(
        "oddp",
        DocEntry {
            signature: "(oddp n)",
            description: "Returns true if n is an odd number.",
            examples: &[
                "(oddp 3)          ;; => true",
                "(oddp 4)          ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert(
        "zerop",
        DocEntry {
            signature: "(zerop n)",
            description: "Returns true if n is zero.",
            examples: &[
                "(zerop 0)         ;; => true",
                "(zerop 1)         ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert(
        "positivep",
        DocEntry {
            signature: "(positivep n)",
            description: "Returns true if n is positive (> 0).",
            examples: &[
                "(positivep 5)     ;; => true",
                "(positivep -1)    ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert(
        "negativep",
        DocEntry {
            signature: "(negativep n)",
            description: "Returns true if n is negative (< 0).",
            examples: &[
                "(negativep -5)    ;; => true",
                "(negativep 0)     ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert("typeof", DocEntry {
        signature: "(typeof value)",
        description: "Returns the type of value as a string: \"integer\", \"float\", \"string\", \"boolean\", \"array\", \"object\", \"function\", or \"null\".",
        examples: &[
            "(typeof 42)       ;; => \"integer\"",
            "(typeof \"hi\")     ;; => \"string\"",
        ],
        category: DocCategory::TypePredicate,
    });

    docs.insert(
        "type-of",
        DocEntry {
            signature: "(type-of value)",
            description: "Alias for `typeof`. Returns the type of value as a string.",
            examples: &["(type-of [1 2 3]) ;; => \"array\""],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert("atom", DocEntry {
        signature: "(atom value)",
        description: "Returns true if value is an atom (not a list/array). Numbers, strings, booleans, null are atoms.",
        examples: &[
            "(atom 42)         ;; => true",
            "(atom [1 2])      ;; => false",
        ],
        category: DocCategory::TypePredicate,
    });

    docs.insert(
        "consp",
        DocEntry {
            signature: "(consp value)",
            description: "Returns true if value is a cons cell (non-empty list/array).",
            examples: &[
                "(consp [1 2 3])   ;; => true",
                "(consp [])        ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    docs.insert(
        "listp",
        DocEntry {
            signature: "(listp value)",
            description: "Returns true if value is a list (array or null).",
            examples: &[
                "(listp [1 2 3])   ;; => true",
                "(listp null)      ;; => true",
                "(listp \"str\")     ;; => false",
            ],
            category: DocCategory::TypePredicate,
        },
    );

    // ========================================================================
    // Collection Operations
    // ========================================================================

    docs.insert(
        "length",
        DocEntry {
            signature: "(length collection)",
            description: "Returns the length of a collection (array or string).",
            examples: &["(length [1 2 3])  ;; => 3", "(length \"hello\") ;; => 5"],
            category: DocCategory::Collection,
        },
    );

    docs.insert("get", DocEntry {
        signature: "(get collection key)",
        description: "Gets a value from collection. For arrays: `(get arr index)`. For objects: `(get obj \"key\")`. Returns null if not found.",
        examples: &[
            "(get [10 20 30] 1)    ;; => 20",
            "(get {:name \"Bob\"} \"name\")  ;; => \"Bob\"",
        ],
        category: DocCategory::Collection,
    });

    docs.insert(
        "first",
        DocEntry {
            signature: "(first collection)",
            description: "Returns the first element of a collection.",
            examples: &["(first [1 2 3])   ;; => 1"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "rest",
        DocEntry {
            signature: "(rest collection)",
            description: "Returns all elements except the first.",
            examples: &["(rest [1 2 3])    ;; => [2 3]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "last",
        DocEntry {
            signature: "(last collection)",
            description: "Returns the last element of a collection.",
            examples: &["(last [1 2 3])    ;; => 3"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "cons",
        DocEntry {
            signature: "(cons element list)",
            description: "Constructs a new list by prepending element to list.",
            examples: &["(cons 0 [1 2 3])  ;; => [0 1 2 3]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "append",
        DocEntry {
            signature: "(append list1 list2 ...)",
            description: "Concatenates lists together.",
            examples: &["(append [1 2] [3 4])  ;; => [1 2 3 4]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "reverse",
        DocEntry {
            signature: "(reverse collection)",
            description: "Returns a reversed copy of the collection.",
            examples: &["(reverse [1 2 3])     ;; => [3 2 1]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "range",
        DocEntry {
            signature: "(range start end)",
            description: "Creates an array of integers from start to end (exclusive).",
            examples: &[
                "(range 0 5)       ;; => [0 1 2 3 4]",
                "(range 1 4)       ;; => [1 2 3]",
            ],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "mapcar",
        DocEntry {
            signature: "(mapcar function list)",
            description:
                "Applies function to each element of list, returning a new list of results.",
            examples: &["(mapcar (lambda (x) (* x 2)) [1 2 3])  ;; => [2 4 6]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "filter",
        DocEntry {
            signature: "(filter predicate list)",
            description: "Returns elements for which predicate returns true.",
            examples: &["(filter evenp [1 2 3 4 5])  ;; => [2 4]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "reduce",
        DocEntry {
            signature: "(reduce function list &optional initial)",
            description: "Reduces list to a single value by applying function cumulatively.",
            examples: &[
                "(reduce + [1 2 3 4])       ;; => 10",
                "(reduce + [1 2 3] 10)      ;; => 16",
            ],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "member",
        DocEntry {
            signature: "(member item list)",
            description: "Returns the tail of list starting with item, or null if not found.",
            examples: &[
                "(member 2 [1 2 3])  ;; => [2 3]",
                "(member 5 [1 2 3])  ;; => null",
            ],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "assoc",
        DocEntry {
            signature: "(assoc key alist)",
            description: "Finds the pair with matching key in an association list.",
            examples: &["(assoc \"a\" [[\"a\" 1] [\"b\" 2]])  ;; => [\"a\" 1]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "elt",
        DocEntry {
            signature: "(elt sequence index)",
            description: "Returns the element at index in sequence.",
            examples: &["(elt [10 20 30] 1)  ;; => 20"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "subseq",
        DocEntry {
            signature: "(subseq sequence start &optional end)",
            description: "Returns a subsequence from start to end (exclusive).",
            examples: &[
                "(subseq \"hello\" 0 2)   ;; => \"he\"",
                "(subseq [1 2 3 4] 1 3)  ;; => [2 3]",
            ],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "sort",
        DocEntry {
            signature: "(sort list &optional comparator)",
            description: "Returns a sorted copy of the list.",
            examples: &["(sort [3 1 4 1 5])  ;; => [1 1 3 4 5]"],
            category: DocCategory::Collection,
        },
    );

    // ========================================================================
    // String Operations
    // ========================================================================

    docs.insert(
        "concat",
        DocEntry {
            signature: "(concat str1 str2 ...)",
            description: "Concatenates strings together.",
            examples: &["(concat \"Hello\" \" \" \"World\")  ;; => \"Hello World\""],
            category: DocCategory::String,
        },
    );

    docs.insert(
        "split",
        DocEntry {
            signature: "(split string delimiter)",
            description: "Splits a string by delimiter into an array.",
            examples: &["(split \"a,b,c\" \",\")  ;; => [\"a\" \"b\" \"c\"]"],
            category: DocCategory::String,
        },
    );

    docs.insert(
        "join",
        DocEntry {
            signature: "(join list delimiter)",
            description: "Joins list elements into a string with delimiter.",
            examples: &["(join [\"a\" \"b\" \"c\"] \"-\")  ;; => \"a-b-c\""],
            category: DocCategory::String,
        },
    );

    docs.insert(
        "replace",
        DocEntry {
            signature: "(replace string pattern replacement)",
            description: "Replaces occurrences of pattern with replacement in string.",
            examples: &["(replace \"hello world\" \"world\" \"there\")  ;; => \"hello there\""],
            category: DocCategory::String,
        },
    );

    docs.insert(
        "trim",
        DocEntry {
            signature: "(trim string)",
            description: "Removes whitespace from both ends of string.",
            examples: &["(trim \"  hello  \")  ;; => \"hello\""],
            category: DocCategory::String,
        },
    );

    docs.insert(
        "upper",
        DocEntry {
            signature: "(upper string)",
            description: "Converts string to uppercase.",
            examples: &["(upper \"hello\")  ;; => \"HELLO\""],
            category: DocCategory::String,
        },
    );

    docs.insert(
        "lower",
        DocEntry {
            signature: "(lower string)",
            description: "Converts string to lowercase.",
            examples: &["(lower \"HELLO\")  ;; => \"hello\""],
            category: DocCategory::String,
        },
    );

    // ========================================================================
    // Math Functions
    // ========================================================================

    docs.insert(
        "sqrt",
        DocEntry {
            signature: "(sqrt n)",
            description: "Returns the square root of n.",
            examples: &["(sqrt 16)  ;; => 4.0", "(sqrt 2)   ;; => 1.414..."],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "pow",
        DocEntry {
            signature: "(pow base exponent)",
            description: "Returns base raised to the power of exponent.",
            examples: &["(pow 2 10)  ;; => 1024", "(pow 3 3)   ;; => 27"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "expt",
        DocEntry {
            signature: "(expt base exponent)",
            description: "Common Lisp alias for `pow`. Returns base raised to exponent.",
            examples: &["(expt 2 8)  ;; => 256"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "exp",
        DocEntry {
            signature: "(exp n)",
            description: "Returns e raised to the power of n (e^n).",
            examples: &["(exp 1)     ;; => 2.718...", "(exp 0)     ;; => 1"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "ln",
        DocEntry {
            signature: "(ln n)",
            description: "Returns the natural logarithm of n (base e).",
            examples: &["(ln 2.718)  ;; => 1.0 (approx)", "(ln 1)      ;; => 0"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "abs",
        DocEntry {
            signature: "(abs n)",
            description: "Returns the absolute value of n.",
            examples: &["(abs -5)    ;; => 5", "(abs 3)     ;; => 3"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "min",
        DocEntry {
            signature: "(min n1 n2 ...)",
            description: "Returns the minimum of the given numbers.",
            examples: &["(min 3 1 4 1 5)  ;; => 1"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "max",
        DocEntry {
            signature: "(max n1 n2 ...)",
            description: "Returns the maximum of the given numbers.",
            examples: &["(max 3 1 4 1 5)  ;; => 5"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "sin",
        DocEntry {
            signature: "(sin radians)",
            description: "Returns the sine of angle in radians.",
            examples: &["(sin 0)      ;; => 0", "(sin 1.571)  ;; => 1 (approx pi/2)"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "cos",
        DocEntry {
            signature: "(cos radians)",
            description: "Returns the cosine of angle in radians.",
            examples: &["(cos 0)      ;; => 1"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "tan",
        DocEntry {
            signature: "(tan radians)",
            description: "Returns the tangent of angle in radians.",
            examples: &["(tan 0)      ;; => 0"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "floor",
        DocEntry {
            signature: "(floor n)",
            description: "Returns the largest integer less than or equal to n.",
            examples: &["(floor 3.7)   ;; => 3", "(floor -3.2)  ;; => -4"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "ceiling",
        DocEntry {
            signature: "(ceiling n)",
            description: "Returns the smallest integer greater than or equal to n.",
            examples: &["(ceiling 3.2)  ;; => 4", "(ceiling -3.7) ;; => -3"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "round",
        DocEntry {
            signature: "(round n)",
            description: "Rounds n to the nearest integer.",
            examples: &["(round 3.5)  ;; => 4", "(round 3.4)  ;; => 3"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "gcd",
        DocEntry {
            signature: "(gcd a b)",
            description: "Returns the greatest common divisor of a and b.",
            examples: &["(gcd 12 8)   ;; => 4", "(gcd 15 25)  ;; => 5"],
            category: DocCategory::Math,
        },
    );

    docs.insert(
        "lcm",
        DocEntry {
            signature: "(lcm a b)",
            description: "Returns the least common multiple of a and b.",
            examples: &["(lcm 4 6)    ;; => 12"],
            category: DocCategory::Math,
        },
    );

    // ========================================================================
    // Bitwise Operations
    // ========================================================================

    docs.insert(
        "logand",
        DocEntry {
            signature: "(logand n1 n2 ...)",
            description: "Bitwise AND of integers.",
            examples: &["(logand 12 10)  ;; => 8  (1100 & 1010 = 1000)"],
            category: DocCategory::Bitwise,
        },
    );

    docs.insert(
        "logior",
        DocEntry {
            signature: "(logior n1 n2 ...)",
            description: "Bitwise OR of integers.",
            examples: &["(logior 12 10)  ;; => 14 (1100 | 1010 = 1110)"],
            category: DocCategory::Bitwise,
        },
    );

    docs.insert(
        "logxor",
        DocEntry {
            signature: "(logxor n1 n2 ...)",
            description: "Bitwise XOR of integers.",
            examples: &["(logxor 12 10)  ;; => 6  (1100 ^ 1010 = 0110)"],
            category: DocCategory::Bitwise,
        },
    );

    docs.insert(
        "lognot",
        DocEntry {
            signature: "(lognot n)",
            description: "Bitwise NOT of integer.",
            examples: &["(lognot 0)   ;; => -1"],
            category: DocCategory::Bitwise,
        },
    );

    docs.insert(
        "ash",
        DocEntry {
            signature: "(ash n count)",
            description: "Arithmetic shift. Positive count shifts left, negative shifts right.",
            examples: &[
                "(ash 1 4)    ;; => 16  (shift left 4)",
                "(ash 16 -2)  ;; => 4   (shift right 2)",
            ],
            category: DocCategory::Bitwise,
        },
    );

    // ========================================================================
    // Type Conversion
    // ========================================================================

    docs.insert(
        "int",
        DocEntry {
            signature: "(int value)",
            description: "Converts value to integer. Parses strings, truncates floats.",
            examples: &["(int \"42\")   ;; => 42", "(int 3.7)     ;; => 3"],
            category: DocCategory::TypeConversion,
        },
    );

    docs.insert(
        "float",
        DocEntry {
            signature: "(float value)",
            description: "Converts value to floating-point number.",
            examples: &[
                "(float \"3.14\")  ;; => 3.14",
                "(float 42)       ;; => 42.0",
            ],
            category: DocCategory::TypeConversion,
        },
    );

    docs.insert(
        "bool",
        DocEntry {
            signature: "(bool value)",
            description:
                "Converts value to boolean. Null, 0, false, empty string are false; all else true.",
            examples: &[
                "(bool 0)       ;; => false",
                "(bool 1)       ;; => true",
                "(bool \"\")     ;; => false",
            ],
            category: DocCategory::TypeConversion,
        },
    );

    docs.insert(
        "string",
        DocEntry {
            signature: "(string value)",
            description: "Converts value to string representation.",
            examples: &[
                "(string 42)     ;; => \"42\"",
                "(string true)   ;; => \"true\"",
            ],
            category: DocCategory::TypeConversion,
        },
    );

    // ========================================================================
    // I/O and Debugging
    // ========================================================================

    docs.insert(
        "log",
        DocEntry {
            signature: "(log :message msg) or (log :value val)",
            description:
                "Logs a message or value to the console. Use keyword arguments :message or :value.",
            examples: &[
                "(log :message \"Hello\")",
                "(log :value x)",
                "(log :message \"x is\" :value x)",
            ],
            category: DocCategory::IO,
        },
    );

    docs.insert(
        "print",
        DocEntry {
            signature: "(print value)",
            description: "Prints value to output with newline.",
            examples: &["(print \"Hello, World!\")"],
            category: DocCategory::IO,
        },
    );

    docs.insert(
        "assert",
        DocEntry {
            signature: "(assert condition &optional message)",
            description: "Asserts that condition is true. Raises error with message if false.",
            examples: &["(assert (> x 0) \"x must be positive\")"],
            category: DocCategory::IO,
        },
    );

    docs.insert(
        "error",
        DocEntry {
            signature: "(error message)",
            description: "Raises an error with the given message.",
            examples: &["(error \"Something went wrong\")"],
            category: DocCategory::IO,
        },
    );

    docs.insert(
        "now",
        DocEntry {
            signature: "(now)",
            description: "Returns the current Unix timestamp in seconds.",
            examples: &["(now)  ;; => 1700000000 (example)"],
            category: DocCategory::IO,
        },
    );

    // ========================================================================
    // Object Operations
    // ========================================================================

    docs.insert(
        "merge",
        DocEntry {
            signature: "(merge obj1 obj2 ...)",
            description: "Merges objects together. Later objects override earlier ones.",
            examples: &[
                "(merge {:a 1} {:b 2})  ;; => {:a 1 :b 2}",
                "(merge {:a 1} {:a 2})  ;; => {:a 2}",
            ],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "keys",
        DocEntry {
            signature: "(keys object)",
            description: "Returns an array of the object's keys.",
            examples: &["(keys {:a 1 :b 2})  ;; => [\"a\" \"b\"]"],
            category: DocCategory::Collection,
        },
    );

    docs.insert(
        "values",
        DocEntry {
            signature: "(values object)",
            description: "Returns an array of the object's values.",
            examples: &["(values {:a 1 :b 2})  ;; => [1 2]"],
            category: DocCategory::Collection,
        },
    );

    docs
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_documentation_lookup() {
        assert!(get_documentation("define").is_some());
        assert!(get_documentation("if").is_some());
        assert!(get_documentation("+").is_some());
        assert!(get_documentation("nonexistent").is_none());
    }

    #[test]
    fn test_format_hover() {
        let entry = get_documentation("define").unwrap();
        let hover = format_hover(entry);
        assert!(hover.contains("```ovsm"));
        assert!(hover.contains("define"));
        assert!(hover.contains("Examples"));
    }
}
