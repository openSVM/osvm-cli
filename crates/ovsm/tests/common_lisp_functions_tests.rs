/// Integration tests for Common Lisp compatibility functions
/// Tests all 27 CL functions added to OVSM LISP interpreter
use ovsm::lexer::SExprScanner;
use ovsm::parser::SExprParser;
use ovsm::runtime::{LispEvaluator, Value};

/// Helper function to execute OVSM code
fn eval(source: &str) -> Value {
    let mut scanner = SExprScanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = SExprParser::new(tokens);
    let program = parser.parse().unwrap();
    let mut evaluator = LispEvaluator::new();
    evaluator.execute(&program).unwrap()
}

/// Helper to check if two floats are approximately equal
fn approx_eq(a: f64, b: f64) -> bool {
    (a - b).abs() < 0.0001
}

// ============================================================================
// Batch 1: Arithmetic Operations (6 functions)
// ============================================================================

#[test]
fn test_cl_arithmetic_shortcuts() {
    // Test mod (Euclidean modulo)
    let result = eval("(mod 17 5)");
    assert_eq!(result, Value::Int(2));

    let result = eval("(mod -17 5)");
    assert_eq!(result, Value::Int(3)); // Euclidean: always positive

    // Test rem (remainder)
    let result = eval("(rem 17 5)");
    assert_eq!(result, Value::Int(2));

    let result = eval("(rem -17 5)");
    assert_eq!(result, Value::Int(-2)); // Takes sign of dividend
}

#[test]
fn test_cl_gcd() {
    // Test gcd - Greatest common divisor
    let result = eval("(gcd 12 18)");
    assert_eq!(result, Value::Int(6));

    let result = eval("(gcd 48 18 30)");
    assert_eq!(result, Value::Int(6));

    // Test gcd with no args (identity)
    let result = eval("(gcd)");
    assert_eq!(result, Value::Int(0));

    // Test gcd with one arg
    let result = eval("(gcd 42)");
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_cl_lcm() {
    // Test lcm - Least common multiple
    let result = eval("(lcm 4 6)");
    assert_eq!(result, Value::Int(12));

    let result = eval("(lcm 3 4 6)");
    assert_eq!(result, Value::Int(12));

    // Test lcm with no args (identity)
    let result = eval("(lcm)");
    assert_eq!(result, Value::Int(1));

    // Test lcm with one arg
    let result = eval("(lcm 7)");
    assert_eq!(result, Value::Int(7));
}

// ============================================================================
// Batch 2: List Predicates (3 functions)
// ============================================================================

#[test]
fn test_cl_atom() {
    // atom should return true for non-lists
    let result = eval("(atom 42)");
    assert_eq!(result, Value::Bool(true));

    let result = eval(r#"(atom "hello")"#);
    assert_eq!(result, Value::Bool(true));

    let result = eval("(atom null)");
    assert_eq!(result, Value::Bool(true));

    // atom should return false for lists
    let result = eval("(atom [1 2 3])");
    assert_eq!(result, Value::Bool(false));

    let result = eval("(atom [])");
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_cl_consp() {
    // consp should return true for non-empty lists
    let result = eval("(consp [1 2 3])");
    assert_eq!(result, Value::Bool(true));

    // consp should return false for empty lists
    let result = eval("(consp [])");
    assert_eq!(result, Value::Bool(false));

    // consp should return false for non-lists
    let result = eval("(consp 42)");
    assert_eq!(result, Value::Bool(false));

    let result = eval("(consp null)");
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_cl_listp() {
    // listp should return true for any list (empty or not)
    let result = eval("(listp [1 2 3])");
    assert_eq!(result, Value::Bool(true));

    let result = eval("(listp [])");
    assert_eq!(result, Value::Bool(true));

    let result = eval("(listp null)");
    assert_eq!(result, Value::Bool(true));

    // listp should return false for non-lists
    let result = eval("(listp 42)");
    assert_eq!(result, Value::Bool(false));

    let result = eval(r#"(listp "hello")"#);
    assert_eq!(result, Value::Bool(false));
}

// ============================================================================
// Batch 3: Bitwise Operations (5 functions)
// ============================================================================

#[test]
fn test_cl_logand() {
    // Bitwise AND
    let result = eval("(logand 12 10)");
    assert_eq!(result, Value::Int(8)); // 1100 & 1010 = 1000

    let result = eval("(logand 15 7 3)");
    assert_eq!(result, Value::Int(3)); // 1111 & 0111 & 0011 = 0011

    // Identity for AND is -1 (all bits set)
    let result = eval("(logand)");
    assert_eq!(result, Value::Int(-1));
}

#[test]
fn test_cl_logior() {
    // Bitwise OR
    let result = eval("(logior 12 10)");
    assert_eq!(result, Value::Int(14)); // 1100 | 1010 = 1110

    let result = eval("(logior 1 2 4)");
    assert_eq!(result, Value::Int(7)); // 001 | 010 | 100 = 111

    // Identity for OR is 0
    let result = eval("(logior)");
    assert_eq!(result, Value::Int(0));
}

#[test]
fn test_cl_logxor() {
    // Bitwise XOR
    let result = eval("(logxor 12 10)");
    assert_eq!(result, Value::Int(6)); // 1100 ^ 1010 = 0110

    let result = eval("(logxor 15 7)");
    assert_eq!(result, Value::Int(8)); // 1111 ^ 0111 = 1000

    // Identity for XOR is 0
    let result = eval("(logxor)");
    assert_eq!(result, Value::Int(0));
}

#[test]
fn test_cl_lognot() {
    // Bitwise NOT
    let result = eval("(lognot 0)");
    assert_eq!(result, Value::Int(-1));

    let result = eval("(lognot -1)");
    assert_eq!(result, Value::Int(0));

    let result = eval("(lognot 10)");
    assert_eq!(result, Value::Int(-11));
}

#[test]
fn test_cl_ash() {
    // Arithmetic shift
    let result = eval("(ash 8 2)");
    assert_eq!(result, Value::Int(32)); // 8 << 2 = 32

    let result = eval("(ash 32 -2)");
    assert_eq!(result, Value::Int(8)); // 32 >> 2 = 8

    let result = eval("(ash 1 4)");
    assert_eq!(result, Value::Int(16)); // 1 << 4 = 16

    let result = eval("(ash 256 -4)");
    assert_eq!(result, Value::Int(16)); // 256 >> 4 = 16
}

// ============================================================================
// Batch 4: List Operations (4 functions)
// ============================================================================

#[test]
fn test_cl_member() {
    // member returns tail of list starting at found element
    let result = eval("(member 3 [1 2 3 4 5])");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Int(3));
            assert_eq!(arr[1], Value::Int(4));
            assert_eq!(arr[2], Value::Int(5));
        }
        _ => panic!("Expected array"),
    }

    // member returns null if not found
    let result = eval("(member 7 [1 2 3 4 5])");
    assert_eq!(result, Value::Null);

    // member works with strings
    let result = eval(r#"(member "b" ["a" "b" "c"])"#);
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 2);
        }
        _ => panic!("Expected array"),
    }
}

#[test]
fn test_cl_assoc() {
    // assoc looks up key in association list
    let result = eval(
        r#"
        (do
          (define alist [["a" 1] ["b" 2] ["c" 3]])
          (assoc "b" alist))
    "#,
    );

    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 2);
            assert_eq!(arr[0], Value::String("b".to_string()));
            assert_eq!(arr[1], Value::Int(2));
        }
        _ => panic!("Expected array pair"),
    }

    // assoc returns null if key not found
    let result = eval(
        r#"
        (do
          (define alist [["a" 1] ["b" 2]])
          (assoc "z" alist))
    "#,
    );
    assert_eq!(result, Value::Null);
}

#[test]
fn test_cl_elt() {
    // elt works on arrays
    let result = eval("(elt [10 20 30 40] 0)");
    assert_eq!(result, Value::Int(10));

    let result = eval("(elt [10 20 30 40] 2)");
    assert_eq!(result, Value::Int(30));

    // elt works on strings
    let result = eval(r#"(elt "hello" 1)"#);
    assert_eq!(result, Value::String("e".to_string()));

    let result = eval(r#"(elt "world" 4)"#);
    assert_eq!(result, Value::String("d".to_string()));
}

#[test]
fn test_cl_subseq() {
    // subseq on arrays with start and end
    let result = eval("(subseq [1 2 3 4 5] 1 3)");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 2);
            assert_eq!(arr[0], Value::Int(2));
            assert_eq!(arr[1], Value::Int(3));
        }
        _ => panic!("Expected array"),
    }

    // subseq on arrays with just start (to end)
    let result = eval("(subseq [1 2 3 4 5] 2)");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Int(3));
        }
        _ => panic!("Expected array"),
    }

    // subseq on strings
    let result = eval(r#"(subseq "hello" 1 4)"#);
    assert_eq!(result, Value::String("ell".to_string()));

    let result = eval(r#"(subseq "world" 2)"#);
    assert_eq!(result, Value::String("rld".to_string()));
}

// ============================================================================
// Batch 5: String Comparisons (3 functions + aliases)
// ============================================================================

#[test]
fn test_cl_string_comparisons() {
    // string-equal (using alternative name due to lexer)
    let result = eval(r#"(string-equal "hello" "hello")"#);
    assert_eq!(result, Value::Bool(true));

    let result = eval(r#"(string-equal "hello" "world")"#);
    assert_eq!(result, Value::Bool(false));

    // string-lessp
    let result = eval(r#"(string-lessp "apple" "banana")"#);
    assert_eq!(result, Value::Bool(true));

    let result = eval(r#"(string-lessp "zebra" "apple")"#);
    assert_eq!(result, Value::Bool(false));

    let result = eval(r#"(string-lessp "abc" "abc")"#);
    assert_eq!(result, Value::Bool(false));

    // string-greaterp
    let result = eval(r#"(string-greaterp "zebra" "apple")"#);
    assert_eq!(result, Value::Bool(true));

    let result = eval(r#"(string-greaterp "apple" "banana")"#);
    assert_eq!(result, Value::Bool(false));

    let result = eval(r#"(string-greaterp "xyz" "xyz")"#);
    assert_eq!(result, Value::Bool(false));
}

// ============================================================================
// Batch 6: Map Variants (2 functions)
// ============================================================================

#[test]
fn test_cl_mapcar() {
    // mapcar applies function and returns new list
    let result = eval("(mapcar (lambda (x) (* x x)) [1 2 3 4 5])");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 5);
            assert_eq!(arr[0], Value::Int(1));
            assert_eq!(arr[1], Value::Int(4));
            assert_eq!(arr[2], Value::Int(9));
            assert_eq!(arr[3], Value::Int(16));
            assert_eq!(arr[4], Value::Int(25));
        }
        _ => panic!("Expected array"),
    }

    // mapcar with addition
    let result = eval("(mapcar (lambda (x) (+ x 10)) [1 2 3])");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr[0], Value::Int(11));
            assert_eq!(arr[1], Value::Int(12));
            assert_eq!(arr[2], Value::Int(13));
        }
        _ => panic!("Expected array"),
    }
}

#[test]
fn test_cl_mapc() {
    // mapc executes for side effects but returns original list
    let result = eval(
        r#"
        (do
          (define items [1 2 3])
          (mapc (lambda (x) (* x 100)) items))
    "#,
    );

    // Should return the original list
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Int(1));
            assert_eq!(arr[1], Value::Int(2));
            assert_eq!(arr[2], Value::Int(3));
        }
        _ => panic!("Expected original array"),
    }
}

// ============================================================================
// Batch 7: Conditional Filters (2 functions)
// ============================================================================

#[test]
fn test_cl_remove_if() {
    // remove-if removes elements matching predicate
    let result = eval("(remove-if (lambda (x) (< x 5)) [1 3 5 7 9 2 4 6 8 10])");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 6);
            assert_eq!(arr[0], Value::Int(5));
            assert_eq!(arr[1], Value::Int(7));
            assert_eq!(arr[2], Value::Int(9));
            assert_eq!(arr[3], Value::Int(6));
            assert_eq!(arr[4], Value::Int(8));
            assert_eq!(arr[5], Value::Int(10));
        }
        _ => panic!("Expected array"),
    }

    // remove-if with modulo predicate
    let result = eval("(remove-if (lambda (x) (== (% x 2) 0)) [1 2 3 4 5 6])");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Int(1));
            assert_eq!(arr[1], Value::Int(3));
            assert_eq!(arr[2], Value::Int(5));
        }
        _ => panic!("Expected array"),
    }
}

#[test]
fn test_cl_remove_if_not() {
    // remove-if-not keeps elements matching predicate (inverse of remove-if)
    let result = eval("(remove-if-not (lambda (x) (> x 5)) [1 3 5 7 9 2 4 6 8 10])");
    match result {
        Value::Array(arr) => {
            assert_eq!(arr.len(), 5);
            assert_eq!(arr[0], Value::Int(7));
            assert_eq!(arr[1], Value::Int(9));
            assert_eq!(arr[2], Value::Int(6));
            assert_eq!(arr[3], Value::Int(8));
            assert_eq!(arr[4], Value::Int(10));
        }
        _ => panic!("Expected array"),
    }
}

// ============================================================================
// Batch 8: Variable Mutation (2 functions)
// ============================================================================

#[test]
fn test_cl_incf() {
    // incf with default delta (1)
    let result = eval(
        r#"
        (do
          (define x 10)
          (incf x)
          x)
    "#,
    );
    assert_eq!(result, Value::Int(11));

    // incf with custom delta
    let result = eval(
        r#"
        (do
          (define y 100)
          (incf y 25)
          y)
    "#,
    );
    assert_eq!(result, Value::Int(125));

    // incf with float
    let result = eval(
        r#"
        (do
          (define z 5.5)
          (incf z 2.5)
          z)
    "#,
    );
    match result {
        Value::Float(f) => assert!(approx_eq(f, 8.0)),
        _ => panic!("Expected float"),
    }

    // incf returns the new value
    let result = eval(
        r#"
        (do
          (define a 5)
          (incf a 3))
    "#,
    );
    assert_eq!(result, Value::Int(8));
}

#[test]
fn test_cl_decf() {
    // decf with default delta (1)
    let result = eval(
        r#"
        (do
          (define x 10)
          (decf x)
          x)
    "#,
    );
    assert_eq!(result, Value::Int(9));

    // decf with custom delta
    let result = eval(
        r#"
        (do
          (define y 100)
          (decf y 30)
          y)
    "#,
    );
    assert_eq!(result, Value::Int(70));

    // decf with float
    let result = eval(
        r#"
        (do
          (define z 10.0)
          (decf z 2.5)
          z)
    "#,
    );
    match result {
        Value::Float(f) => assert!(approx_eq(f, 7.5)),
        _ => panic!("Expected float"),
    }
}

// ============================================================================
// Edge Cases and Type Coercion
// ============================================================================

#[test]
fn test_cl_type_coercion() {
    // incf with mixed Int/Float
    let result = eval(
        r#"
        (do
          (define x 10)
          (incf x 2.5))
    "#,
    );
    match result {
        Value::Float(f) => assert!(approx_eq(f, 12.5)),
        _ => panic!("Expected float from Int + Float coercion"),
    }

    // decf with mixed Float/Int
    let result = eval(
        r#"
        (do
          (define y 10.5)
          (decf y 3))
    "#,
    );
    match result {
        Value::Float(f) => assert!(approx_eq(f, 7.5)),
        _ => panic!("Expected float from Float - Int coercion"),
    }
}

#[test]
fn test_cl_edge_cases() {
    // Empty list operations
    let result = eval("(member 1 [])");
    assert_eq!(result, Value::Null);

    let result = eval("(assoc \"key\" [])");
    assert_eq!(result, Value::Null);

    // Variadic with single arg
    let result = eval("(logand 15)");
    assert_eq!(result, Value::Int(15));

    let result = eval("(logior 7)");
    assert_eq!(result, Value::Int(7));

    // String operations edge cases
    let result = eval(r#"(subseq "a" 0 1)"#);
    assert_eq!(result, Value::String("a".to_string()));

    let result = eval(r#"(elt "x" 0)"#);
    assert_eq!(result, Value::String("x".to_string()));
}

// ============================================================================
// Comprehensive Integration Test
// ============================================================================

#[test]
fn test_cl_comprehensive_usage() {
    // Complex example using multiple CL functions together
    let result = eval(
        r#"
        (do
          ;; Create data
          (define numbers [1 2 3 4 5 6 7 8 9 10])

          ;; Filter evens using remove-if-not
          (define evens (remove-if-not (lambda (x) (== (% x 2) 0)) numbers))

          ;; Square them using mapcar
          (define squared (mapcar (lambda (x) (* x x)) evens))

          ;; Get first element
          (define first (elt squared 0))

          ;; Increment it
          (define counter first)
          (incf counter 10)

          counter)
    "#,
    );

    // First even is 2, squared is 4, plus 10 is 14
    assert_eq!(result, Value::Int(14));
}
