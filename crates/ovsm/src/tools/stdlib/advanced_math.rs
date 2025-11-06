//! Advanced mathematical functions - Common Lisp compatible

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

/// Register all advanced math tools
pub fn register(registry: &mut ToolRegistry) {
    // Trigonometric functions
    registry.register(SinTool);
    registry.register(CosTool);
    registry.register(TanTool);
    registry.register(AsinTool);
    registry.register(AcosTool);
    registry.register(AtanTool);
    registry.register(Atan2Tool);

    // Hyperbolic functions
    registry.register(SinhTool);
    registry.register(CoshTool);
    registry.register(TanhTool);
    registry.register(AsinhTool);
    registry.register(AcoshTool);
    registry.register(AtanhTool);

    // Exponential and logarithmic
    registry.register(ExpTool);
    registry.register(LogTool);
    registry.register(Log10Tool);
    registry.register(Log2Tool);
    registry.register(ExptTool);

    // Rounding functions
    registry.register(TruncateTool);
    registry.register(FtruncateTool);
    registry.register(FfloorTool);
    registry.register(FceilingTool);
    registry.register(FroundTool);

    // Number operations
    registry.register(ModTool);
    registry.register(RemTool);
    registry.register(GcdTool);
    registry.register(LcmTool);
    registry.register(IsqrtTool);

    // Bit operations
    registry.register(LogandTool);
    registry.register(LogiorTool);
    registry.register(LogxorTool);
    registry.register(LognotTool);
    registry.register(AshTool);
    registry.register(LshTool);

    // Additional math
    registry.register(SignumTool);
    registry.register(ConjugateTool);
    registry.register(PhaseToolMath);
    registry.register(RationalTool);
    registry.register(NumeratorTool);
    registry.register(DenominatorTool);

    // Constants
    registry.register(PiTool);
    registry.register(ETool);
}

// ============================================================================
// Trigonometric Functions
// ============================================================================

/// SIN - Sine
pub struct SinTool;

impl Tool for SinTool {
    fn name(&self) -> &str {
        "SIN"
    }

    fn description(&self) -> &str {
        "Compute sine of angle (in radians)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SIN".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.sin()))
    }
}

/// COS - Cosine
pub struct CosTool;

impl Tool for CosTool {
    fn name(&self) -> &str {
        "COS"
    }

    fn description(&self) -> &str {
        "Compute cosine of angle (in radians)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "COS".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.cos()))
    }
}

/// TAN - Tangent
pub struct TanTool;

impl Tool for TanTool {
    fn name(&self) -> &str {
        "TAN"
    }

    fn description(&self) -> &str {
        "Compute tangent of angle (in radians)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "TAN".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.tan()))
    }
}

/// ASIN - Arcsine
pub struct AsinTool;

impl Tool for AsinTool {
    fn name(&self) -> &str {
        "ASIN"
    }

    fn description(&self) -> &str {
        "Compute arcsine (inverse sine)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ASIN".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.asin()))
    }
}

/// ACOS - Arccosine
pub struct AcosTool;

impl Tool for AcosTool {
    fn name(&self) -> &str {
        "ACOS"
    }

    fn description(&self) -> &str {
        "Compute arccosine (inverse cosine)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ACOS".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.acos()))
    }
}

/// ATAN - Arctangent
pub struct AtanTool;

impl Tool for AtanTool {
    fn name(&self) -> &str {
        "ATAN"
    }

    fn description(&self) -> &str {
        "Compute arctangent (inverse tangent)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ATAN".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;

        if args.len() > 1 {
            // Two argument atan2
            let y = args[1].as_float()?;
            Ok(Value::Float(x.atan2(y)))
        } else {
            Ok(Value::Float(x.atan()))
        }
    }
}

/// ATAN2 - Two-argument arctangent
pub struct Atan2Tool;

impl Tool for Atan2Tool {
    fn name(&self) -> &str {
        "ATAN2"
    }

    fn description(&self) -> &str {
        "Compute atan2(y, x)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ATAN2".to_string(),
                reason: "Expected y and x arguments".to_string(),
            });
        }

        let y = args[0].as_float()?;
        let x = args[1].as_float()?;
        Ok(Value::Float(y.atan2(x)))
    }
}

// ============================================================================
// Hyperbolic Functions
// ============================================================================

/// SINH - Hyperbolic sine
pub struct SinhTool;

impl Tool for SinhTool {
    fn name(&self) -> &str {
        "SINH"
    }

    fn description(&self) -> &str {
        "Compute hyperbolic sine"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SINH".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.sinh()))
    }
}

/// COSH - Hyperbolic cosine
pub struct CoshTool;

impl Tool for CoshTool {
    fn name(&self) -> &str {
        "COSH"
    }

    fn description(&self) -> &str {
        "Compute hyperbolic cosine"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "COSH".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.cosh()))
    }
}

/// TANH - Hyperbolic tangent
pub struct TanhTool;

impl Tool for TanhTool {
    fn name(&self) -> &str {
        "TANH"
    }

    fn description(&self) -> &str {
        "Compute hyperbolic tangent"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "TANH".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.tanh()))
    }
}

/// ASINH - Inverse hyperbolic sine
pub struct AsinhTool;

impl Tool for AsinhTool {
    fn name(&self) -> &str {
        "ASINH"
    }

    fn description(&self) -> &str {
        "Compute inverse hyperbolic sine"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ASINH".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.asinh()))
    }
}

/// ACOSH - Inverse hyperbolic cosine
pub struct AcoshTool;

impl Tool for AcoshTool {
    fn name(&self) -> &str {
        "ACOSH"
    }

    fn description(&self) -> &str {
        "Compute inverse hyperbolic cosine"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ACOSH".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.acosh()))
    }
}

/// ATANH - Inverse hyperbolic tangent
pub struct AtanhTool;

impl Tool for AtanhTool {
    fn name(&self) -> &str {
        "ATANH"
    }

    fn description(&self) -> &str {
        "Compute inverse hyperbolic tangent"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ATANH".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.atanh()))
    }
}

// ============================================================================
// Exponential and Logarithmic
// ============================================================================

/// EXP - Exponential (e^x)
pub struct ExpTool;

impl Tool for ExpTool {
    fn name(&self) -> &str {
        "EXP"
    }

    fn description(&self) -> &str {
        "Compute e^x"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "EXP".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.exp()))
    }
}

/// LOG - Natural logarithm
pub struct LogTool;

impl Tool for LogTool {
    fn name(&self) -> &str {
        "LOG"
    }

    fn description(&self) -> &str {
        "Compute natural logarithm or log to base"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LOG".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;

        if args.len() > 1 {
            // Log to specified base
            let base = args[1].as_float()?;
            Ok(Value::Float(x.log(base)))
        } else {
            // Natural log
            Ok(Value::Float(x.ln()))
        }
    }
}

/// LOG10 - Base-10 logarithm
pub struct Log10Tool;

impl Tool for Log10Tool {
    fn name(&self) -> &str {
        "LOG10"
    }

    fn description(&self) -> &str {
        "Compute base-10 logarithm"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LOG10".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.log10()))
    }
}

/// LOG2 - Base-2 logarithm
pub struct Log2Tool;

impl Tool for Log2Tool {
    fn name(&self) -> &str {
        "LOG2"
    }

    fn description(&self) -> &str {
        "Compute base-2 logarithm"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LOG2".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.log2()))
    }
}

/// EXPT - Raise to power
pub struct ExptTool;

impl Tool for ExptTool {
    fn name(&self) -> &str {
        "EXPT"
    }

    fn description(&self) -> &str {
        "Raise base to power"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "EXPT".to_string(),
                reason: "Expected base and exponent arguments".to_string(),
            });
        }

        let base = args[0].as_float()?;
        let exp = args[1].as_float()?;
        Ok(Value::Float(base.powf(exp)))
    }
}

// ============================================================================
// Rounding Functions
// ============================================================================

/// TRUNCATE - Truncate toward zero, return quotient and remainder
pub struct TruncateTool;

impl Tool for TruncateTool {
    fn name(&self) -> &str {
        "TRUNCATE"
    }

    fn description(&self) -> &str {
        "Truncate toward zero"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "TRUNCATE".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Int(x.trunc() as i64))
    }
}

/// FTRUNCATE - Float truncate
pub struct FtruncateTool;

impl Tool for FtruncateTool {
    fn name(&self) -> &str {
        "FTRUNCATE"
    }

    fn description(&self) -> &str {
        "Truncate toward zero, return float"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FTRUNCATE".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.trunc()))
    }
}

/// FFLOOR - Float floor
pub struct FfloorTool;

impl Tool for FfloorTool {
    fn name(&self) -> &str {
        "FFLOOR"
    }

    fn description(&self) -> &str {
        "Floor function, return float"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FFLOOR".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.floor()))
    }
}

/// FCEILING - Float ceiling
pub struct FceilingTool;

impl Tool for FceilingTool {
    fn name(&self) -> &str {
        "FCEILING"
    }

    fn description(&self) -> &str {
        "Ceiling function, return float"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FCEILING".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.ceil()))
    }
}

/// FROUND - Float round
pub struct FroundTool;

impl Tool for FroundTool {
    fn name(&self) -> &str {
        "FROUND"
    }

    fn description(&self) -> &str {
        "Round to nearest integer, return float"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FROUND".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        Ok(Value::Float(x.round()))
    }
}

// ============================================================================
// Number Operations
// ============================================================================

/// MOD - Modulus
pub struct ModTool;

impl Tool for ModTool {
    fn name(&self) -> &str {
        "MOD"
    }

    fn description(&self) -> &str {
        "Compute modulus (floor-based)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MOD".to_string(),
                reason: "Expected dividend and divisor".to_string(),
            });
        }

        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    return Err(Error::ToolExecutionError {
                        tool: "MOD".to_string(),
                        reason: "Division by zero".to_string(),
                    });
                }
                Ok(Value::Int(a.rem_euclid(*b)))
            }
            _ => {
                let a = args[0].as_float()?;
                let b = args[1].as_float()?;
                Ok(Value::Float(a.rem_euclid(b)))
            }
        }
    }
}

/// REM - Remainder
pub struct RemTool;

impl Tool for RemTool {
    fn name(&self) -> &str {
        "REM"
    }

    fn description(&self) -> &str {
        "Compute remainder (truncate-based)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REM".to_string(),
                reason: "Expected dividend and divisor".to_string(),
            });
        }

        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    return Err(Error::ToolExecutionError {
                        tool: "REM".to_string(),
                        reason: "Division by zero".to_string(),
                    });
                }
                Ok(Value::Int(a % b))
            }
            _ => {
                let a = args[0].as_float()?;
                let b = args[1].as_float()?;
                Ok(Value::Float(a % b))
            }
        }
    }
}

/// GCD - Greatest common divisor
pub struct GcdTool;

impl Tool for GcdTool {
    fn name(&self) -> &str {
        "GCD"
    }

    fn description(&self) -> &str {
        "Compute greatest common divisor"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }

        fn gcd(a: i64, b: i64) -> i64 {
            if b == 0 {
                a.abs()
            } else {
                gcd(b, a % b)
            }
        }

        let mut result = args[0].as_int()?;
        for arg in &args[1..] {
            result = gcd(result, arg.as_int()?);
        }

        Ok(Value::Int(result))
    }
}

/// LCM - Least common multiple
pub struct LcmTool;

impl Tool for LcmTool {
    fn name(&self) -> &str {
        "LCM"
    }

    fn description(&self) -> &str {
        "Compute least common multiple"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(1));
        }

        fn gcd(a: i64, b: i64) -> i64 {
            if b == 0 {
                a.abs()
            } else {
                gcd(b, a % b)
            }
        }

        fn lcm(a: i64, b: i64) -> i64 {
            if a == 0 || b == 0 {
                0
            } else {
                (a.abs() / gcd(a, b)) * b.abs()
            }
        }

        let mut result = args[0].as_int()?;
        for arg in &args[1..] {
            result = lcm(result, arg.as_int()?);
        }

        Ok(Value::Int(result))
    }
}

/// ISQRT - Integer square root
pub struct IsqrtTool;

impl Tool for IsqrtTool {
    fn name(&self) -> &str {
        "ISQRT"
    }

    fn description(&self) -> &str {
        "Compute integer square root (floor of sqrt)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ISQRT".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let n = args[0].as_int()?;
        if n < 0 {
            return Err(Error::InvalidArguments {
                tool: "ISQRT".to_string(),
                reason: "Cannot compute square root of negative number".to_string(),
            });
        }

        Ok(Value::Int((n as f64).sqrt() as i64))
    }
}

// ============================================================================
// Bit Operations
// ============================================================================

/// LOGAND - Bitwise AND
pub struct LogandTool;

impl Tool for LogandTool {
    fn name(&self) -> &str {
        "LOGAND"
    }

    fn description(&self) -> &str {
        "Bitwise AND"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(-1)); // All bits set
        }

        let mut result = args[0].as_int()?;
        for arg in &args[1..] {
            result &= arg.as_int()?;
        }

        Ok(Value::Int(result))
    }
}

/// LOGIOR - Bitwise OR
pub struct LogiorTool;

impl Tool for LogiorTool {
    fn name(&self) -> &str {
        "LOGIOR"
    }

    fn description(&self) -> &str {
        "Bitwise OR"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }

        let mut result = args[0].as_int()?;
        for arg in &args[1..] {
            result |= arg.as_int()?;
        }

        Ok(Value::Int(result))
    }
}

/// LOGXOR - Bitwise XOR
pub struct LogxorTool;

impl Tool for LogxorTool {
    fn name(&self) -> &str {
        "LOGXOR"
    }

    fn description(&self) -> &str {
        "Bitwise XOR"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }

        let mut result = args[0].as_int()?;
        for arg in &args[1..] {
            result ^= arg.as_int()?;
        }

        Ok(Value::Int(result))
    }
}

/// LOGNOT - Bitwise NOT
pub struct LognotTool;

impl Tool for LognotTool {
    fn name(&self) -> &str {
        "LOGNOT"
    }

    fn description(&self) -> &str {
        "Bitwise NOT (one's complement)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LOGNOT".to_string(),
                reason: "Expected integer argument".to_string(),
            });
        }

        let n = args[0].as_int()?;
        Ok(Value::Int(!n))
    }
}

/// ASH - Arithmetic shift
pub struct AshTool;

impl Tool for AshTool {
    fn name(&self) -> &str {
        "ASH"
    }

    fn description(&self) -> &str {
        "Arithmetic shift left (positive) or right (negative)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ASH".to_string(),
                reason: "Expected integer and shift count".to_string(),
            });
        }

        let n = args[0].as_int()?;
        let count = args[1].as_int()?;

        let result = if count >= 0 { n << count } else { n >> -count };

        Ok(Value::Int(result))
    }
}

/// LSH - Logical shift
pub struct LshTool;

impl Tool for LshTool {
    fn name(&self) -> &str {
        "LSH"
    }

    fn description(&self) -> &str {
        "Logical shift left (positive) or right (negative)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "LSH".to_string(),
                reason: "Expected integer and shift count".to_string(),
            });
        }

        let n = args[0].as_int()? as u64;
        let count = args[1].as_int()?;

        let result = if count >= 0 {
            (n << count) as i64
        } else {
            (n >> -count) as i64
        };

        Ok(Value::Int(result))
    }
}

// ============================================================================
// Additional Math
// ============================================================================

/// SIGNUM - Sign of number (-1, 0, or 1)
pub struct SignumTool;

impl Tool for SignumTool {
    fn name(&self) -> &str {
        "SIGNUM"
    }

    fn description(&self) -> &str {
        "Return sign of number (-1, 0, or 1)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SIGNUM".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        match &args[0] {
            Value::Int(n) => {
                let sign = if *n < 0 {
                    -1
                } else if *n > 0 {
                    1
                } else {
                    0
                };
                Ok(Value::Int(sign))
            }
            Value::Float(f) => {
                let sign = if *f < 0.0 {
                    -1.0
                } else if *f > 0.0 {
                    1.0
                } else {
                    0.0
                };
                Ok(Value::Float(sign))
            }
            _ => Err(Error::TypeError {
                expected: "number".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// CONJUGATE - Complex conjugate (for real numbers, returns same)
pub struct ConjugateTool;

impl Tool for ConjugateTool {
    fn name(&self) -> &str {
        "CONJUGATE"
    }

    fn description(&self) -> &str {
        "Return complex conjugate (identity for real numbers)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CONJUGATE".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        // For real numbers, conjugate is identity
        Ok(args[0].clone())
    }
}

/// PHASE - Phase angle (for real numbers, 0 or pi)
pub struct PhaseToolMath;

impl Tool for PhaseToolMath {
    fn name(&self) -> &str {
        "PHASE"
    }

    fn description(&self) -> &str {
        "Return phase angle (0 for positive, pi for negative)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PHASE".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        let x = args[0].as_float()?;
        let phase = if x < 0.0 { std::f64::consts::PI } else { 0.0 };
        Ok(Value::Float(phase))
    }
}

/// RATIONAL - Convert to rational (in OVSM, returns float)
pub struct RationalTool;

impl Tool for RationalTool {
    fn name(&self) -> &str {
        "RATIONAL"
    }

    fn description(&self) -> &str {
        "Convert to rational representation (returns float in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "RATIONAL".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        Ok(args[0].clone())
    }
}

/// NUMERATOR - Get numerator (for OVSM numbers, returns value)
pub struct NumeratorTool;

impl Tool for NumeratorTool {
    fn name(&self) -> &str {
        "NUMERATOR"
    }

    fn description(&self) -> &str {
        "Get numerator (returns value for non-rational)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NUMERATOR".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        // OVSM doesn't have rational numbers, return the value
        Ok(args[0].clone())
    }
}

/// DENOMINATOR - Get denominator (for OVSM numbers, returns 1)
pub struct DenominatorTool;

impl Tool for DenominatorTool {
    fn name(&self) -> &str {
        "DENOMINATOR"
    }

    fn description(&self) -> &str {
        "Get denominator (returns 1 for non-rational)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "DENOMINATOR".to_string(),
                reason: "Expected numeric argument".to_string(),
            });
        }

        // OVSM doesn't have rational numbers, return 1
        Ok(Value::Int(1))
    }
}

// ============================================================================
// Constants
// ============================================================================

/// PI - Mathematical constant π
pub struct PiTool;

impl Tool for PiTool {
    fn name(&self) -> &str {
        "PI"
    }

    fn description(&self) -> &str {
        "Mathematical constant π (pi)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Float(std::f64::consts::PI))
    }
}

/// E - Mathematical constant e (Euler's number)
pub struct ETool;

impl Tool for ETool {
    fn name(&self) -> &str {
        "E"
    }

    fn description(&self) -> &str {
        "Mathematical constant e (Euler's number)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Float(std::f64::consts::E))
    }
}
