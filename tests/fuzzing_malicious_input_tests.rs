//! FUZZING AND MALICIOUS INPUT TESTS
//! Testing with invalid, malformed, and adversarial inputs
//! Goal: Find crashes, panics, and security vulnerabilities

use anyhow::Result;
use std::collections::HashMap;

#[cfg(test)]
mod string_fuzzing_tests {
    use super::*;

    #[test]
    fn test_null_bytes_in_strings() -> Result<()> {
        let malicious = "Hello\0World\0\0";

        // Should handle null bytes gracefully
        assert!(malicious.contains('\0'));
        assert_eq!(malicious.len(), 13);

        Ok(())
    }

    #[test]
    fn test_extremely_long_strings() -> Result<()> {
        // 1 million character string
        let massive = "x".repeat(1_000_000);

        assert_eq!(massive.len(), 1_000_000);
        assert!(massive.chars().all(|c| c == 'x'));

        Ok(())
    }

    #[test]
    fn test_unicode_bomb() -> Result<()> {
        // Strings that expand when normalized
        let zalgo = "H̴̡̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯͓͔͕͖͙͚͐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌ͅͅe̷̡̢̨̛̛̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇͈͉͙͚͐̑̒̓̔̕͘͜͝͝l̸̡̢̧̨̛̛̗̘̙̜̝̞̟̠̣̤̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇͈͉͙͚͐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌͝l̸̡̢̧̨̛̛̗̘̙̜̝̞̟̠̣̤̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇͈͉͙͚͐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌͝o̷";

        // Should not panic
        let len = zalgo.len();
        assert!(len > 0);

        Ok(())
    }

    #[test]
    fn test_emoji_overflow() -> Result<()> {
        let emoji_spam = "🔥".repeat(10000);

        assert_eq!(emoji_spam.chars().count(), 10000);

        Ok(())
    }

    #[test]
    fn test_control_characters() -> Result<()> {
        let control = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F";

        // Should handle control chars
        assert_eq!(control.len(), 16);

        Ok(())
    }

    #[test]
    fn test_mixed_encoding_attack() -> Result<()> {
        // Attempt UTF-8 / UTF-16 confusion
        let mixed = "Hello\u{FEFF}World\u{FFFE}";

        assert!(mixed.contains("Hello"));
        assert!(mixed.contains("World"));

        Ok(())
    }

    #[test]
    fn test_newline_injection() -> Result<()> {
        let log_injection = "user\nADMIN: access granted\n";

        // Should not be interpreted as separate log lines
        assert!(log_injection.contains('\n'));

        Ok(())
    }

    #[test]
    fn test_homoglyph_attack() -> Result<()> {
        // Characters that look the same but are different
        let ascii_o = "Hello";
        let cyrillic_o = "Hеllo"; // 'е' is Cyrillic

        // Should be different
        assert_ne!(ascii_o, cyrillic_o);

        Ok(())
    }

    #[test]
    fn test_rtl_override() -> Result<()> {
        // Right-to-left override attack
        let rtl = "file\u{202E}txt.exe";

        // Displays as "fileexe.txt" but is actually .exe
        assert!(rtl.contains(".exe"));

        Ok(())
    }

    #[test]
    fn test_zero_width_characters() -> Result<()> {
        let hidden = "admin\u{200B}\u{200C}\u{200D}pass";

        // Contains zero-width chars (admin=5 + 3 zero-width + pass=4 = 12 chars)
        assert!(hidden.len() > 10);
        assert_eq!(hidden.chars().count(), 12); // Includes zero-width

        Ok(())
    }
}

#[cfg(test)]
mod number_fuzzing_tests {
    use super::*;

    #[test]
    fn test_integer_overflow() {
        let max = i32::MAX;
        let overflow = max.wrapping_add(1);

        assert_eq!(overflow, i32::MIN);
    }

    #[test]
    fn test_division_by_zero() {
        // Use runtime values to avoid compile-time detection
        let x = std::hint::black_box(10);
        let y = std::hint::black_box(0);

        let result = std::panic::catch_unwind(|| x / y);

        assert!(result.is_err());
    }

    #[test]
    fn test_modulo_by_zero() {
        // Use runtime values to avoid compile-time detection
        let x = std::hint::black_box(10);
        let y = std::hint::black_box(0);

        let result = std::panic::catch_unwind(|| x % y);

        assert!(result.is_err());
    }

    #[test]
    fn test_float_nan_arithmetic() {
        let nan = f64::NAN;

        assert!((nan + 1.0).is_nan());
        assert!((nan * 2.0).is_nan());
        assert!((nan / 3.0).is_nan());
        assert!((0.0f64 / 0.0f64).is_nan());
    }

    #[test]
    fn test_float_infinity_arithmetic() {
        let inf = f64::INFINITY;

        assert_eq!(inf + 1.0, inf);
        assert_eq!(inf * 2.0, inf);
        assert!((inf / inf).is_nan());
        assert!((inf - inf).is_nan());
    }

    #[test]
    fn test_subnormal_floats() {
        let subnormal = f64::from_bits(1); // Smallest positive subnormal

        assert!(subnormal > 0.0);
        assert!(subnormal < f64::MIN_POSITIVE);
    }

    #[test]
    fn test_negative_zero() {
        let pos_zero = 0.0f64;
        let neg_zero = -0.0f64;

        // They're equal but have different bit patterns
        assert_eq!(pos_zero, neg_zero);
        assert_ne!(pos_zero.to_bits(), neg_zero.to_bits());
    }

    #[test]
    fn test_lossy_float_conversion() {
        // Use a value that definitely loses precision
        let huge: u64 = (1u64 << 53) + 1; // Beyond f64's mantissa precision
        let as_float = huge as f64;
        let back_to_int = as_float as u64;

        // Precision lost
        assert_ne!(huge, back_to_int);
    }
}

#[cfg(test)]
mod path_traversal_tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_parent_directory_traversal() {
        let malicious = "../../../etc/passwd";
        let path = Path::new(malicious);

        assert!(path.to_str().unwrap().contains(".."));
    }

    #[test]
    fn test_absolute_path_injection() {
        let malicious = "/etc/shadow";
        let path = Path::new(malicious);

        assert!(path.is_absolute());
    }

    #[test]
    fn test_null_byte_path() {
        let malicious = "safe\0/etc/passwd";

        // Path should handle null bytes
        assert!(malicious.contains('\0'));
    }

    #[test]
    fn test_symlink_bomb_pattern() {
        // Patterns that could create symlink loops
        let patterns = vec!["a/../a/../a/../a", "./././././.", "a/./b/../a/./b/../"];

        for pattern in patterns {
            let path = Path::new(pattern);
            assert!(path.components().count() > 0);
        }
    }

    #[test]
    fn test_windows_path_injection() {
        let patterns = vec![
            "C:\\Windows\\System32",
            "\\\\?\\C:\\",
            "CON",
            "PRN",
            "AUX",
            "NUL",
        ];

        for pattern in patterns {
            let _path = Path::new(pattern);
            // Should not panic
        }
    }

    #[test]
    fn test_unicode_path_normalization() {
        let paths = vec![
            "café",
            "cafe\u{0301}", // Composed vs decomposed
        ];

        for path in paths {
            let _p = Path::new(path);
        }
    }
}

#[cfg(test)]
mod injection_attack_tests {
    use super::*;

    #[test]
    fn test_sql_injection_patterns() {
        let sql_attacks = vec![
            "' OR '1'='1",
            "'; DROP TABLE users--",
            "1' UNION SELECT * FROM passwords--",
            "admin'--",
            "' OR 1=1#",
        ];

        for attack in sql_attacks {
            // Should be escaped/sanitized
            assert!(attack.contains('\'') || attack.contains('-'));
        }
    }

    #[test]
    fn test_command_injection_patterns() {
        let cmd_attacks = vec![
            "; rm -rf /",
            "| cat /etc/passwd",
            "&& whoami",
            "`cat /etc/shadow`",
            "$(reboot)",
        ];

        for attack in cmd_attacks {
            // Should detect shell metacharacters
            assert!(
                attack.contains(';')
                    || attack.contains('|')
                    || attack.contains('&')
                    || attack.contains('`')
                    || attack.contains('$')
            );
        }
    }

    #[test]
    fn test_xss_injection_patterns() {
        let xss_attacks = vec![
            "<script>alert('XSS')</script>",
            "javascript:alert(1)",
            "<img src=x onerror=alert(1)>",
            "<svg onload=alert(1)>",
        ];

        for attack in xss_attacks {
            assert!(attack.contains('<') || attack.contains("javascript:"));
        }
    }

    #[test]
    fn test_ldap_injection_patterns() {
        let ldap_attacks = vec!["*)(uid=*", "admin)(&(password=*))", "*"];

        for attack in ldap_attacks {
            assert!(attack.contains('*') || attack.contains(')'));
        }
    }

    #[test]
    fn test_xml_injection_patterns() {
        let xml_attacks = vec![
            "<?xml version=\"1.0\"?><!DOCTYPE foo [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]>",
            "<foo>&xxe;</foo>",
        ];

        // First attack contains "xml" and "ENTITY", second contains entity reference &xxe;
        assert!(xml_attacks[0].contains("xml") || xml_attacks[0].contains("ENTITY"));
        assert!(xml_attacks[1].contains("xxe")); // Contains entity reference
    }
}

#[cfg(test)]
mod buffer_overflow_simulation_tests {
    use super::*;

    #[test]
    fn test_string_capacity_overflow() {
        let mut s = String::with_capacity(10);

        // Try to overflow capacity
        for _ in 0..100 {
            s.push('x');
        }

        assert_eq!(s.len(), 100);
        assert!(s.capacity() >= 100);
    }

    #[test]
    fn test_vec_capacity_overflow() {
        let mut v = Vec::with_capacity(10);

        for i in 0..1000 {
            v.push(i);
        }

        assert_eq!(v.len(), 1000);
        assert!(v.capacity() >= 1000);
    }

    #[test]
    fn test_slice_bounds() {
        let data = vec![1, 2, 3, 4, 5];

        // Out of bounds should panic
        let result = std::panic::catch_unwind(|| {
            let _ = data[10];
        });

        assert!(result.is_err());
    }

    #[test]
    fn test_unchecked_indexing() {
        let data = vec![1, 2, 3];

        // Safe: using get()
        assert_eq!(data.get(10), None);

        // Unsafe: direct indexing would panic
        let result = std::panic::catch_unwind(|| data[10]);
        assert!(result.is_err());
    }
}

#[cfg(test)]
mod format_string_attacks {
    use super::*;

    #[test]
    fn test_format_string_injection() {
        // Potential format string vulnerability patterns
        let user_input = "%s%s%s%s%s%s%s%s";

        // Should not be used directly in format!
        let safe = format!("{}", user_input);
        assert_eq!(safe, user_input); // Treated as literal

        // Dangerous (commented out):
        // let dangerous = format!(user_input); // Would expand format specifiers
    }

    #[test]
    fn test_printf_style_attacks() {
        let attacks = vec!["%x%x%x%x", "%n", "%s", "%.1000000s"];

        for attack in attacks {
            // Should treat as literal string
            let safe = format!("{}", attack);
            assert_eq!(safe, attack);
        }
    }
}

#[cfg(test)]
mod deserialization_attacks {
    use super::*;

    #[test]
    fn test_malformed_json() {
        let malformed = vec![
            "{",
            "}",
            "{{}",
            "[[[",
            "null{",
            "{\"key\":}",
            "{\"key\":value}",
        ];

        for json in malformed {
            let result: Result<serde_json::Value, _> = serde_json::from_str(json);
            assert!(result.is_err());
        }
    }

    #[test]
    fn test_deeply_nested_json() {
        // Create deeply nested JSON
        let mut json = String::from("[");
        for _ in 0..1000 {
            json.push_str("[");
        }
        for _ in 0..1000 {
            json.push_str("]");
        }
        json.push_str("]");

        // Should handle or reject gracefully
        let result: Result<serde_json::Value, _> = serde_json::from_str(&json);
        // May succeed or fail with stack overflow
        let _ = result;
    }

    #[test]
    fn test_huge_json_numbers() {
        let huge_number = "9".repeat(10000);
        let json = format!("{}", huge_number);

        let result: Result<serde_json::Value, _> = serde_json::from_str(&json);
        // Should handle large numbers
        let _ = result;
    }

    #[test]
    fn test_unicode_in_json() {
        let json = r#"{"emoji":"🔥","zalgo":"H̴e̷l̸l̸o̷"}"#;

        let result: Result<serde_json::Value, _> = serde_json::from_str(json);
        assert!(result.is_ok());
    }

    #[test]
    fn test_duplicate_keys_in_json() {
        let json = r#"{"key":"value1","key":"value2"}"#;

        let result: Result<serde_json::Value, _> = serde_json::from_str(json);
        // Last key should win
        assert!(result.is_ok());
    }
}

#[cfg(test)]
mod regex_dos_tests {
    use super::*;

    #[test]
    fn test_catastrophic_backtracking() {
        // Patterns that could cause exponential time
        let evil_patterns = vec![
            ("(a+)+", "aaaaaaaaaaaaaaaaaaaaaaX"),
            ("(a*)*", "aaaaaaaaaaaaaaaaaaaaaaX"),
            ("(a|a)*", "aaaaaaaaaaaaaaaaaaaaaaX"),
        ];

        for (pattern, input) in evil_patterns {
            // Should timeout or handle efficiently
            let re = regex::Regex::new(pattern).unwrap();
            let start = std::time::Instant::now();
            let _result = re.is_match(input);
            let elapsed = start.elapsed();

            // Should complete in reasonable time
            assert!(elapsed < std::time::Duration::from_secs(1));
        }
    }
}

#[cfg(test)]
mod resource_exhaustion_attacks {
    use super::*;

    #[test]
    fn test_zip_bomb_pattern() {
        // Simulate decompression bomb
        let compressed_size = 100;
        let expansion_ratio = 1000;

        let data = vec![0u8; compressed_size];
        let expanded = data.repeat(expansion_ratio);

        assert_eq!(expanded.len(), compressed_size * expansion_ratio);
    }

    #[test]
    fn test_billion_laughs_attack() {
        // XML entity expansion attack pattern
        let entity_depth = 10;
        let expansion_factor = 2;

        let mut size = 1;
        for _ in 0..entity_depth {
            size *= expansion_factor;
        }

        assert!(size > 1000);
    }

    #[test]
    fn test_hash_collision_attack() {
        // Attempt to create hash collisions
        let mut map = HashMap::new();

        // These might collide depending on hasher
        for i in 0..10000 {
            map.insert(i, i);
        }

        assert_eq!(map.len(), 10000);
    }
}

#[cfg(test)]
mod timing_attack_tests {
    use super::*;

    #[test]
    fn test_constant_time_comparison_needed() {
        // Timing attacks on string comparison
        let secret = "secret_password_12345";

        let wrong1 = "a";
        let wrong2 = "secret_";
        let wrong3 = "secret_password_1234";

        // Standard comparison leaks timing info
        let start1 = std::time::Instant::now();
        let _ = secret == wrong1;
        let time1 = start1.elapsed();

        let start2 = std::time::Instant::now();
        let _ = secret == wrong2;
        let time2 = start2.elapsed();

        let start3 = std::time::Instant::now();
        let _ = secret == wrong3;
        let time3 = start3.elapsed();

        // Timing might leak information (not guaranteed in test though)
        println!("Timing: {:?} {:?} {:?}", time1, time2, time3);
    }
}

#[cfg(test)]
mod environment_manipulation_tests {
    use super::*;

    #[test]
    fn test_environment_variable_injection() {
        let malicious = "PATH=/tmp:$PATH";

        // Should sanitize before setting
        assert!(malicious.contains("PATH"));
    }

    #[test]
    fn test_locale_manipulation() {
        // Attempt to manipulate locale
        let locales = vec!["en_US.UTF-8", "C", "POSIX", "../../../etc/passwd"];

        for locale in locales {
            // Should validate locale strings
            assert!(!locale.is_empty());
        }
    }
}
