/**
 * Tree-sitter grammar for OVSM LISP
 *
 * OVSM is a Common Lisp dialect for blockchain automation.
 * This grammar provides accurate parsing for syntax highlighting,
 * code folding, and structural navigation.
 */

module.exports = grammar({
  name: 'ovsm',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  rules: {
    // Entry point - a program is a sequence of expressions
    source_file: $ => repeat($._expression),

    // Comments start with semicolon
    comment: $ => /;[^\n]*/,

    // Expressions - the core of any LISP
    _expression: $ => choice(
      $.list,
      $.array,
      $.object,
      $.string,
      $.number,
      $.boolean,
      $.null,
      $.symbol,
      $.keyword_argument,
      $.quote_expression,
      $.quasiquote_expression,
      $.unquote_expression,
      $.unquote_splicing_expression,
    ),

    // S-expression list: (...)
    list: $ => seq(
      '(',
      optional($.list_head),
      repeat($._expression),
      ')',
    ),

    // The first element of a list (function position)
    list_head: $ => choice(
      $.special_form,
      $.builtin_function,
      $.operator,
      $.symbol,
    ),

    // Special forms have unique evaluation rules
    special_form: $ => choice(
      'if',
      'define',
      'set!',
      'let',
      'let*',
      'lambda',
      'do',
      'progn',
      'while',
      'for',
      'when',
      'unless',
      'cond',
      'case',
      'typecase',
      'defun',
      'defn',
      'defmacro',
      'const',
      'catch',
      'throw',
      'loop',
      'flet',
      'labels',
      'destructuring-bind',
      'try',
    ),

    // Built-in functions
    builtin_function: $ => choice(
      // Type predicates
      'null?',
      'empty?',
      'evenp',
      'oddp',
      'zerop',
      'positivep',
      'negativep',
      'typeof',
      'type-of',
      'atom',
      'consp',
      'listp',

      // Collection operations
      'length',
      'get',
      'first',
      'rest',
      'last',
      'cons',
      'append',
      'reverse',
      'range',
      'mapcar',
      'map',
      'filter',
      'reduce',
      'member',
      'assoc',
      'elt',
      'subseq',
      'sort',
      'keys',
      'values',
      'merge',

      // String operations
      'concat',
      'split',
      'join',
      'replace',
      'trim',
      'upper',
      'lower',

      // Math functions
      'sqrt',
      'pow',
      'expt',
      'exp',
      'ln',
      'abs',
      'min',
      'max',
      'sin',
      'cos',
      'tan',
      'asin',
      'acos',
      'atan',
      'floor',
      'ceiling',
      'ceil',
      'round',
      'truncate',
      'trunc',
      'gcd',
      'lcm',
      'mod',
      'rem',

      // Bitwise operations
      'logand',
      'logior',
      'logxor',
      'lognot',
      'ash',

      // Type conversion
      'int',
      'integer',
      'float',
      'bool',
      'string',
      'parse-int',
      'parse-float',

      // I/O and debugging
      'log',
      'print',
      'assert',
      'error',
      'now',

      // Logical
      'not',
      'and',
      'or',

      // Variable manipulation
      'incf',
      'decf',
      'gensym',
      'defvar',

      // Blockchain functions (MCP tools)
      'getBalance',
      'getSignaturesForAddress',
      'getTransaction',
      'getAccountInfo',
      'getTokenAccountsByOwner',
      'get_account_transfers',
    ),

    // Operators
    operator: $ => choice(
      '+', '-', '*', '/', '%',
      '=', '==', '!=', '<', '>', '<=', '>=',
    ),

    // Array literal: [...]
    array: $ => seq(
      '[',
      repeat($._expression),
      ']',
    ),

    // Object literal: {:key value ...}
    object: $ => seq(
      '{',
      repeat($.object_pair),
      '}',
    ),

    object_pair: $ => seq(
      $.keyword_argument,
      $._expression,
    ),

    // Strings
    string: $ => seq(
      '"',
      repeat(choice(
        $.string_content,
        $.escape_sequence,
      )),
      '"',
    ),

    string_content: $ => /[^"\\]+/,

    escape_sequence: $ => /\\[nrt\\"']/,

    // Numbers
    number: $ => choice(
      $.float,
      $.integer,
    ),

    float: $ => /-?[0-9]+\.[0-9]+/,
    integer: $ => /-?[0-9]+/,

    // Booleans
    boolean: $ => choice('true', 'false'),

    // Null
    null: $ => choice('null', 'nil'),

    // Symbols (identifiers)
    symbol: $ => /[a-zA-Z_][a-zA-Z0-9_\-?!*+\/]*/,

    // Keyword arguments: :keyword
    keyword_argument: $ => /:[a-zA-Z_][a-zA-Z0-9_\-]*/,

    // Quote expressions
    quote_expression: $ => seq("'", $._expression),

    // Quasiquote
    quasiquote_expression: $ => seq('`', $._expression),

    // Unquote
    unquote_expression: $ => seq(',', $._expression),

    // Unquote-splicing
    unquote_splicing_expression: $ => seq(',@', $._expression),
  },
});
