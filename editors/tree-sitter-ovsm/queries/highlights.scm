; Tree-sitter highlight queries for OVSM LISP
; This file maps AST nodes to highlight groups

; Comments
(comment) @comment

; Strings
(string) @string
(escape_sequence) @string.escape

; Numbers
(integer) @number
(float) @number.float

; Booleans
(boolean) @constant.builtin

; Null
(null) @constant.builtin

; Special forms (keywords with special evaluation)
(special_form) @keyword

; Built-in functions
(builtin_function) @function.builtin

; Operators
(operator) @operator

; Keyword arguments
(keyword_argument) @variable.parameter

; Symbols (identifiers)
(symbol) @variable

; Symbols in function position (list head)
(list
  (list_head
    (symbol) @function.call))

; Punctuation
"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

; Quote markers
"'" @punctuation.special
"`" @punctuation.special
"," @punctuation.special
",@" @punctuation.special

; Special patterns for define/defun
(list
  (list_head
    (special_form) @keyword)
  (symbol) @function.definition)

; Let bindings
(list
  (list_head
    (special_form) @keyword (#match? @keyword "^let"))
  (array
    (array
      (symbol) @variable.definition)))

; Lambda parameters
(list
  (list_head
    (special_form) @keyword (#eq? @keyword "lambda"))
  (array
    (symbol) @variable.parameter))

; For loop variable
(list
  (list_head
    (special_form) @keyword (#eq? @keyword "for"))
  (array
    (symbol) @variable.definition))
