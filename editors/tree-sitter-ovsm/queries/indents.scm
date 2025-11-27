; Tree-sitter indent queries for OVSM LISP
; Defines automatic indentation behavior

; Lists increase indent for their children
(list) @indent

; Arrays increase indent for their children
(array) @indent

; Objects increase indent for their children
(object) @indent

; Closing brackets dedent
")" @dedent
"]" @dedent
"}" @dedent
