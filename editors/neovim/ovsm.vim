" Vim syntax file for OVSM LISP
" Language: OVSM
" Maintainer: OpenSVM Team
" Latest Revision: 2024

if exists("b:current_syntax")
  finish
endif

" Case sensitive matching
syn case match

" Comments (LISP-style semicolon comments)
syn match ovsmComment ";.*$" contains=ovsmTodo
syn keyword ovsmTodo contained TODO FIXME XXX NOTE HACK BUG

" Strings with escape sequences
syn region ovsmString start='"' end='"' skip='\\"' contains=ovsmEscape
syn match ovsmEscape contained "\\[nrt\\\"']"

" Numbers
syn match ovsmFloat "-\?\d\+\.\d\+"
syn match ovsmInteger "-\?\d\+"

" Boolean and null constants
syn keyword ovsmConstant true false null nil

" Special forms (keywords that have special evaluation rules)
syn keyword ovsmSpecialForm if define set! let let* lambda do progn
syn keyword ovsmSpecialForm while for when unless cond case typecase
syn keyword ovsmSpecialForm defun defn defmacro const catch throw
syn keyword ovsmSpecialForm loop flet labels destructuring-bind

" Control flow builtins
syn keyword ovsmControl and or not

" Type predicates
syn keyword ovsmPredicate null? empty? evenp oddp zerop positivep negativep
syn keyword ovsmPredicate typeof type-of atom consp listp

" Collection operations
syn keyword ovsmBuiltin length get first rest last cons append reverse
syn keyword ovsmBuiltin range mapcar map filter reduce member assoc elt
syn keyword ovsmBuiltin subseq sort keys values merge

" String operations
syn keyword ovsmBuiltin concat split join replace trim upper lower

" Math functions
syn keyword ovsmBuiltin sqrt pow expt exp ln abs min max
syn keyword ovsmBuiltin sin cos tan asin acos atan
syn keyword ovsmBuiltin floor ceiling ceil round truncate trunc
syn keyword ovsmBuiltin gcd lcm mod rem

" Bitwise operations
syn keyword ovsmBuiltin logand logior logxor lognot ash

" Type conversion
syn keyword ovsmBuiltin int integer float bool string parse-int parse-float

" I/O and debugging
syn keyword ovsmBuiltin log print assert error now

" Variable manipulation
syn keyword ovsmBuiltin incf decf gensym defvar

" Keyword arguments (LISP-style :keyword)
syn match ovsmKeywordArg ":[a-zA-Z_][a-zA-Z0-9_\-]*"

" Quote and quasi-quote
syn match ovsmQuote "'" nextgroup=ovsmQuoted
syn match ovsmQuasiQuote "`" nextgroup=ovsmQuoted
syn match ovsmUnquote ",@\?" nextgroup=ovsmQuoted
syn match ovsmQuoted contained "[^ \t\n()[\]{}]*"

" Parentheses, brackets, braces
syn match ovsmParenOpen "("
syn match ovsmParenClose ")"
syn match ovsmBracketOpen "\["
syn match ovsmBracketClose "\]"
syn match ovsmBraceOpen "{"
syn match ovsmBraceClose "}"

" Operators (when used as function names)
syn match ovsmOperator "(\s*\zs[+\-*/=%<>!]=\?"
syn match ovsmOperator "(\s*\zs<="
syn match ovsmOperator "(\s*\zs>="
syn match ovsmOperator "(\s*\zs!="

" Function call (identifier after opening paren)
syn match ovsmFuncCall "(\s*\zs[a-zA-Z_][a-zA-Z0-9_\-?!*+/]*" contains=ovsmSpecialForm,ovsmBuiltin,ovsmControl,ovsmPredicate

" Regular identifiers
syn match ovsmIdentifier "[a-zA-Z_][a-zA-Z0-9_\-?!*+/]*"

" Rainbow parentheses (optional - enable with a plugin)
" These can be used with rainbow-parentheses plugins
syn cluster ovsmListCluster contains=ovsmString,ovsmComment,ovsmConstant,ovsmNumber,ovsmFloat,ovsmInteger,ovsmKeywordArg,ovsmSpecialForm,ovsmBuiltin,ovsmControl,ovsmPredicate,ovsmIdentifier,ovsmOperator

" Highlighting links
hi def link ovsmComment Comment
hi def link ovsmTodo Todo
hi def link ovsmString String
hi def link ovsmEscape SpecialChar
hi def link ovsmFloat Float
hi def link ovsmInteger Number
hi def link ovsmConstant Constant
hi def link ovsmSpecialForm Keyword
hi def link ovsmControl Conditional
hi def link ovsmPredicate Function
hi def link ovsmBuiltin Function
hi def link ovsmKeywordArg Type
hi def link ovsmQuote Special
hi def link ovsmQuasiQuote Special
hi def link ovsmUnquote Special
hi def link ovsmOperator Operator
hi def link ovsmFuncCall Function
hi def link ovsmIdentifier Identifier
hi def link ovsmParenOpen Delimiter
hi def link ovsmParenClose Delimiter
hi def link ovsmBracketOpen Delimiter
hi def link ovsmBracketClose Delimiter
hi def link ovsmBraceOpen Delimiter
hi def link ovsmBraceClose Delimiter

let b:current_syntax = "ovsm"
