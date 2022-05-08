; Special identifiers
;--------------------

([
    (identifier)
 ] @constant
 (#match? @constant "^[A-Z_][A-Z\\d_]+$"))

; Variables
;----------

(identifier) @variable

; Properties
;-----------

(property_identifier) @property

; Transforms
;-----------
(transform transformer: (identifier) @function)

; Map Filter Reduce
;------------------
(map_filter_reduce transformer: (mrf) @function)

; Literals
;---------

(this) @variable.builtin
(acc) @variable.builtin

[
  (true)
  (false)
  (null)
  (undefined)
] @constant.builtin

(comment) @comment

(string) @string

(regex) @string.special
(number) @number

; Tokens
;-------

[
  ";"
  "?."
  "."
  ","
] @punctuation.delimiter

[
  "-"
  "+"
  "*"
  "/"
   "%"
  "<"
  "<="
  "=="
  "!"
  "!="
  ">"
  ">="
  "|"
  "&&"
  "||"
  "??"
] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
]  @punctuation.bracket
