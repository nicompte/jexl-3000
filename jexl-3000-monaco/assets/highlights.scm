; =============================================================================
; highlights.scm — jexl3000 syntax highlighting
;
; ORDERING: tree-sitter highlights use LAST-MATCH-WINS when multiple patterns
; capture the same node.  Generic fallbacks (identifier, punctuation, etc.)
; must come FIRST; more-specific contextual patterns come AFTER so they
; override the fallback.
;
; DO NOT add captures for purely structural nodes such as `program`,
; `statement`, or `expression_statement`.  These span entire expressions and
; capturing them causes tree-sitter's highlight engine to assign them a scope
; before it can reach the leaf nodes inside, effectively shadowing every child.
; =============================================================================

; ── Generic identifier fallback ───────────────────────────────────────────────
; This MUST be first so that every more-specific rule below overrides it.
(identifier) @variable

; ── Comments ──────────────────────────────────────────────────────────────────
(comment) @comment

; ── Built-in constants ────────────────────────────────────────────────────────
(true)  @constant.builtin
(false) @constant.builtin
(null)     @constant.builtin
(now)      @constant.builtin
(now_utc)  @constant.builtin

; ── Special keywords ──────────────────────────────────────────────────────────
(this) @keyword.special

; ── Built-in accumulator variable ─────────────────────────────────────────────
(acc) @variable.builtin
(index) @variable.builtin

; ── Strings and escape sequences ──────────────────────────────────────────────
(string)          @string
(escape_sequence) @string.escape

; ── Numbers ───────────────────────────────────────────────────────────────────
(number) @number

; ── Regular expressions ───────────────────────────────────────────────────────
(regex)         @string.regexp
(regex_pattern) @string.regexp
(regex_flags)   @string.special

; ── MRF built-ins (map / filter / reduce / sortBy / apply / ...) ──────────────
(mrf) @function.builtin

; ── Custom transforms (value | myTransformer) ─────────────────────────────────
; Comes AFTER `(identifier) @variable` so it wins for transform identifiers.
(transform
  transformer: (identifier) @function)

; ── Member-expression punctuation and properties ─────────────────────────────
(member_expression "."  @punctuation.delimiter)
(member_expression "?." @punctuation.delimiter)

(member_expression
  property: (property_identifier) @property)

(member_expression
  property: (private_property_identifier) @property)

; ── Shorthand property identifiers  { foo } ───────────────────────────────────
(shorthand_property_identifier) @property

; ── Object key identifiers (pair key) ─────────────────────────────────────────
(pair
  key: (property_identifier) @property)

; ── MRF punctuation ───────────────────────────────────────────────────────────
(map_filter_reduce "(" @punctuation.bracket)
(map_filter_reduce ")" @punctuation.bracket)
(map_filter_reduce "," @punctuation.delimiter)
(map_filter_reduce "|" @operator)

; ── Transform pipe ────────────────────────────────────────────────────────────
(transform "|" @operator)

; ── Brackets ──────────────────────────────────────────────────────────────────
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

; ── Other delimiters ──────────────────────────────────────────────────────────
[
  ","
  ":"
] @punctuation.delimiter

; ── Pipe operator ─────────────────────────────────────────────────────────────
"|" @operator

; ── Binary operators ──────────────────────────────────────────────────────────
(binary_expression operator: "&&"  @operator)
(binary_expression operator: "||"  @operator)
(binary_expression operator: "+"   @operator)
(binary_expression operator: "-"   @operator)
(binary_expression operator: "*"   @operator)
(binary_expression operator: "/"   @operator)
(binary_expression operator: "//"  @operator)
(binary_expression operator: "%"   @operator)
(binary_expression operator: "^"   @operator)
(binary_expression operator: "~"   @operator)
(binary_expression operator: "@+"  @operator)
(binary_expression operator: "@"   @operator)
(binary_expression operator: "<"   @operator)
(binary_expression operator: "<="  @operator)
(binary_expression operator: ">"   @operator)
(binary_expression operator: ">="  @operator)
(binary_expression operator: "in"  @operator)
(binary_expression operator: "=="  @operator)
(binary_expression operator: "!="  @operator)
(binary_expression operator: "??"  @operator)

; ── Unary operators ───────────────────────────────────────────────────────────
(unary_expression operator: "!" @operator)
(unary_expression operator: "-" @operator)
(unary_expression operator: "+" @operator)

; ── Ternary expression ────────────────────────────────────────────────────────
(ternary_expression "?" @operator)
(ternary_expression ":" @punctuation.delimiter)

; ── meta_property — new.target ────────────────────────────────────────────────
(meta_property "new"    @keyword)
(meta_property "."      @punctuation.delimiter)
(meta_property "target" @keyword)
