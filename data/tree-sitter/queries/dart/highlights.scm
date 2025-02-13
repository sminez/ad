[
  "import"
  "library"
  "export"
  "as"
  "show"
  "hide"
] @keyword.import

[
  (case_builtin)
  "late"
  "required"
  "on"
  "extends"
  "in"
  "is"
  "new"
  "super"
  "with"
] @keyword

[
  "class"
  "enum"
  "extension"
] @keyword.type

"return" @keyword.return

[
  "deferred"
  "factory"
  "get"
  "implements"
  "interface"
  "library"
  "operator"
  "mixin"
  "part"
  "set"
  "typedef"
] @keyword

[
  "async"
  "async*"
  "sync*"
  "await"
  "yield"
] @keyword.coroutine

[
  (const_builtin)
  (final_builtin)
  "abstract"
  "covariant"
  "external"
  "static"
  "final"
  "base"
  "sealed"
] @keyword.modifier

[
  "if"
  "else"
  "switch"
  "default"
] @keyword.conditional

(conditional_expression
  [
    "?"
    ":"
  ] @keyword.conditional.ternary)

[
  "try"
  "throw"
  "catch"
  "finally"
  (break_statement)
] @keyword.exception

[
  "do"
  "while"
  "continue"
  "for"
] @keyword.repeat

(class_definition
  name: (identifier) @type)

(constructor_signature
  name: (identifier) @type)

(scoped_identifier
  scope: (identifier) @type)

(function_signature
  name: (identifier) @function.method)

(getter_signature
  (identifier) @function.method)

(setter_signature
  name: (identifier) @function.method)

(enum_declaration
  name: (identifier) @type)

(enum_constant
  name: (identifier) @type)

(void_type) @type

(type_identifier) @type

(type_alias
  (type_identifier) @type.definition)

(type_arguments
  [
    "<"
    ">"
  ] @punctuation.bracket)

(inferred_type) @keyword


(identifier) @variable
(this) @variable.builtin

(dotted_identifier_list) @string
(string_literal) @string

[
  (hex_integer_literal)
  (decimal_integer_literal)
  (decimal_floating_point_literal)
] @number

(symbol_literal) @string.special.symbol
(string_literal) @string

(true) @boolean
(false) @boolean

(null_literal) @constant.builtin

(comment) @comment
(documentation_comment) @comment.documentation

(function_expression_body
  (identifier) @function.call)

[
  "=>"
  ".."
  "??"
  "=="
  "!"
  "?"
  "&&"
  "%"
  "<"
  ">"
  "="
  ">="
  "<="
  "||"
  ">>>="
  ">>="
  "<<="
  "&="
  "|="
  "??="
  "%="
  "+="
  "-="
  "*="
  "/="
  "^="
  "~/="
  (shift_operator)
  (multiplicative_operator)
  (increment_operator)
  (is_operator)
  (prefix_operator)
  (equality_operator)
  (additive_operator)
] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ";"
  "."
  ","
  ":"
  "?."
  "?"
] @punctuation.delimiter
