(shebang) @keyword.directive

(identifier) @variable

; Assume all-caps names are constants
((identifier) @constant
  (#match? @constant "^[A-Z][A-Z%d_]*$"))

(const_item
  name: (identifier) @constant)
(type_identifier) @type
(primitive_type) @type.builtin
(field_identifier) @variable.member
(shorthand_field_identifier) @variable.member
(shorthand_field_initializer
  (identifier) @variable.member)
(mod_item
  name: (identifier) @module)
(self) @keyword.builtin

(function_item (identifier) @function)
(function_signature_item (identifier) @function)

"$" @function.macro

(metavariable) @function.macro

(macro_definition
  "macro_rules!" @function.macro)

; Attribute macros
(attribute_item
  (attribute
    (identifier) @function.macro))

(inner_attribute_item
  (attribute
    (identifier) @function.macro))

(attribute
  (scoped_identifier
    (identifier) @function.macro .))

; Function-like macros
(macro_invocation
  macro: (identifier) @function.macro)

(macro_invocation
  macro: (scoped_identifier
    (identifier) @function.macro .))

(call_expression
  function: (identifier) @function.call)
(call_expression
  function: (scoped_identifier
    (identifier) @function.call .))
(call_expression
  function: (field_expression
    field: (field_identifier) @function.call))
(generic_function
  function: (identifier) @function.call)
(generic_function
  function: (scoped_identifier
    name: (identifier) @function.call))
(generic_function
  function: (field_expression
    field: (field_identifier) @function.call))

[
  (line_comment)
  (block_comment)
  (outer_doc_comment_marker)
  (inner_doc_comment_marker)
] @comment

(line_comment
  (doc_comment)) @comment.documentation

(block_comment
  (doc_comment)) @comment.documentation


(boolean_literal) @boolean
(integer_literal) @number
(float_literal) @number.float

(char_literal) @character

(string_literal) @string
(raw_string_literal) @string

(use_wildcard
  "*" @character.special)

(remaining_field_pattern
  ".." @character.special)

"_" @character.special

; Keywords
[
  "use"
  "mod"
] @keyword.import

(use_as_clause
  "as" @keyword.import)

"fn" @keyword.function

[
  "return"
  "yield"
] @keyword.return

(type_cast_expression
  "as" @keyword.operator)

(qualified_type
  "as" @keyword.operator)

[
  "default"
  "impl"
  "let"
  "move"
  "unsafe"
  "where"
] @keyword

[
  "enum"
  "struct"
  "union"
  "trait"
  "type"
] @keyword.type

[
  "async"
  "await"
] @keyword.coroutine

"try" @keyword.exception

[
  "ref"
  "pub"
  (mutable_specifier)
  "const"
  "static"
  "dyn"
  "extern"
] @keyword.modifier

[
  "if"
  "else"
  "match"
] @keyword.conditional

[
  "break"
  "continue"
  "in"
  "loop"
  "while"
] @keyword.repeat

"for" @keyword

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

(closure_parameters
  "|" @punctuation.bracket)

[
  ","
  "."
  ":"
  "::"
  ";"
  "->"
  "=>"
] @punctuation.delimiter


; Operators
[
  "!"
  "!="
  "%"
  "%="
  "&"
  "&&"
  "&="
  "*"
  "*="
  "+"
  "+="
  "-"
  "-="
  ".."
  "..="
  "..."
  "/"
  "/="
  "<"
  "<<"
  "<<="
  "<="
  "="
  "=="
  ">"
  ">="
  ">>"
  ">>="
  "?"
  "@"
  "^"
  "^="
  "|"
  "|="
  "||"
] @operator

(type_arguments
  [
    "<"
    ">"
  ] @punctuation.bracket)

(type_parameters
  [
    "<"
    ">"
  ] @punctuation.bracket)

(bracketed_type
  [
    "<"
    ">"
  ] @punctuation.bracket)

(for_lifetimes
  [
    "<"
    ">"
  ] @punctuation.bracket)


(attribute_item
  "#" @punctuation.special)

(inner_attribute_item
  [
    "!"
    "#"
  ] @punctuation.special)

(enum_variant
  name: (identifier) @constant)

; Assume that uppercase names in paths are types
(scoped_identifier
  path: (identifier) @module)

(scoped_identifier
  (scoped_identifier
    name: (identifier) @module))

(scoped_type_identifier
  path: (identifier) @module)

(scoped_type_identifier
  path: (identifier) @type
  (#match? @type "^[A-Z]"))

(scoped_type_identifier
  (scoped_identifier
    name: (identifier) @module))

((scoped_identifier
  path: (identifier) @type)
  (#match? @type "^[A-Z]"))

((scoped_identifier
  name: (identifier) @type)
  (#match? @type "^[A-Z]"))

((scoped_identifier
  name: (identifier) @constant)
  (#match? @constant "^[A-Z][A-Z%d_]*$"))

((scoped_identifier
  path: (identifier) @type
  name: (identifier) @constant)
  (#match? @type "^[A-Z]")
  (#match? @constant "^[A-Z]"))

((scoped_type_identifier
  path: (identifier) @type
  name: (type_identifier) @constant)
  (#match? @type "^[A-Z]")
  (#match? @constant "^[A-Z]"))

((identifier) @constant.builtin
  (#any-of? @constant.builtin "Some" "None" "Ok" "Err"))

(macro_invocation
  macro: (identifier) @function.macro
  "!" @function.macro)
