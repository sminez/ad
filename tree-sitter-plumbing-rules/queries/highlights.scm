(comment) @comment

(variable_declaration
  name: (identifier) @constant
  value: (variable_value) @string)

["data" "src" "dst" "wdir" "arg" "attr" "plumb"] @keyword
["matches" "narrows" "set" "from" "is" "isfile" "isdir" "add" "delete" "to" "start"] @function
["="] @punctuation

(regex) @string.regex
(value_content) @character
(variable_reference) @string.template

(plumb_to port: (identifier) @number)
(attr_pair attr: (identifier) @boolean)
