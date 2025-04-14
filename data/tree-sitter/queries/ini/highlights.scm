(section_name
  (text) @markup.heading)

(comment) @comment

[
  "["
  "]"
] @punctuation.bracket

"=" @operator

(setting
  (setting_name) @property)

(setting_value) @string
