(setext_heading
  (paragraph) @markup.heading.1
  (setext_h1_underline) @markup.heading.1)

(setext_heading
  (paragraph) @markup.heading.2
  (setext_h2_underline) @markup.heading.2)

(atx_heading (atx_h1_marker)) @markup.heading.1
(atx_heading (atx_h2_marker)) @markup.heading.2
(atx_heading (atx_h3_marker)) @markup.heading.3
(atx_heading (atx_h4_marker)) @markup.heading.4
(atx_heading (atx_h5_marker)) @markup.heading.5
(atx_heading (atx_h6_marker)) @markup.heading.6

(info_string) @label

(pipe_table_header (pipe_table_cell) @markup.heading)
(pipe_table_header "|" @punctuation.special)
(pipe_table_row "|" @punctuation.special)
(pipe_table_delimiter_row "|" @punctuation.special)
(pipe_table_delimiter_cell) @punctuation.special

(indented_code_block) @markup.raw.block
(fenced_code_block) @markup.raw.block

(fenced_code_block
  (fenced_code_block_delimiter) @markup.raw.block)

(fenced_code_block
  (info_string
    (language) @label))

[
  (link_destination)
] @markup.link.url

[
  (link_title)
  (link_label)
] @markup.link.label

((link_label)
  .
  ":" @punctuation.delimiter)

[
  (list_marker_plus)
  (list_marker_minus)
  (list_marker_star)
  (list_marker_dot)
  (list_marker_parenthesis)
] @markup.list

(thematic_break) @punctuation.special

(task_list_marker_unchecked) @markup.list.unchecked
(task_list_marker_checked) @markup.list.checked

(block_quote) @markup.quote

[
  (plus_metadata)
  (minus_metadata)
] @keyword.directive

[
  (block_continuation)
  (block_quote_marker)
] @punctuation.special

(backslash_escape) @string.escape

; From MDeiml/tree-sitter-markdown
; (code_span) @markup.raw

; (emphasis) @markup.italic

; (strong_emphasis) @markup.bold

; (strikethrough) @markup.strikethrough

; [
;   (backslash_escape)
;   (hard_line_break)
; ] @string.escape

; [
;   (link_label)
;   (link_text)
;   (link_title)
;   (image_description)
; ] @markup.link.label

; [
;   (link_destination)
;   (uri_autolink)
;   (email_autolink)
; ] @markup.link.url
