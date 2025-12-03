/**
 * @file PlumbingRules grammar for tree-sitter
 * @author Innes Anderson-Morrison
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'plumbing_rules',

  extras: $ => [
    /\s|\\\r?\n/,
    $.comment,
  ],

  rules: {
    source_file: $ => repeat(choice(
      $.variable_declaration,
      $.rule_block,
      $._blank_line
    )),

    comment: $ => seq('#', /.*\n/),
    _blank_line: $ => '\n',

    variable_declaration: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $.variable_value),
      "\n",
    ),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    variable_value: $ => /[^\n]+/,

    rule_block: $ => seq(
      repeat1(seq($.pattern, '\n')),
      repeat1(seq($.action, '\n'))
    ),

    pattern: $ => choice(
      $.data_matches,
      $.data_narrows,
      $.data_set,
      $.data_from,
      $.field_is,
      $.field_set,
      $.field_matches,
      $.arg_isfile,
      $.arg_isdir,
      $.attr_add,
      $.attr_delete
    ),

    data_matches: $ => seq('data', 'matches', ' ', field('regex', $.regex)),
    data_narrows: $ => seq('data', 'narrows', ' ', field('regex', $.regex)),
    data_set: $ => seq('data', 'set', ' ', field('value', $.value)),
    data_from: $ => seq('data', 'from', ' ', field('command', $.command)),

    field_is: $ => seq($.field, 'is', ' ', field('value', $.value)),
    field_set: $ => seq($.match_field, 'set', ' ', field('value', $.value)),
    field_matches: $ => seq($.match_field, 'matches', ' ', field('regex', $.regex)),

    arg_isfile: $ => seq('arg', 'isfile', ' ', field('path', $.value)),
    arg_isdir: $ => seq('arg', 'isdir', ' ', field('path', $.value)),

    attr_add: $ => seq('attr', 'add', ' ', field('attrs', $.attr_list)),
    attr_delete: $ => seq('attr', 'delete', ' ', field('attr', $.identifier)),

    field: $ => choice('src', 'dst', 'wdir', 'data'),
    match_field: $ => choice('src', 'dst', 'wdir'),

    action: $ => choice(
      $.plumb_to,
      $.plumb_start
    ),

    plumb_to: $ => seq('plumb', 'to', field('port', $.identifier)),
    plumb_start: $ => seq('plumb', 'start', field('command', $.command)),

    variable_reference: $ => /\$[a-zA-Z0-9_]+/,
    value_content: $ => /[^\n $]+/,

    value: $ => repeat1(choice(
      $.variable_reference,
      $.value_content
    )),

    command: $ => repeat1(choice(
      $.variable_reference,
      $.value_content
    )),

    regex: $ => /[^\n]+/,
    attr_list: $ => choice(
      $.attr_pair,
      seq(repeat1(seq($.attr_pair, ' ')), $.attr_pair),
    ),

    attr_pair: $ => seq(
      field('attr', $.identifier),
      '=',
      field('attr_value', $.value)
    ),
  }
});
