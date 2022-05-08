module.exports = grammar({
  name: 'jexl3000',

  externals: $ => [
    $._ternary_qmark,
  ],

  extras: $ => [
    $.comment,
    /[\s\p{Zs}\uFEFF\u2060\u200B]/,
  ],

  supertypes: $ => [
    $.statement,
    $.expression,
    $.primary_expression,
    $.pattern,
  ],

  inline: $ => [
    $._expressions,
    $._identifier,
    $._reserved_identifier,
    $._lhs_expression,
  ],

  precedences: $ => [
    [
      $.expression_statement,
      'member',
      'unary_void',
      'binary_exp',
      'map_filter_reduce',
      'transform',
      'binary_times',
      'binary_plus',
      'binary_compare',
      'binary_relation',
      'binary_equality',
      'binary_regex_test',
      'binary_regex_match',
      'logical_and',
      'logical_or',
      'ternary',
      $.sequence_expression
    ],
    ['assign', $.primary_expression],
    ['member', 'new', $.expression],
    ['literal'],
    [$.primary_expression, 'object'],
    /*[$.import_statement, $.import],
    [$.export_statement, $.primary_expression],*/
  ],

  conflicts: $ => [
    [$.primary_expression, $._property_name],
    [$.primary_expression, $._property_name],
    [$.primary_expression],
    /*[$.primary_expression, $.method_definition],*/
    /*[$.primary_expression, $.rest_pattern],
    [$.primary_expression, $.pattern],
    [$.primary_expression, $._for_header],*/
    [$.array, $.array_pattern],
    [$.object, $.object_pattern],
    /*[$.pattern],
    [ $.object_assignment_pattern],
    [$.labeled_statement, $._property_name],
    [$.computed_property_name, $.array],*/
    [$.binary_expression, $._initializer],
    [$.transform, $.binary_expression],
  ],

  word: $ => $.identifier,

  rules: {
    program: $ => seq(
      repeat($.statement)
    ),

    _mrf: $ => $.mrf,

    //
    // Statements
    //

    /*transform: $ => prec.right(seq(
      $._expressions,
      "|",
      $.identifier,
      optional(seq("(", $._expressions, ")")),
    )),*/
    transform: $ => prec.left('object', seq(
      field('subject', $._expressions),
      '|',
      field('transformer', $.identifier),
      optional(field('arguments', $.arguments))
    )),

    map_filter_reduce: $ => prec.left('object', seq(
      field('subject', $._expressions),
      '|',
      field('transformer', $._mrf),
      '(',
      field('expression', $.expression),
      ')'
    )),

    statement: $ => choice(
      $.expression_statement,
    ),

    expression_statement: $ => seq(
      $._expressions
    ),

    parenthesized_expression: $ => seq(
      '(',
      $._expressions,
      ')'
    ),

    //
    // Expressions
    //
    _expressions: $ => choice(
      $.expression,
      /*$.sequence_expression*/
    ),

    expression: $ => choice(
      $.primary_expression,
      $.unary_expression,
      $.binary_expression,
      $.transform,
      $.map_filter_reduce,
      $.ternary_expression,
    ),

    primary_expression: $ => choice(
      $.member_expression,
      $.parenthesized_expression,
      $._identifier,
      alias($._reserved_identifier, $.identifier),
      $.this,
      $.acc,
      $.number,
      $.string,
      $.regex,
      $.true,
      $.false,
      $.null,
      $.object,
      $.array,
      $.meta_property,
    ),

    object: $ => prec('object', seq(
      '{',
      commaSep(optional(choice(
        $.pair,
        alias(
          choice($.identifier, $._reserved_identifier),
          $.shorthand_property_identifier
        )
      ))),
      '}'
    )),

    object_pattern: $ => prec('object', seq(
      '{',
      commaSep(optional(choice(
        $.pair_pattern,
        alias(
          choice($.identifier, $._reserved_identifier),
          $.shorthand_property_identifier_pattern
        )
      ))),
      '}'
    )),

    array: $ => seq(
      '[',
      commaSep(optional(
        $.expression,
      )),
      ']'
    ),

    array_pattern: $ => seq(
      '[',
      commaSep(optional(
        $.pattern,
      )),
      ']'
    ),

    member_expression: $ => prec('member', seq(
      field('object', choice($.expression, $.primary_expression)),
      choice('.', '?.'),
      field('property', choice(
        $.private_property_identifier,
        alias($.identifier, $.property_identifier)))
    )),

    _lhs_expression: $ => choice(
      $.member_expression,
      $._identifier,
      alias($._reserved_identifier, $.identifier),
    ),

    _initializer: $ => seq(
      '=',
      field('value', $.expression)
    ),

    ternary_expression: $ => prec.right('ternary', seq(
      field('condition', $.expression),
      alias($._ternary_qmark, '?'),
      field('consequence', $.expression),
      ':',
      field('alternative', $.expression)
    )),

    binary_expression: $ => choice(
      ...[
        ['&&', 'logical_and'],
        ['||', 'logical_or'],
        ['+', 'binary_plus'],
        ['-', 'binary_plus'],
        ['*', 'binary_times'],
        ['/', 'binary_times'],
        ['%', 'binary_times'],
        ['~', 'binary_regex_test'],
        ['@', 'binary_regex_match'],
        ['<', 'binary_relation'],
        ['<=', 'binary_relation'],
        ['==', 'binary_equality'],
        ['!=', 'binary_equality'],
        ['>=', 'binary_relation'],
        ['>', 'binary_relation'],
        ['??', 'ternary'],
        ['in', 'binary_relation'],
      ].map(([operator, precedence]) =>
        prec.left(precedence, seq(
          field('left', $.expression),
          field('operator', operator),
          field('right', $.expression)
        ))
      )
    ),

    unary_expression: $ => prec.left('unary_void', seq(
      field('operator', choice('!', '-', '+')),
      field('argument', $.expression)
    )),

    sequence_expression: $ => seq(
      field('left', $.expression),
      ',',
      field('right', choice($.sequence_expression, $.expression))
    ),

    //
    // Primitives
    //

    // Here we tolerate unescaped newlines in double-quoted and
    // single-quoted string literals.
    // This is legal in typescript as jsx/tsx attribute values (as of
    // 2020), and perhaps will be valid in javascript as well in the
    // future.
    //
    string: $ => choice(
      seq(
        '"',
        repeat(choice(
          alias($.unescaped_double_string_fragment, $.string_fragment),
          $.escape_sequence
        )),
        '"'
      ),
      seq(
        "'",
        repeat(choice(
          alias($.unescaped_single_string_fragment, $.string_fragment),
          $.escape_sequence
        )),
        "'"
      )
    ),

    // Workaround to https://github.com/tree-sitter/tree-sitter/issues/1156
    // We give names to the token() constructs containing a regexp
    // so as to obtain a node in the CST.
    //
    unescaped_double_string_fragment: $ =>
      token.immediate(prec(1, /[^"\\]+/)),

    // same here
    unescaped_single_string_fragment: $ =>
      token.immediate(prec(1, /[^'\\]+/)),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[^xu0-7]/,
        /[0-7]{1,3}/,
        /x[0-9a-fA-F]{2}/,
        /u[0-9a-fA-F]{4}/,
        /u{[0-9a-fA-F]+}/
      )
    )),

    comment: $ => token(choice(
      seq('#', /.*/),
      //seq(
      //  '/*',
      //  /[^*]*\*+([^/*][^*]*\*+)*/,
      //  '/'
      //)
    )),

    regex: $ => seq(
      '/',
      field('pattern', $.regex_pattern),
      token.immediate('/'),
      optional(field('flags', $.regex_flags))
    ),

    regex_pattern: $ => token.immediate(prec(-1,
      repeat1(choice(
        seq(
          '[',
          repeat(choice(
            seq('\\', /./), // escaped character
            /[^\]\n\\]/       // any character besides ']' or '\n'
          )),
          ']'
        ),              // square-bracket-delimited character class
        seq('\\', /./), // escaped character
        /[^/\\\[\n]/    // any character besides '[', '\', '/', '\n'
      ))
    )),

    regex_flags: $ => token.immediate(/[a-z]+/),

    number: $ => {
      const hex_literal = seq(
        choice('0x', '0X'),
        /[\da-fA-F](_?[\da-fA-F])*/
      )

      const decimal_digits = /\d(_?\d)*/
      const signed_integer = seq(optional(choice('-', '+')), decimal_digits)
      const exponent_part = seq(choice('e', 'E'), signed_integer)

      const binary_literal = seq(choice('0b', '0B'), /[0-1](_?[0-1])*/)

      const octal_literal = seq(choice('0o', '0O'), /[0-7](_?[0-7])*/)

      const bigint_literal = seq(choice(hex_literal, binary_literal, octal_literal, decimal_digits), 'n')

      const decimal_integer_literal = choice(
        '0',
        seq(optional('0'), /[1-9]/, optional(seq(optional('_'), decimal_digits)))
      )

      const decimal_literal = choice(
        seq(decimal_integer_literal, '.', optional(decimal_digits), optional(exponent_part)),
        seq('.', decimal_digits, optional(exponent_part)),
        seq(decimal_integer_literal, exponent_part),
        seq(decimal_digits),
      )

      return token(choice(
        hex_literal,
        decimal_literal,
        binary_literal,
        octal_literal,
        bigint_literal,
      ))
    },

    // 'undefined' is syntactically a regular identifier in JavaScript.
    // However, its main use is as the read-only global variable whose
    // value is [undefined], for which there's no literal representation
    // unlike 'null'. We gave it its own rule so it's easy to
    // highlight in text editors and other applications.
    _identifier: $ => $.identifier,

    identifier: $ => {
      const alpha = /[^\x00-\x1F\s\p{Zs}0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      const alphanumeric = /[^\x00-\x1F\s\p{Zs}:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      return token(seq(alpha, repeat(alphanumeric)))
    },

    mrf: $ => choice(
      'map', 'filter', 'reduce', 'sortBy', 'apply', 'find', 'findIndex', 'any', 'all'
    ),

    private_property_identifier: $ => {
      const alpha = /[^\x00-\x1F\s\p{Zs}0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      const alphanumeric = /[^\x00-\x1F\s\p{Zs}:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      return token(seq('#', alpha, repeat(alphanumeric)))
    },

    meta_property: $ => seq('new', '.', 'target'),

    this: $ => 'this',
    acc: $ => 'acc',
    true: $ => 'true',
    false: $ => 'false',
    null: $ => 'null',
    undefined: $ => 'undefined',

    //
    // Expression components
    //

    arguments: $ => seq(
      '(',
      commaSep(optional($.expression)),
      ')'
    ),

    // This negative dynamic precedence ensures that during error recovery,
    // unfinished constructs are generally treated as literal expressions,
    // not patterns.
    pattern: $ => prec.dynamic(-1, choice(
      $._lhs_expression,
      $.rest_pattern
    )),

    rest_pattern: $ => prec.right(seq(
      '...',
      $._lhs_expression
    )),

    pair: $ => seq(
      field('key', $._property_name),
      ':',
      field('value', $.expression)
    ),

    pair_pattern: $ => seq(
      field('key', $._property_name),
      ':',
      field('value', $.pattern)
    ),

    _property_name: $ => choice(
      alias(choice(
        $.identifier,
        $._reserved_identifier
      ), $.property_identifier),
      $.string,
      $.number,
    ),

    _reserved_identifier: $ => choice(
      'get',
      'set',
      'async',
      'static',
      'export'
    ),

    _semicolon: $ => choice(';')
  }
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}
