// Implemented based on
// https://github.com/sixtyfpsui/sixtyfps/
//   docs/langref.md
//   sixtyfps_compiler/{lexer.rs,parser.rs,parser/*.rs}
//                     object_tree.rs
// 75ba29cf2fc593e824718bc2b21036b4ee2430f4

module.exports = grammar({
  name: 'sixtyfps',

  extras: $ => [
    /\p{space}/,
    $.line_comment,
    $.block_comment,
  ],

  supertypes: $ => [
    $._expression,
  ],

  inline: $ => [
    $._component,
    $._element_content_member,
    $._literal,
  ],

  word: $ => $.identifier,

  rules: {
    document: $ => repeat(choice(
      $._component,
      // TODO: *ExportsList
      // TODO: *ImportSpecifier
      // TODO: *StructDeclaration
    )),

    _component: $ => choice(
      $.component,
      $.global_component,
    ),

    component: $ => seq(
      field('id', $.identifier),
      ':=',
      field('root_element', $.element),
    ),

    global_component: $ => seq(
      'global',
      field('id', $.identifier),
      ':=',
      field('root_element', $.element_content),
    ),

    sub_element: $ => seq(
      optional(seq(
        field('id', $.identifier),
        ':=',
      )),
      $.element,
    ),

    element: $ => seq(
      field('base_type', alias($.qualified_name, $.qualified_type_name)),
      $.element_content,
    ),

    element_content: $ => seq(
      '{',
      repeat($._element_content_member),
      '}',
    ),

    _element_content_member: $ => choice(
      // TODO: *PropertyDeclaration
      $.binding,
      // TODO: *CallbackConnection
      // TODO: *CallbackDeclaration
      $.sub_element,
      // TODO: *RepeatedElement
      // TODO: *PropertyAnimation
      // TODO: *TwoWayBinding
      // TODO: *States
      // TODO: *Transitions
      // TODO: ?ChildrenPlaceholder
    ),

    binding: $ => seq(
      field('name', $.identifier),
      ':',
      field('expr', $.binding_expression),
    ),

    binding_expression: $ => choice(
      seq(
        $.code_block,
        optional(';'),
      ),
      seq(
        $._expression,
        ';',
      ),
    ),

    _expression: $ => choice(
      $._literal,
      $.parenthesized_expression,
      // TODO: ?FunctionCallExpression
      // TODO: ?SelfAssignment
      // TODO: ?ConditionalExpression
      $.qualified_name,
      // TODO: ?BinaryExpression
      // TODO: ?Array
      // TODO: ?ObjectLiteral
      // TODO: ?UnaryOpExpression
      // TODO: ?CodeBlock
      // TODO: ?StringTemplate
      // TODO: ?AtImageUrl
      // TODO: ?AtLinearGradient
    ),

    parenthesized_expression: $ => seq(
      '(',
      $._expression,
      ')',
    ),

    code_block: $ => seq(
      '{',
      // TODO: *Expression
      // TODO: *ReturnStatement
      '}',
    ),

    line_comment: $ => token(seq(
      '//',
      /.*/,
    )),

    // TODO: nested comment
    // https://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    block_comment: $ => token(seq(
      '/*',
      /[^*]*\*+([^/*][^*]*\*+)*/,
      '/'
    )),

    _literal: $ => choice(
      $.string_literal,
      $.number_literal,
      $.color_literal,
    ),

    string_literal: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        $.string_fragment,
      )),
      token.immediate('"'),
    ),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      /./,  // TODO: '{' to start template string
    )),

    string_fragment: $ => token.immediate(/[^"\\]+/),

    number_literal: $ => seq(
      token(seq(
        /[0-9]+/,
        optional(/\.[0-9]*/),
      )),
      optional($.unit),
    ),

    unit: $ => choice(
      token.immediate('%'),
      token.immediate(/[a-zA-Z]+/),
    ),

    color_literal: $ => /#[a-zA-Z0-9]*/,

    // TODO: lex_identifier() accepts c.is_alphanumeric(), which may contain
    // non-ASCII Alpha/Nd/Nl/No character.
    identifier: $ => token(seq(
      /[a-zA-Z_]/,
      repeat(/[a-zA-Z0-9_\-]/),
    )),

    qualified_name: $ => sep1($.identifier, '.'),
  },
});

function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
