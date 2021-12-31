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

  inline: $ => [
    $._component,
    $._element_content_member,
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
      field('base_type', $.qualified_type_name),
      $.element_content,
    ),

    element_content: $ => seq(
      '{',
      repeat($._element_content_member),
      '}',
    ),

    _element_content_member: $ => choice(
      // TODO: *PropertyDeclaration
      // TODO: *Binding
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

    // TODO: lex_identifier() accepts c.is_alphanumeric(), which may contain
    // non-ASCII Alpha/Nd/Nl/No character.
    identifier: $ => token(seq(
      /[a-zA-Z_]/,
      repeat(/[a-zA-Z0-9_\-]/),
    )),

    qualified_type_name: $ => sep1($.identifier, '.'),
  },
});

function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
