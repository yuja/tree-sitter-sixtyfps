// Implemented based on
// https://github.com/sixtyfpsui/sixtyfps/
//   docs/langref.md
//   sixtyfps_compiler/{lexer.rs,parser.rs,parser/*.rs}
//                     {expression_tree.rs,object_tree.rs}
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
    $._statement,
    $._type,
    $._literal,
  ],

  precedences: $ => [
    ['unary', 'mul', 'add', 'equality', 'logical', 'ternary'],
  ],

  word: $ => $.identifier,

  rules: {
    document: $ => repeat(choice(
      $._component,
      // TODO: *ExportsList
      $.import_specifier,
      // TODO: *StructDeclaration
    )),

    import_specifier: $ => seq(
      'import',
      optional(seq(
        field('names', $.import_export_identifier_list),
        'from',
      )),
      field('source', $.string_literal),
      ';',
    ),

    import_export_identifier_list: $ => seq(
      '{',
      sep($.import_export_identifier, ','),
      '}',
    ),

    import_export_identifier: $ => seq(
      field('name', $.identifier),
      optional(seq(
        'as',
        field('alias', $.identifier),
      )),
    ),

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

    repeated_element: $ => seq(
      'for',
      field('model_data', optional($.identifier)),
      optional(seq(
        '[',
        field('index', $.identifier),
        ']',
      )),
      'in',
      field('model', $._expression),
      ':',
      $.sub_element,
    ),

    conditional_element: $ => seq(
      'if',
      field('condition', $._expression),
      ':',
      $.sub_element,
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
      $.property_declaration,
      $.binding,
      $.callback_declaration,
      $.callback_connection,
      $.sub_element,
      $.repeated_element,
      $.conditional_element,
      // TODO: *PropertyAnimation
      $.two_way_binding,
      // TODO: *States
      // TODO: *Transitions
      $.children_placeholder,
    ),

    property_declaration: $ => seq(
      'property',
      choice(
        seq(
          '<',
          field('type', $._type),
          '>',
          // TODO: better structure
          choice(
            seq(
              field('name', $.identifier),
              ';',
            ),
            field('binding', $.binding),
            field('binding', $.two_way_binding),
          ),
        ),
        seq(
          field('binding', $.two_way_binding),
        ),
      ),
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

    two_way_binding: $ => seq(
      field('name', $.identifier),
      '<=>',
      field('expr', $._expression),
      ';',
    ),

    callback_declaration: $ => seq(
      'callback',
      // TODO: better name vs binding structure
      choice(
        seq(
          field('name', $.identifier),
          optional(seq(
            field('parameters', $.callback_declaration_parameters),
            optional(seq(
              '->',
              field('return_type', $._type),
            )),
          )),
          ';',
        ),
        field('binding', $.two_way_binding),
      ),
    ),

    callback_declaration_parameters: $ => seq(
      '(',
      optional(seq(
        sep1($._type, ','),
        optional(','),
      )),
      ')',
    ),

    callback_connection: $ => seq(
      field('name', $.identifier),
      optional(field('parameters', $.callback_connection_parameters)),
      '=>',
      field('expr', $.code_block),
    ),

    callback_connection_parameters: $ => seq(
      '(',
      optional(seq(
        sep1($.identifier, ','),
        optional(','),
      )),
      ')',
    ),

    children_placeholder: $ => seq(
      '@',
      'children',
    ),

    _statement: $ => choice(
      $.empty_statement,
      $.if_statement,
      $.return_statement,
      $.self_assignment,
      $._expression_statement,
    ),

    empty_statement: $ => ';',

    // TODO: naming rule conflicts: SyntaxKind::ConditionalExpression
    if_statement: $ => seq(
      'if',
      field('condition', $.parenthesized_expression),
      field('true_expr', $.code_block),
      optional(seq(
        'else',
        field('false_expr', choice($.if_statement, $.code_block)),
      )),
    ),

    return_statement: $ => seq(
      'return',
      optional($._expression),
      ';',
    ),

    self_assignment: $ => seq(
      field('lhs', $._expression),
      field('op', choice('-=', '+=', '*=', '/=', '=')),
      field('rhs', $._expression),
      ';',
    ),

    _expression_statement: $ => seq(
      $._expression,
      ';',
    ),

    _expression: $ => choice(
      $._literal,
      $.parenthesized_expression,
      $.function_call_expression,
      $.conditional_expression,
      $.qualified_name,
      $.binary_expression,
      // TODO: ?Array
      // TODO: ?ObjectLiteral
      $.unary_op_expression,
      // TODO: ?StringTemplate
      // TODO: ?AtImageUrl
      // TODO: ?AtLinearGradient
    ),

    parenthesized_expression: $ => seq(
      '(',
      $._expression,
      ')',
    ),

    // It appears any expression can be a function name, which might be unintentional.
    function_call_expression: $ => seq(
      field('function', $._expression),
      field('arguments', $.arguments),
    ),

    arguments: $ => seq(
      '(',
      sep($._expression, ','),
      ')',
    ),

    unary_op_expression: $ => prec.right('unary', seq(
      field('op', choice('+', '-', '!')),
      field('sub', $._expression),
    )),

    binary_expression: $ => choice(
      prec.left('mul', seq(
        field('lhs', $._expression),
        field('op', choice('*', '/')),
        field('rhs', $._expression),
      )),
      prec.left('add', seq(
        field('lhs', $._expression),
        field('op', choice('+', '-')),
        field('rhs', $._expression),
      )),
      prec.left('equality', seq(
        field('lhs', $._expression),
        field('op', choice('==', '!=', '>=', '<=', '<', '>')),
        field('rhs', $._expression),
      )),
      prec.left('logical', seq(
        field('lhs', $._expression),
        field('op', choice('||', '&&')),
        field('rhs', $._expression),
      )),
    ),

    conditional_expression: $ => prec.right('ternary', seq(
      field('condition', $._expression),
      '?',
      field('true_expr', $._expression),
      ':',
      field('false_expr', $._expression),
    )),

    code_block: $ => seq(
      '{',
      repeat($._statement),
      '}',
    ),

    _type: $ => choice(
      // TODO: [ ... ]
      // TODO: { ... }
      alias($.qualified_name, $.qualified_type_name),
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

function sep(rule, sep) {
  return optional(sep1(rule, sep));
}

function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
