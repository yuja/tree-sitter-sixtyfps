// Implemented based on
// https://github.com/sixtyfpsui/sixtyfps/
//   docs/langref.md
//   sixtyfps_compiler/{lexer.rs,parser.rs,parser/*.rs}
//                     {expression_tree.rs,object_tree.rs}
// 75ba29cf2fc593e824718bc2b21036b4ee2430f4

module.exports = grammar({
  name: 'sixtyfps',

  externals: $ => [
    $.block_comment,
  ],

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
    $._at_keyword,
    $._type,
    $._literal,
  ],

  precedences: $ => [
    ['unary', 'mul', 'add', 'equality', 'logical', 'ternary'],
    [$.object_literal, $.code_block],
  ],

  word: $ => $.identifier,

  rules: {
    document: $ => repeat(choice(
      $._component,
      $.exports_list,
      $.import_specifier,
      $.struct_declaration,
    )),

    // TODO: consolidate export/import node names
    exports_list: $ => seq(
      'export',
      choice(
        field('names', $.import_export_identifier_list),
        $._component,
        $.struct_declaration,
      ),
    ),

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
      $.property_animation,
      $.two_way_binding,
      $.states,
      $.transitions,
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

    qualified_binding: $ => seq(
      field('name', $.qualified_name),
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

    property_animation: $ => seq(
      'animate',
      field('property_names', choice(
        alias('*', $.property_wildcard),
        $.qualified_property_names,
      )),
      field('bindings', $.property_bindings),
    ),

    qualified_property_names: $ => sep1($.qualified_name, ','),

    property_bindings: $ => seq(
      '{',
      repeat($.binding),
      '}',
    ),

    states: $ => seq(
      'states',
      '[',
      repeat($.state),
      ']',
    ),

    state: $ => seq(
      field('id', $.identifier),
      optional(seq(
        'when',
        field('condition', $._expression),
      )),
      ':',
      field('property_changes', $.property_changes),
    ),

    property_changes: $ => seq(
      '{',
      repeat($.qualified_binding),
      '}',
    ),

    transitions: $ => seq(
      'transitions',
      '[',
      repeat($.transition),
      ']',
    ),

    transition: $ => seq(
      field('direction', choice('in', 'out')),
      field('id', $.identifier),
      ':',
      field('animations', $.property_animations),
    ),

    property_animations: $ => seq(
      '{',
      repeat($.property_animation),
      '}',
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
      trailingCommaSep($._type),
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
      trailingCommaSep($.identifier),
      ')',
    ),

    children_placeholder: $ => seq(
      '@',
      'children',
    ),

    code_block: $ => seq(
      '{',
      repeat($._statement),
      optional(choice(
        $.self_assignment,
        $._expression,
      )),
      '}',
    ),

    _statement: $ => choice(
      $.empty_statement,
      $.if_statement,
      $.return_statement,
      $._self_assignment_statement,
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
    ),

    _self_assignment_statement: $ => seq(
      $.self_assignment,
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
      $.array,
      $.object_literal,
      $.unary_op_expression,
      // TODO: ?StringTemplate
      $._at_keyword,
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
      trailingCommaSep($._expression),
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

    array: $ => seq(
      '[',
      trailingCommaSep($._expression),
      ']',
    ),

    // TODO: drop _literal from node name?
    object_literal: $ => seq(
      '{',
      trailingCommaSep($.object_member),
      '}',
    ),

    object_member: $ => seq(
      field('name', $.identifier),
      ':',
      field('expr', $._expression),
    ),

    _at_keyword: $ => choice(
      $.at_image_url,
      $.at_linear_gradient,
    ),

    at_image_url: $ => seq(
      '@',
      choice('image-url', 'image_url'),
      '(',
      field('source', $.string_literal),
      ')',
    ),

    at_linear_gradient: $ => seq(
      '@',
      choice('linear-gradient', 'linear_gradient'),
      field('arguments', $.at_linear_gradient_arguments),
    ),

    // TODO: strictness of @keyword arguments
    at_linear_gradient_arguments: $ => seq(
      '(',
      sep(choice($._literal, $.gradient_stop), ','),
      ')',
    ),

    gradient_stop: $ => seq(
      field('color', $.color_literal),
      field('position', $.number_literal),
    ),

    struct_declaration: $ => seq(
      'struct',
      field('name', $.identifier),
      ':=',
      field('fields', $.object_type),
    ),

    _type: $ => choice(
      $.array_type,
      $.object_type,
      alias($.qualified_name, $.qualified_type_name),
    ),

    array_type: $ => seq(
      '[',
      $._type,
      ']',
    ),

    object_type: $ => seq(
      '{',
      trailingCommaSep($.object_type_member),
      '}',
    ),

    object_type_member: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $._type),
    ),

    line_comment: $ => token(seq(
      '//',
      /.*/,
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

function trailingCommaSep(rule) {
  return optional(seq(
    sep1(rule, ','),
    optional(','),
  ));
}
