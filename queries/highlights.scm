; Functions

(callback_declaration
  name: (identifier) @function)
(callback_declaration
  binding: (two_way_binding
             name: (identifier) @function))

(callback_connection
  name: (identifier) @function)
(callback_connection_parameters
  (identifier) @variable.parameter)

; Properties

(property_declaration
  ["<" ">"] @punctuation.bracket)

(property_declaration
  name: (identifier) @property)
(binding
  name: (identifier) @property)
(qualified_binding
  name: (qualified_name
          (identifier) @property))
(two_way_binding
  name: (identifier) @property)

(qualified_property_names
  (qualified_name
    (identifier) @property))

; At keywords

(children_placeholder) @variable.builtin
(at_image_url) @function.builtin
(at_linear_gradient) @function.builtin

; Types and variable identifiers

(import_export_identifier
  (identifier) @type)

(component
  id: (identifier) @type)
(global_component
  id: (identifier) @type)
(sub_element
  id: (identifier) @variable)

(object_member
  name: (identifier) @property)

(qualified_type_name
 (identifier) @type)

(qualified_name
  (identifier) @type
  (#match? @type "^[A-Z]"))  ; assume uppercase names are types

; TODO: maybe introduce dedicated nodes for these identifiers?
((identifier) @variable.builtin
  (#eq? @variable.builtin "self"))
((identifier) @variable.builtin
  (#eq? @variable.builtin "parent"))
((identifier) @constant.builtin
  (#eq? @constant.builtin "true"))
((identifier) @constant.builtin
  (#eq? @constant.builtin "false"))
(identifier) @variable

; Literals

(line_comment) @comment
(block_comment) @comment

(string_literal) @string

(number_literal) @number

(color_literal) @string.special

; Tokens

[
  ","
  "."
  ":"
  ";"
] @punctuation.delimiter

[
  "*"
  "+"
  "-"
  "/"
  "!"
  "!="
  "<"
  "<="
  "=="
  ">"
  ">="
  "&&"
  "||"
  "*="
  "+="
  "-="
  "/="
  ":="
  "="
  "->"
  "<=>"
  "=>"
] @punctuation.operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

; Keywords

[
  "animate"
  "as"
  "callback"
  "export"
  "for"
  "from"
  "global"
  "if"
  "import"
  "in"
  "out"
  "property"
  "return"
  "states"
  "struct"
  "transitions"
  "when"
] @keyword
