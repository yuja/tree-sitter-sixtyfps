;;; sityfps-mode.el --- Major mode for editing SixtyFPS UI -*- lexical-binding: t; -*-

;; Author: Yuya Nishihara <yuya@tcha.org>
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.16.1") (tree-sitter-indent "0.3"))

;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-indent)

(defconst sixtyfps-mode--tree-sitter-patterns
  [
   ;; Comments
   (line_comment) @comment
   (block_comment) @comment

   ;; Keywords
   ["animate"
    "as"
    "callback"
    "else"
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
    "when"] @keyword

   ;; Tokens
   [","
    "."
    ":"
    ";"] @punctuation.delimiter

   ["*"
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
    "=>"] @punctuation.operator

   ["("
    ")"
    "["
    "]"
    "{"
    "}"] @punctuation.bracket

   ;; Literals
   [(self)
    (parent)
    (root)] @variable.builtin

   [(string_fragment)
    (escape_sequence)
    "\""] @string
   (template_substitution ["\\{" "}"] @string)

   (number_literal) @number
   (color_literal) @string.special
   (bool_literal) @constant.builtin

   ;; At keywords
   (children_placeholder) @variable.builtin
   (at_image_url) @function.builtin
   (at_linear_gradient) @function.builtin

   ;; Functions
   (callback_declaration
    name: (identifier) @function)
   (callback_declaration
    binding: (two_way_binding
              name: (identifier) @function))

   (callback_connection
    name: (identifier) @function)
   (callback_connection_parameters
    (identifier) @variable.parameter)

   (function_call_expression
    function: (qualified_name (identifier) @function.call \.))

   ;; Properties
   (property_declaration
    ["<" ">"] @punctuation.bracket)

   (property_declaration
    name: (identifier) @property.definition)
   (property_declaration
    [(binding
      name: (identifier) @property.definition)
     (two_way_binding
      name: (identifier) @property.definition)])

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

   (state
    id: (identifier) @variable)
   (transition
    id: (identifier) @variable)

   ;; Types and variable identifiers
   (import_export_identifier
    (identifier) @type)

   (component
    id: (identifier) @type)
   (global_component
    id: (identifier) @type)
   (sub_element
    id: (identifier) @variable)
   (repeated_element
    [model_data: (identifier) @variable
     index: (identifier) @variable])

   (object_member
    name: (identifier) @property)

   (struct_declaration
    name: (identifier) @type)
   (object_type_member
    name: (identifier) @property.definition)

   (qualified_type_name
    (identifier) @type)

   (qualified_name
    (identifier) @type
    (.match? @type "^[A-Z]"))  ; assume uppercase names are types

   ;(identifier) @variable
   ])

(defcustom sixtyfps-indent-offset 4
  "Indent offset for SixtyFPS major mode."
  :type 'integer
  :group 'sixtyfps)

;; Empty line won't be indented properly, but let's start with structurally correct rule.
;; https://codeberg.org/FelipeLema/tree-sitter-indent.el/issues/8
(defconst sixtyfps-mode--indent-scopes
  '((indent-all
     ;; these nodes are always indented
     . ())
    (indent-rest
     ;; if parent node is one of this and node is not first -> indent
     . (self_assignment
        conditional_expression
        binary_expression))
    (indent-body
     ;; if parent node is one of this and current node is in middle -> indent
     . (import_export_identifier_list
        element_content
        states
        transitions
        property_animations
        property_bindings
        property_changes
        code_block
        array
        array_type
        object_literal
        object_type))
    (paren-indent
     ;; if parent node is one of these -> indent to paren opener
     . (callback_connection_parameters
        callback_declaration_parameters
        arguments
        at_linear_gradient_arguments
        parenthesized_expression))
    (align-char-to
     ;; chaining char -> node types we move parentwise to find the first chaining char
     . ())
    (aligned-siblings
     ;; siblings (nodes with same parent) should be aligned to the first child
     . ())
    (multi-line-text
     ;; if node is one of this, then don't modify the indent
     ;; this is basically a peaceful way out by saying "this looks like something
     ;; that cannot be indented using AST, so best I leave it as-is"
     . ())
    (outdent
     ;; these nodes always outdent (1 shift in opposite direction)
     . ())))

(defvar sixtyfps-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for SixtyFPS major mode.")

(defvar sixtyfps-mode-syntax-table
  (let ((table (make-syntax-table)))
    table))

;;;###autoload
(define-derived-mode sixtyfps-mode prog-mode "SixtyFPS"
  "Major mode for SixtyFPS UI.

\\{sixtyfps-mode-map}"
  :group 'sixtyfps
  :syntax-table sixtyfps-mode-syntax-table

  ; copied from csharp-tree-sitter.el
  ; https://github.com/ubolonton/emacs-tree-sitter/issues/84
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))

  (setq-local tree-sitter-hl-default-patterns sixtyfps-mode--tree-sitter-patterns)

  (setq-local indent-line-function #'tree-sitter-indent-line)
  (setq-local tree-sitter-indent-offset sixtyfps-indent-offset)
  (setq-local tree-sitter-indent-current-scopes sixtyfps-mode--indent-scopes)

  (tree-sitter-hl-mode))

(add-to-list 'tree-sitter-major-mode-language-alist
             '(sixtyfps-mode . sixtyfps))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.60\\'" . sixtyfps-mode))

(provide 'sixtyfps-mode)

;;; sixtyfps-mode.el ends here
