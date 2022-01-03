;;; sityfps-mode.el --- Major mode for editing SixtyFPS UI -*- lexical-binding: t; -*-

;; Author: Yuya Nishihara <yuya@tcha.org>
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.16.1"))

;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-hl)

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

  (tree-sitter-hl-mode))

(add-to-list 'tree-sitter-major-mode-language-alist
             '(sixtyfps-mode . sixtyfps))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.60\\'" . sixtyfps-mode))

(provide 'sixtyfps-mode)

;;; sixtyfps-mode.el ends here
