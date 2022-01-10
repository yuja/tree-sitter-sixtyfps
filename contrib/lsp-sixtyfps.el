;;; lsp-sityfps.el --- LSP client for SixtyFPS UI -*- lexical-binding: t; -*-

;; Author: Yuya Nishihara <yuya@tcha.org>
;; Package-Requires: ((emacs "26.1") (lsp-mode "8.0"))

;;; Code:

(require 'lsp-mode)

(add-to-list 'lsp-language-id-configuration
             '(sixtyfps-mode . "sixtyfps"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "sixtyfps-lsp")
                  :activation-fn (lsp-activate-on "sixtyfps")
                  :server-id 'sixtyfps))

(provide 'lsp-sixtyfps)

;;; lsp-sixtyfps.el ends here
