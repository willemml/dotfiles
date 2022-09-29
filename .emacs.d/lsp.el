;;; lsp.el --- LSP config

;;; Commentary:
;; LSP config --- Configure lsp-mode for languages I use regularly.

;;; Code:

;; LSP mode
(use-package
  lsp-mode						;;
  :init							;;
  (setq lsp-keymap-prefix "C-c l")			;;
  :hook ((rust-mode . lsp)				;;
	 (lsp-mode . lsp-enable-which-key-integration))	;;
  :commands lsp)					;;
(use-package
  lsp-ui
  :commands lsp-ui-mode)

;; Rust mode
(use-package
  rust-mode
  :custom (rust-format-on-save t)
  :hook (rust-mode . (lambda ()
		       (setq indent-tabs-mode nil)))
  :hook (rust-mode . (lambda ()
		       (prettify-symbols-mode)))
  :bind ("C-c C-c" . 'rust-run)
  :commands rust-run)

;; Rustic
(use-package
  rustic
  :custom (rustic-lsp-server 'rust-analyzer))

;; Highlighting with tree-sitter
(use-package
  tree-sitter-langs)
(use-package
  tree-sitter
  :config (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Company mode for completion pop-ups
(use-package
  company
  :init (global-company-mode)
  :commands global-company-mode)

;; Flycheck for syntax checking
(use-package
  flycheck
  :init (global-flycheck-mode))


(provide 'lsp)

;;; lsp.el ends here
