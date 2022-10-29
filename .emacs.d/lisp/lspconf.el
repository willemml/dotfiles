;;; lspconf.el --- Configuration for code editing

;;; Commentary:
;; Setup lsp-mode for different languages, set some keybinds and
;; formatting settings.

;;; Code:

(require 'company)
(require 'rust-mode)
(require 'format-all)

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(require 'lsp-ui)
(require 'lsp-mode)
(require 'lsp-java)

;; Don't use tabs for indents
(setq indent-tabs-mode nil)

;; Increase garbage collector threshold
(setq gc-cons-threshold 100000000)

;; Increase emacs data read limit
(setq read-process-output-max (* 1024 1024))

;; Disable LSP logging
(setq lsp-log-io nil)

;; Enable treemacs/lsp-mode sync
(lsp-treemacs-sync-mode 1)

;; Bind Emacs built in completion using completion-at-point to "C-M-i"
(global-set-key (kbd "C-M-i") 'completion-at-point)

;; Keybind to format/prettify document, uses either format-all or
;; lsp-mode depending on availability
(global-set-key (kbd "C-c C-y")  'my/format-document)

;; Set the default formatters so that you aren't prompted every time.
(setq-default format-all-formatters format-all-default-formatters)

;; Align company-mode tooltips to the right hand side
(setq company-tooltip-align-annotations t)

;; Display number of completions before and after current suggestions
;; in company-mode
(setq company-tooltip-offset-display 'lines)

;; Display text icon of type in company popup
(setq company-format-margin-function #'company-text-icons-margin)

;; Binds for rust dev.
(my/define-multiple-keys rust-mode-map
						 '(
                           ("C-c C-y" lsp-format-buffer)
						   ("C-c C-c" rust-run-clippy)
						   ("C-c C-r" rust-run)
						   ("C-c C-t" rust-test)
						   ("C-c C-o" rust-compile)))

;; Use Google's Java code styling.
(setq lsp-java-format-settings-url
	  "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
(setq lsp-java-format-settings-profile
	  "GoogleStyle")

;; Use jdtls 1.16.0
(setq lsp-java-jdt-download-url
	  "https://download.eclipse.org/jdtls/milestones/1.16.0/jdt-language-server-1.16.0-202209291445.tar.gz")

;; Set indent level to 4
(setq-default tab-width 4)

(defun my/generic-code-hook ()
  "Enable some basic features for coding."
  (interactive)
  (electric-pair-local-mode)
  (company-mode)
  (electric-indent-local-mode))

(defun my/enable-ide-features ()
  "Enable IDE features like electric pair/indent and LSP."
  (interactive)
  (my/generic-code-hook)
  (lsp-deferred)
  (tree-sitter-hl-mode))

(defun my/format-document ()
  "Formats the buffer using 'lsp-format-buffer'.
Falls back to 'format-all-buffer' if LSP does not support formatting."
  (interactive)
  (cond ((lsp-feature? "textDocument/formatting")
		 (lsp-format-buffer))
		((lsp-feature? "textDocument/rangeFormatting")
		 (lsp-format-buffer))
		(t (format-all-buffer))))

(add-hook 'javascript-mode-hook 'my/enable-ide-features)
(add-hook 'java-mode-hook 'my/enable-ide-features)
(add-hook 'html-mode-hook 'my/enable-ide-features)
(add-hook 'rust-mode-hook 'my/enable-ide-features)
(add-hook 'sh-mode-hook 'my/enable-ide-features)
(add-hook 'c-mode-hook 'my/enable-ide-features)

(add-hook 'emacs-lisp-mode-hook 'my/generic-code-hook)

(add-hook 'after-init-hook #'global-flycheck-mode)


(provide 'lspconf)

;;; lspconf.el ends here
