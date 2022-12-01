;;; lspconf.el --- Configuration for code editing

;;; Commentary:
;; Setup lsp-mode for different languages, set some keybinds and
;; formatting settings.

;;; Code:

(use-package company
  :ensure t
  :defer t
  :autoload company-text-icons-margin
  :init
  ;; Align company-mode tooltips to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; Display number of completions before and after current suggestions
  ;; in company-mode
  (setq company-tooltip-offset-display 'lines)

  ;; Display text icon of type in company popup
  (setq company-format-margin-function #'company-text-icons-margin)
  :hook ((company-mode . my/electric-mode)
		 (sh-mode . company-mode)
		 (emacs-lisp-mode . company-mode)))

(use-package rust-mode
  :ensure t
  :defer t
  :bind (:map rust-mode-map
			  ("C-c C-y" . lsp-format-buffer)
			  ("C-c C-c" . rust-run-clippy)
			  ("C-c C-r" . rust-run)
			  ("C-c C-t" . rust-test)
			  ("C-c C-o" . rust-compile)))

(use-package format-all
  :ensure t
  :commands format-all-buffer
  :defer t
  :hook (my/electric-mode format-all-ensure-formatter))

(use-package tree-sitter
  :ensure t
  :defer t
  :ensure tree-sitter-langs
  :hook ((rust-mode c-mode shell-mode javascript-mode python-mode) .tree-sitter-hl-mode))

(use-package lsp-mode
  :ensure t
  :after format-all
  :defer t
  :autoload lsp-deferred lsp-feature
  :commands lsp-format-buffer
  :preface
  (defun my/format-document ()
	"Formats the buffer using 'lsp-format-buffer'.
Falls back to 'format-all-buffer' if LSP does not support formatting."
	(interactive)
    (cond ((fboundp 'lsp-feature?)
	       (cond ((lsp-feature? "textDocument/formatting")
		          (lsp-format-buffer))
		         ((lsp-feature? "textDocument/rangeFormatting")
		          (lsp-format-buffer))
		         (t (format-all-buffer))))
          (t (format-all-buffer))))
  :custom
  (lsp-log-io nil "Disable LSP logging.")
  (lsp-keymap-prefix "C-c l" "Set the prefix for 'lsp-command-keymap'")
  :config
  (lsp-treemacs-sync-mode 1)
  :hook ((lsp-mode . company-mode)
		 (rust-mode . lsp)
		 (c-mode . lsp)
		 (javascript-mode . lsp))
  :bind ("C-c C-y" . my/format-document))

(use-package lsp-ui :commands lsp-ui-mode :ensure t)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol :ensure t)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list :ensure t)
(use-package lsp-java
  :ensure t
  :custom
  (lsp-java-format-settings-url
   "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml" "Use Google's Java code styling.")
  (lsp-java-format-settings-profile
   "GoogleStyle" "Use the correct profile for code styling.")
  (lsp-java-jdt-download-url
   "https://download.eclipse.org/jdtls/milestones/1.16.0/jdt-language-server-1.16.0-202209291445.tar.gz" "Use jdtls 1.16.0.")
  :hook (java-mode . lsp))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package arduino-mode
  :ensure t
  :custom (arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino"))
(use-package flycheck-arduino
  :ensure arduino-mode
  :hook (arduino-mode flycheck-arduino-setup))

;; Bind Emacs built in completion using completion-at-point to "C-M-i"
(global-set-key (kbd "C-M-i") 'completion-at-point)

;; Keybind to format/prettify document, uses either format-all or
;; lsp-mode depending on availability
(global-set-key (kbd "C-c C-y")  'my/format-document)

(provide 'lspconf)

;;; lspconf.el ends here
