no;;; packages.el --- Install and configure packages
;;; Commentary:
;; Package init file --- install and configure packages

;;; Code:

;; Install `use-package` if it isn't installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; Enable package manager
(require 'package)

;; Melpa
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; Org ELPA archive
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Enable `use-package`
(eval-when-compile
  (require 'use-package))

;; Ensure packages referenced by `use-package` are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Install the JetBrains Darcula theme
(use-package jetbrains-darcula-theme
	     :config
	     (load-theme 'jetbrains-darcula t))

;; Get emacsql-sqlite for org-roam
(use-package emacsql-sqlite)

;; Install `org-roam`
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  ;; Always wrap lines in org mode
  (org-startup-truncated nil)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))

;; LSP mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)

;; Rust mode
(use-package rust-mode
  :custom
  (rust-format-on-save t)
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :hook (rust-mode . (lambda () (prettify-symbols-mode)))
  :bind ("C-c C-c" . 'rust-run)
  :commands rust-run)

;; Rustic
(use-package rustic
  :custom
  (rustic-lsp-server 'rust-analyzer))

;; Highlighting with tree-sitter
(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Company mode for completion pop-ups
(use-package company
  :init (global-company-mode)
  :commands global-company-mode)

;; Flycheck for syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

(provide 'packages)

;;; packages.el ends here
