;;; init.el --- Init file for Emacs


;;; Commentary:
;; Initializes and configures Emacs

;;; Code:

;; Use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level 'error)

;; Set Custom file to avoid clutter in this one
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Disable Toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable intro screen on startup
(setq inhibit-startup-message t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

;; Load config
(require 'myfuncs)
(require 'packages)
(require 'ivyconf)
(require 'orgconf)
(require 'lspconf)

;; Load yasnippet
(require 'yasnippet)
(require 'yasnippet-snippets)

;; Enable yasnippet
(yas-global-mode 1)

;; Reduce wrist pain
(global-set-key (kbd "M-n") "~")
(global-set-key (kbd "M-N") "`")

;; If darwin load macOS settings.
(if (eq system-type 'darwin)
	(require 'macos))

;; Bind to cleanup whitespace
(global-set-key (kbd "C-c M-c") 'whitespace-cleanup)

;; Spaces in minibuffer for org-roam
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; Install the JetBrains Darcula theme
(load-theme 'dracula t)

;; Start the Emacs deamon
(server-start)

(provide 'init)

;;; init.el ends here
