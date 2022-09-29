;;; init.el --- Init file for Emacs


;;; Commentary:
;; Initializes and configures Emacs

;;; Code:

;; Use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set Custom file to avoid clutter in this one
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Use `command` as `meta` in macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Disable Toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable intro screen on startup
(setq inhibit-startup-message t)

;; Function to make defining different files for my config easier
(defun my/config-file (name relpath)
  "Define and load a config file.
NAME is the variable name for the path (RELPATH is the path of
the file relative to the Emacs dir)."
  (setq name (expand-file-name relpath user-emacs-directory))
  (load name))

;; Increase for lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Bind "C-c C-/ to comment out a region
(global-set-key (kbd "C-c C-/") 'comment-region)

;; Reduce wrist pain
(global-set-key (kbd "M-n") "~")
(global-set-key (kbd "M-N") "`")

;; Bind to cleanup whitespace
(global-set-key (kbd "C-c M-c") 'whitespace-cleanup)

;; Spaces in minibuffer for org-roam
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; Package config file, installing stuff
(my/config-file 'packages-file "packages.el")
;; Custom file, for whatever Custom does
(my/config-file 'custom-file "custom.el")
;; org-mode and org-roam related config
(my/config-file 'orgmode-orgroam-file "org.el")
;; LSP config
(my/config-file 'lsp-config-file "lsp.el")

(provide 'init)

;;; init.el ends here
