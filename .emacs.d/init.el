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

;; Don't use tabs for indents
(setq indent-tabs-mode nil)

(defun my/config-file (name relpath)
  "Define and load a config file.
NAME is the variable name for the path (RELPATH is the path of
the file relative to the Emacs dir)."
  (setq name (expand-file-name relpath user-emacs-directory))
  (load name))

(defun my/define-multiple-keys (map keys)
  "Define multiple keys in a keymap."
  (dolist (key keys nil)
    (define-key map (kbd (car key)) (nth 1 key))))

(defun my/customize-set-variables (variables)
  "Set multiple Customize variables at once."
  (dolist (variable variables nil)
    (customize-set-variable (car variable) (nth 1 variable))))

;; Custom file, for whatever Custom does
(my/config-file 'custom-file "custom.el")
;; Package config file, installing stuff
(my/config-file 'packages-file "packages.el")
;; org-mode and org-roam related config
(my/config-file 'orgmode-orgroam-file "org.el")

;; Bind "C-c C-/ to comment out a region
(global-set-key (kbd "C-c C-/") 'comment-region)

;; Reduce wrist pain
(global-set-key (kbd "M-n") "~")
(global-set-key (kbd "M-N") "`")

;; Bind to cleanup whitespace
(global-set-key (kbd "C-c M-c") 'whitespace-cleanup)

;; Bind Emacs built in completion using completion-at-point to "C-M-i"
(global-set-key (kbd "C-M-i") 'completion-at-point)

;; Spaces in minibuffer for org-roam
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; Install the JetBrains Darcula theme
(load-theme 'jetbrains-darcula t)

;; bind format-all for handling code formatting/prettifying.
(global-set-key (kbd "C-c C-y")  'format-all-buffer)

(setq-default format-all-formatters format-all-default-formatters)

(global-tree-sitter-mode)
(add-hook 'after-init-hook 'global-company-mode)

(define-key rust-mode-map (kbd "C-c C-r") 'rust-run)

(my/define-multiple-keys rust-mode-map
			 '(
			   ("C-c C-c" rust-run-clippy)
			   ("C-c C-r" rust-run)
			   ("C-c C-t" rust-test)
			   ("C-c C-o" rust-compile)))

(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)

(server-start)

(provide 'init)

;;; init.el ends here
