;;; init.el --- Init file for Emacs


;;; Commentary:
;; Initializes and configures Emacs

;;; Code:

;; Use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq native-comp-async-report-warnings-errors t)
(setq warning-minimum-level 'error)

;; Set Custom file to avoid clutter in this one
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Use `command' as `meta' and `control' in macOS
(setq mac-left-command-modifier 'meta)
(setq mac-right-command-modifier 'control)
(setq mac-option-modifier 'super)

;; Stop C-h on right CMD key from hiding frames.
(setq mac-pass-command-to-system nil)
(setq mac-pass-control-to-system nil)

;; Disable Toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable intro screen on startup
(setq inhibit-startup-message t)

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
(load-theme 'dracula t)

;; bind format-all for handling code formatting/prettifying.
(global-set-key (kbd "C-c C-y")  'format-all-buffer)

(setq-default format-all-formatters format-all-default-formatters)


(my/define-multiple-keys rust-mode-map
			 '(
                           ("C-c C-y" lsp-format-buffer)
			   ("C-c C-c" rust-run-clippy)
			   ("C-c C-r" rust-run)
			   ("C-c C-t" rust-test)
			   ("C-c C-o" rust-compile)))

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
  (unless (eq major-mode 'emacs-lisp-mode)
    (lsp-deferred)
    (local-set-key (kbd "C-c C-y") 'lsp-format-buffer)
    (tree-sitter-hl-mode)))

(add-hook 'javascript-mode-hook 'my/enable-ide-features)
(add-hook 'java-mode-hook 'my/enable-ide-features)
(add-hook 'html-mode-hook 'my/enable-ide-features)
(add-hook 'rust-mode-hook 'my/enable-ide-features)
(add-hook 'sh-mode-hook 'my/enable-ide-features)
(add-hook 'c-mode-hook 'my/enable-ide-features)
(add-hook 'emacs-lisp-mode-hook 'my/generic-code-hook)

(defun my/find-file-in-folder-shortcut (folder)
  "Interactively call find-file after using 'cd' into 'folder'."
  (cd (expand-file-name folder))
  (call-interactively #'find-file))

(defun apsc160 ()
  "Shortcut to APSC 160 folder."
  (interactive)
  (my/find-file-in-folder-shortcut "~/Documents/school/ubc/apsc160"))

(defun dev ()
  "Shortcut to '~/dev' folder."
  (interactive)
  (my/find-file-in-folder-shortcut "~/dev"))

(server-start)

(provide 'init)

;;; init.el ends here
