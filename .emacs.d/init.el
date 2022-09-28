;;; init.el --- Init file for Emacs


;;; Commentary:
;; Initializes and configures Emacs

;;; Code:

;; Use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use `command` as `meta` in macOS
(setq mac-command-modifier 'meta)

;; Disable Toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable intro screen on startup
(setq inhibit-startup-message t)

;; Function to make defining different files for my config easier
(defun user-config-file (name relpath)
  "Define and load a config file.
NAME is the variable name for the path (RELPATH is the path of
the file relative to the Emacs dir)."
  (setq name
	(expand-file-name relpath user-emacs-directory))
  (load name))

;; Increase for lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Package config file, installing stuff
(user-config-file 'packages-file "packages.el")
;; Custom file, for whatever Custom does
(user-config-file 'custom-file "custom.el")

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company company-mode jetbrains-darcula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
