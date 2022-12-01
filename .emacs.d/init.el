;;; init.el --- Init file for Emacs


;;; Commentary:
;; Initializes and configures Emacs

;;; Code:

;; Increase garbage collector threshold before load
(require 'package)

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

(add-to-list 'load-path (concat user-emacs-directory "/use-package"))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Don't use tabs for indents
(setq indent-tabs-mode nil)

;; Don't warn when cannot guess python indent level
(setq python-indent-guess-indent-offset-verbose nil)

;; Set indent level to 4
(setq-default tab-width 4)

;; Increase emacs data read limit
(setq read-process-output-max (* 1024 1024))

;; Set Custom file to avoid clutter in this one
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Disable intro screen on startup
(setq inhibit-startup-message t)

(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

;; Load config
(require 'myfuncs)
(require 'ivyconf)
(require 'orgconf)
(require 'lspconf)

;; Load yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :config
  (yas-reload-all))

;; Put all backup files in the Trash
(setq backup-directory-alist '((".*" . "~/.Trash")))

;; Reduce wrist pain
(global-set-key (kbd "M-n") "~")
(global-set-key (kbd "M-N") "`")

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; Slightly lower GC threshold after init
(setq gc-cons-threshold 100000000)

(provide 'init)

;;; init.el ends here
