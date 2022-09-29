;;; packages.el --- Install and configure packages
;;; Commentary:
;; Package init file --- install and configure packages

;;; Code:

;; Install `use-package` if it isn't installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; Enable package manager
(require 'package)

;; Melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Enable `use-package`
(eval-when-compile
  (require 'use-package))

;; Ensure packages referenced by `use-package` are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Install the JetBrains Darcula theme
(use-package
  jetbrains-darcula-theme
  :config (load-theme 'jetbrains-darcula t))

(defun elisp-format-buffer-cleanup-whitespace ()
  "Format Emacs Lisp code and then run 'whitespace-cleanup'."
  (interactive)
  (elisp-format-buffer)
  (whitespace-cleanup))

(defun my-elisp-mode-before-save-hook ()
  "Format Emacs Lisp code when in 'emacs-lisp-mode'.
Check if major mode is 'emacs-lisp-mode' then format code and
cleanup whitespace."
  (when (eq major-mode 'emacs-lisp-mode)
    (elisp-format-buffer-cleanup-whitespace)))

;; Format on save
(add-hook 'before-save-hook #'elisp-format-buffer-cleanup-whitespace)

;; Elisp Format to make elisp code look nice, also set keybind ~C-c
;; C-f~ to format elisp code in buffers
(use-package
  elisp-format
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-f" . elisp-format-buffer-cleanup-whitespace)))

(provide 'packages)

;;; packages.el ends here
