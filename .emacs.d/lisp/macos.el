;;; macos.el --- macOS specific configuration

;;; Commentary:
;; Sets some macOS specific configs, especially around keybindings
;; (relocation control and meta).

;;; Code:

;; Use `command' as `meta' and `control' in macOS
(setq mac-left-command-modifier 'meta)
(setq mac-right-command-modifier 'control)
(setq mac-option-modifier 'super)

;; Stop C-h on right CMD key from hiding frames.
(setq mac-pass-command-to-system nil)
(setq mac-pass-control-to-system nil)

;; Put all backup files in the Trash
(setq backup-directory-alist '((".*" . "~/.Trash")))

(require 'org-mac-image-paste)

(provide 'macos)

;;; macos.el ends here
