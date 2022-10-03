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

;; format-all.el for handling all code formatting/prettifying.
(use-package
  format-all
  :bind ("C-c C-y" . 'format-all-buffer))

(defun my-format-all-hook (language formatter)
  "Set the default format-all formatter for language.
See 'format-all-formatters' for more info."
  (add-to-list 'format-all-formatters '(language formatter)))

(setq-default format-all-formatters format-all-default-formatters)

(provide 'packages)

;;; packages.el ends here
