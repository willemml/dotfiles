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
  (org-roam-setup)
  (org-roam-db-autosync-mode))
