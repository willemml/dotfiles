;;; org.el --- Configuration and setup for org-mode and org-roam
;;; Commentary:
;; Org config file --- configure org mode and org roam

;;; Code:

;; Org ELPA archive
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Get emacsql-sqlite for org-roam
(use-package 
  emacsql-sqlite)

;; Install `org-roam`
(use-package 
  org-roam 
  :demand t 
  :defines org-roam-v2-ack 
  :init (setq org-roam-v2-ack t) 
  :custom (org-roam-directory (file-truename "~/Documents/org-roam")) 
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  ;; Always wrap lines in org mode
  (org-startup-truncated nil) 
  (org-roam-completion-everywhere t) 
  :bind (("C-c n l" . org-roam-buffer-toggle) 
	 ("C-c n f" . org-roam-node-find) 
	 ("C-c n i" . org-roam-node-insert) 
	 :map org-mode-map ("C-M-i" . completion-at-point)) 
  :bind-keymap ("C-c n d" . org-roam-dailies-map) 
  :config (require 'org-roam-dailies) 
  (org-roam-db-autosync-mode))

;; Org Tempo for quick structure templates
(require 'org-tempo)

;; Live previews of LaTeX fragments
(use-package 
  org-latex-impatient 
  :defer t 
  :hook (org-mode . org-latex-impatient-mode) 
  :init 
  :custom (org-latex-impatient-tex2svg-bin "~/.local/bin/tex2svg"))

;; Automatic toggling of LaTeX previews when entering/exiting
(use-package 
  org-fragtog 
  :hook (org-mode 'org-fragtop-mode))

(provide 'org)

;;; org.el ends here
