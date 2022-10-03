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
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  ;; Always wrap lines in org mode
  (org-startup-truncated nil)
  ;; Always open src blocks in current window
  (org-src-window-setup 'current-window)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ;; ("g" "math100-group-assignments" plain  (file "~/Documents/org-roam/templates/math100-group-assignments.org")
     ;;  :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
     ;;  :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 :map org-mode-map
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n I" . my/org-roam-node-insert-immediate)
	 ("C-c C-o" . my/follow-org-link)
	 ("C-c n c" . org-id-get-create)
	 ("C-c n a" . org-roam-alias-add)
	 ("C-c n t" . org-roam-tag-add)
	 ("C-M-i" . completion-at-point))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(defun my/org-roam-node-insert-immediate (arg &rest args)
  "Insert a link to a new node without capturing anything."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-force-open-current-window ()
  "Open a link using 'org-open-at-point' in current window."
  (interactive)
  (let ((org-link-frame-setup (quote
			       ((vm . vm-visit-folder)
				(vm-imap . vm-visit-imap-folder)
				(gnus. gnus)
				(file . find-file)
				(wl . wl)))
			      ))
    (org-open-at-point)))

(defun my/follow-org-link (arg)
  "Follow a link in org-mode If an argument is given, opens in new
window otherwise opens in current window."
  (interactive "P")
  (if arg
      (org-open-at-point)
    (my/org-force-open-current-window)))

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

(use-package
  ob-swiftui)
(ob-swiftui-setup)

(org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
							 (shell . t)
							 (js . t)
							 (calc . t)
							 (C . t)))

;; make it easy to publish all org docs as html
(setq org-publish-project-alist
      '(("Root"
         :base-directory "~/Documents/org-roam/"
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/public_html"
         :section-numbers nil
         :with-toc nil)))

(use-package org-roam)

(provide 'org)

;;; org.el ends here
