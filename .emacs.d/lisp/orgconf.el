;;; org.el --- Configuration and setup for org-mode and org-roam
;;; Commentary:
;; Org config file --- configure org mode and org roam

;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-tempo)
(require 'org-fragtog)
(require 'org-roam-ui)
(require 'org-roam-dailies)
(require 'org-latex-impatient)
(require 'ox-publish)
(require 'plantuml-mode)
(require 'emacsql-sqlite)
(require 'graphviz-dot-mode)

(setq org-roam-directory (file-truename "~/Documents/org-roam"))
(setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))

;; Disable the 'validate' button on HTML exported Org docs.
(setq org-html-validation-link nil)

(my/customize-set-variables
 '('(org-roam-v2-ack t)
   ;; Always open src blocks in current window
   '(org-src-window-setup 'current-window)
   '(org-roam-completion-everywhere t)
   '(org-latex-impatient-tex2svg-bin "~/.local/bin/tex2svg")
   '(org-roam-capture-templates
     '(("d" "default" plain "%?"
		:target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
		:unnarrowed t) ))))

(org-roam-db-autosync-mode)

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
  (let ((org-link-frame-setup
		 (quote
		  ((vm . vm-visit-folder)
		   (vm-imap . vm-visit-imap-folder)
		   (gnus. gnus)
		   (file . find-file)
		   (wl . wl)))
		 ))
    (org-open-at-point)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files
		(delete-dups
		 (my/org-roam-list-notes-by-tag "todo"))))

;; make sure plantuml uses the executable from brew and not jar
(setq plantuml-executable-path "/opt/homebrew/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)
(setq org-plantuml-executable-path plantuml-executable-path)
(setq org-plantuml-exec-mode 'plantuml)

;; Build the agenda list the first time for the session
(my/refresh-agenda-list)

(defun my/follow-org-link (arg)
  "Follow a link in org-mode If an argument is given, opens in new
window otherwise opens in current window."
  (interactive "P")
  (if arg
      (org-open-at-point)
    (my/org-force-open-current-window)))

(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook 'org-latex-impatient-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(define-key graphviz-dot-mode-map (kbd "C-c C-y") 'graphviz-dot-indent-graph)

(org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
														 (shell . t)
														 (js . t)
														 (dot . t)
														 (calc . t)
														 (plantuml . t)
														 (C . t)))

;; Re-display images after executing org-babel (mainly for PlantUML)
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; Disable evaluation confirmation for certain languages.
(defun my/org-confirm-babel-evaluate (lang body)
  (not (member lang '("plantuml" "dot"))))

(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)

;; make it easy to publish all org docs as html
(setq org-publish-project-alist
      '(("Root"
		 :base-directory "~/Documents/org-roam/"
		 :publishing-function org-html-publish-to-html
		 :publishing-directory "~/public_html"
		 :section-numbers nil
		 :with-author nil
		 :with-creator t
		 :with-toc t
		 :time-stamp-file nil
		 :with-toc nil)))

;; Hide tags on exported documents
(setq org-export-with-tags nil)

;; Configure HTML export
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

(setq org-latex-impatient-tex2svg-bin
      (expand-file-name "~/.config/yarn/global/node_modules/.bin/tex2svg"))

(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n d") 'org-roam-dailies-map)
(global-set-key (kbd "C-c n a") 'org-agenda)

(my/define-multiple-keys org-mode-map
						 '(
						   ("C-c n i" org-roam-node-insert)
						   ("C-c n I" my/org-roam-node-insert-immediate)
						   ("C-c C-o" my/follow-org-link)
						   ("C-c n c" org-id-get-create)
						   ("C-c n n" org-roam-alias-add)
						   ("C-c n t" org-roam-tag-add)))

(provide 'orgconf)

;;; org.el ends here
