;;; org.el --- Configuration and setup for org-mode and org-roam
;;; Commentary:
;; Org config file --- configure org mode and org roam

;;; Code:

(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))

(use-package org
  :ensure t
  :pin gnu
  :preface
  (defun my/indent-org-block-automatically ()
	"Indent the current org code block."
	(interactive)
	(when (org-in-src-block-p)
	  (org-edit-special)
      (indent-region (point-min) (point-max))
      (org-edit-src-exit)))

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

  (defun my/follow-org-link (arg)
	"Follow a link in orgmode If ARG is given.
Opens in new window otherwise opens in current window."
	(interactive "P")
	(if arg
		(org-open-at-point)
	  (my/org-force-open-current-window)))

  :custom
  (org-src-window-setup 'current-window "Always open src blocks in current window.")
  (org-confirm-babel-evaluate nil "Don't ask for confirmation when executing code in org.")
  (org-src-fontify-natively t "Syntax highlighting in org src blocks.")
  (org-src-tab-acts-natively t "Make tabs act as they should for the language in org code blocks.")
  (org-src-preserve-indentation t "Keep indentation the same (align with leftmost column.)")

  (org-plantuml-executable-path plantuml-executable-path)
  (org-plantuml-exec-mode 'plantuml)

  (org-export-with-tags nil "Hide tags on exported documents.")

  (org-babel-python-command "/opt/homebrew/bin/python3.10")

  (org-publish-project-alist
   '(("Root"
	  :base-directory "~/Documents/org-roam/"
	  :publishing-function org-html-publish-to-html
	  :publishing-directory "~/public_html"
	  :section-numbers nil
	  :with-author nil
	  :with-creator t
	  :with-toc t
	  :time-stamp-file nil)) "Make it easy to publish all org docs as html.")

  ;; Configure HTML export
  (org-html-validation-link nil            "Don't show validation link.")
  (org-html-head-include-scripts nil       "Use our own scripts.")
  (org-html-head-include-default-style nil "Use our own styles.")
  (org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
  (org-html-section)

  :hook ((org-mode . auto-fill-mode)
		 ;; Re-display images after executing org-babel (mainly for PlantUML)
		 (org-babel-after-execute . org-redisplay-inline-images))

  :bind (("C-c n c" . org-id-get-create)
		 ("C-c n a" . org-agenda)
		 :map org-mode-map
		 ("C-c C-o" . my/follow-org-link)
		 ("C-c C-y" . my/indent-org-block-automatically))
  :config
  (require 'org-tempo)
  (require 'ox-publish))

(use-package htmlize :ensure t)

(use-package emacsql-sqlite :ensure t)

(use-package org-contrib
  :ensure t
  :pin nongnu)

(use-package org-roam
  :ensure t
  :preface
  (defun my/org-roam-node-insert-immediate (arg &rest args)
	"Insert a link to a new node ARG with ARGS without capturing anything."
	(interactive "P")
	(let ((args (cons arg args))
		  (org-roam-capture-templates (list (append (car org-roam-capture-templates)
													'(:immediate-finish t)))))
	  (apply #'org-roam-node-insert args)))


  (defun my/org-roam-filter-by-tag (tag-name)
	"Filter org notes by tag TAG-NAME."
	(lambda (node)
	  (member tag-name (org-roam-node-tags node))))

  (defun my/org-roam-list-notes-by-tag (tag-name)
	"List all org notes with the tag TAG-NAME."
	(mapcar #'org-roam-node-file
			(seq-filter
			 (my/org-roam-filter-by-tag tag-name)
			 (org-roam-node-list))))

  (defun my/refresh-agenda-list ()
	"Refresh the list of files to search for agenda entries."
	(interactive)
	(setq org-agenda-files
		  (delete-dups
		   (my/org-roam-list-notes-by-tag "todo"))))

  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n d" . org-roam-dailies-map)
		 :map org-mode-map
		 ("C-c n t" . org-roam-tag-add)
		 ("C-c n n" . org-roam-alias-add)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n I" . my/org-roam-node-insert-immediate))
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-v2-ack t)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(("d" "default" plain "%?"
								 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
								 :unnarrowed t)))
  :commands (org-roam-db-sync)
  :hook ((emacs-startup . org-roam-db-sync))
  :config
  (org-roam-db-autosync-mode)
  ;; Build the agenda list the first time for the session
  (my/refresh-agenda-list))

(use-package org-roam-ui
  :ensure t
  :defer t
  :after org-roam)

(use-package plantuml-mode
  :ensure t
  :custom
  ;; make sure plantuml uses the executable from brew and not jar
  (plantuml-executable-path "/opt/homebrew/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package graphviz-dot-mode
  :defer t
  :bind (:map graphviz-dot-mode-map
			  ("C-c C-y" . graphviz-dot-indent-graph)))

(use-package ob-arduino
  :ensure org
  :ensure arduino-mode
  :commands (org-babel-execute:arduino))

(use-package ob-ruby
  :ensure org
  :commands (org-babel-execute:ruby))
(use-package ob-shell
  :ensure org
  :commands (org-babel-execute:sh
			 org-babel-execute:shell
			 org-babel-execute:bash
			 org-babel-execute:ash
			 org-babel-execute:csh
			 org-babel-execute:zsh
			 org-babel-execute:ksh
			 org-babel-execute:dash
			 org-babel-execute:posh
			 org-babel-execute:mksh))
(use-package ob-js
  :ensure org
  :commands (org-babel-execute:js))
(use-package ob-dot
  :ensure org
  :commands (org-babel-execute:dot))
(use-package ob-calc
  :ensure org
  :commands (org-babel-execute:calc))
(use-package ob-plantuml
  :ensure org
  :commands (org-babel-execute:plantuml))
(use-package ob-emacs-lisp
  :ensure org
  :commands (org-babel-execute:elisp
			 org-babel-execute:emacs-lisp))
(use-package ob-c
  :ensure org
  :commands (org-babel-execute:C
			 org-babel-exectue:C++))
(use-package ob-python
  :ensure org
  :commands (org-babel-execute:python)
  :config
  (defun my/org-babel-execute:python-session (body params)
	(let ((session-name (cdr (assq :session params))))
	(when (not (eq session-name "none"))
	  (org-babel-python-initiate-session session-name))))
  (advice-add #'org-babel-execute:python :before #'my/org-babel-execute:python-session))

(provide 'orgconf)

;;; org.el ends here
