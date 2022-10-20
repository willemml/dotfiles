;;; myfuncs.el --- Custom functions for my config

;;; Commentary:
;; Some functions to improve my Emacs experience and make
;; configuration easier.

;;; Code:

(defun my/define-multiple-keys (map keys)
  "Define multiple KEYS in a keymap.
Argument MAP keymap in which to bind the keys."
  (dolist (key keys nil)
    (define-key map (kbd (car key)) (nth 1 key))))

(defun my/customize-set-variables (variables)
  "Set multiple Customize VARIABLES at once."
  (dolist (variable variables nil)
    (customize-set-variable (car variable) (nth 1 variable))))

(defun my/find-file-in-folder-shortcut (folder)
  "Interactively call `find-file' after using 'cd' into 'FOLDER'."
  (cd (expand-file-name folder))
  (call-interactively #'find-file))

(defun apsc160 ()
  "Shortcut to APSC 160 folder."
  (interactive)
  (my/find-file-in-folder-shortcut "~/Documents/school/ubc/apsc160"))

(defun dev ()
  "Shortcut to '~/dev' folder."
  (interactive)
  (my/find-file-in-folder-shortcut "~/dev"))

(provide 'myfuncs)

;;; myfuncs.el ends here
