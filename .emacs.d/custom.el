;;; custom.el --- Custom config save

;;; Commentary:
;; Where Custom saves its stuff

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link-frame-setup
   '((file . find-file)
     (vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (wl . wl-other-frame)))
 '(package-selected-packages
   '(org-roam-ui format-all gnuplot ob-swiftui org-latex-impatient org-fragtog org-tempo org-mode elisp-format jetbrains-darcula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)

;;; custom.el ends here
