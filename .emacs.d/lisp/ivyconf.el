;;; ivyconf.el --- Configure Ivy, Counsel and Swiper

;;; Commentary:
;; Simply initialize the Ivy autocomplete engine and replace default
;; binds to emacs commands with Counsel commands, also enable search
;; with swiper.

;;; Code:

(use-package ivy
  :ensure t
  :commands (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :bind (("C-c C-r" . ivy-resume))
  :hook (after-init . ivy-mode))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
		 ("C-x C-f" . counsel-find-file)
		 ("<f1> f" . counsel-describe-function)
		 ("<f1> v" . counsel-describe-variable)
		 ("<f1> o" . counsel-describe-symbol)
		 ("<f1> l" . counsel-find-library)
		 ("<f2> i" . counsel-info-lookup-symbol)
		 ("<f2> u" . counsel-unicode-char)
		 ("C-c g" . counsel-git)
		 ("C-c j" . counsel-git-grep)
		 ("C-c k" . counsel-ag)
		 ("C-x l" . counsel-locate)
		 ("C-S-o" . counsel-rhythmbox)
		 :map minibuffer-local-map
		 ("C-r" . counsel-minibuffer-history)))

(provide 'ivyconf)

;;; ivyconf.el ends here
