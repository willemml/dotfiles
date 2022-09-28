;; Use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use `command` as `meta` in macOS
(setq mac-command-modifier 'meta)

;; Disable Toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable intro screen on startup
(setq inhibit-startup-message t)

;; Function to make defining different files for my config easier
(defun user-config-file (name relpath)
  (setq name
	(expand-file-name relpath user-emacs-directory))
  (load name))

;; Package config file, installing stuff
(user-config-file 'packages-file "packages.el")
;; Custom file, for whatever Custom does
(user-config-file 'custom-file "custom.el")
