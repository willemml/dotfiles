;; Disable Toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)
;; Disable menubar
(menu-bar-mode -1)

;; Increase garbage collector threshold before load
(setq gc-cons-threshold 640000000)

(setq debug-on-error t)

;; Use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Minimize native-comp warnings
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level 'error)

;; Use `command' as `meta' and `control' in macOS
(setq mac-left-command-modifier 'meta)
(setq mac-right-command-modifier 'control)
(setq mac-option-modifier 'super)

;; Stop C-h on right CMD key from hiding frames.
(setq mac-pass-command-to-system nil)
(setq mac-pass-control-to-system nil)
