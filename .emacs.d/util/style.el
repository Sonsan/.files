(tool-bar-mode 0)
(menu-bar-mode 0)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(setq-default frame-title-format '("Emacs - %b"))
(define-key menu-bar-tools-menu [games] nil)			;; Disable games
(column-number-mode t)
(scroll-bar-mode 0)

;; set font
(set-default-font "Source Code Pro")

;; show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq mouse-yank-at-point t) ;; paste at cursor pos

;; Search-Copy-Paste
(setq-default case-fold-search nil)   ;; make search case insensitive
