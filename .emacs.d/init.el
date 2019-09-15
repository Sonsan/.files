;; BASIC SETTINGS =================================
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; installs my packages via melpa
(load "~/.emacs.d/init-packages")

(setq-default frame-title-format '("Emacs - %b"))
(define-key menu-bar-tools-menu [games] nil)			;; Disable games
(column-number-mode t)
(scroll-bar-mode 0)

(setq mouse-yank-at-point t) ;; paste at cursor pos

;; set font
(set-default-font "Source Code Pro")

;; show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Search-Copy-Paste
(setq-default case-fold-search nil)   ;; make search case insensitive


;; MISC ================================================

;; set dump directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; HASKELL STUFF
(setq haskell-process-log t)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq default-directory "~/")

;; open on startup
(find-file "~/.emacs.d/init.el")
(find-file "~/.emacs.d/welcome.org")

;; set the default widths
(setq-default tab-width 2)

;; allow normal use of TAB
(defun myTab ()
  "Insert tab char. (ASCII 2, \t)"
  (interactive)
  (insert "\t"))

(global-set-key (kbd "TAB") 'myTab) ; same as Ctrl+i


;; LaTeX Stuff

;; AUCTeX settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; reftex
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
(global-set-key [down-mouse-3] 'imenu)

;; *.tex --> *.dvi --> *.ps --> *.pdf
(setq-default TeX-PDF-from-DVI "Dvips")

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

