;; My Personal Emacs config
;; - Nils


;;; BASIC SETTINGS
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(define-key menu-bar-tools-menu [games] nil)			;; Disable games
(column-number-mode t)
(scroll-bar-mode 0)
(setq backup-directory-alist '(("." . "~/.emacs-saves")))       ;; set dump directory
(fset 'yes-or-no-p 'y-or-n-p)  ;; y/n instead of yes/no

;;; INSTALL MY PACKAGES
(load "~/.emacs.d/util/init-packages")


;;; BUFFER RELATED
(setq-default frame-title-format '("Emacs - %b"))
;; show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; set font
(set-default-font "Source Code Pro")
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;; Search-Copy-Paste
(setq-default case-fold-search nil)   ;; make search case insensitive
(setq mouse-yank-at-point t) ;; paste at cursor pos


;;; INDENTATION
(setq-default indent-tabs-mode t)
(setq tab-width 2)
(define-key global-map (kbd "RET") 'newline-and-indent)


;;; LINE MOVEMENT
;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Move lines up or down
(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)


;; EXPAND-REGION
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)			;; remove marked section when typing

;; GOTO LINE
;;(global-set-key (kbd "M-g") 'goto-line)


;; LaTeX SETTINGS
;; AUCTeX settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; reftex
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
(global-set-key [down-mouse-3] 'imenu)

;; *.tex --> *.dvi --> *.ps --> *.pdf
(setq-default TeX-PDF-from-DVI "Dvips")


;;; PROGRAMMING

;; HASKELL STUFF
(setq haskell-process-log t)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


;; open on startup
(find-file "~/.emacs.d/init.el")
(find-file "~/.emacs.d/welcome.org")

(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (nord)))
 '(custom-safe-themes
	 (quote
		("82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" default)))
 '(package-selected-packages
	 (quote
		(expand-region rainbow-delimiters nord-theme zenburn-theme auctex-latexmk auctex el2org haskell-mode magit org-bullets pdf-tools))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
