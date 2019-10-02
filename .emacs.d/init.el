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

;; BiBTeX
(require 'company-bibtex)
(add-to-list 'company-backends 'company-bibtex)

(setq company-bibtex-bibliography
      '("~/Documents/University/LaTeX/uni.bib"))


;;; PROGRAMMING

;; CODE COMPLETION
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; PROJECT MANAGEMENT
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; C/C++
(add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))


;; HASKELL STUFF
(setq haskell-process-log t)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


;; open on startup
(find-file "~/.emacs.d/init.el")
(find-file "~/.emacs.d/welcome.org")

(global-set-key (kbd "C-x g") 'magit-status)

