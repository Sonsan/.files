;; My Personal Emacs config
;; - Nils


;;; BASICS
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(define-key menu-bar-tools-menu [games] nil)			;; Disable games
(column-number-mode t)
(scroll-bar-mode 0)
(setq backup-directory-alist '(("." . "~/.emacs-saves")))       ;; autosave directory
(fset 'yes-or-no-p 'y-or-n-p)                                   ;; y/n instead of yes/no
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Reload config


;; initialize package repos
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

;;; USE-PACKAGE SETUP
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; BUFFER RELATED
(setq-default frame-title-format '("Emacs - %b"))
;; show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; set font
(set-default-font "Source Code Pro")

;; transparancy
(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(global-set-key (kbd "C-c C-t") 'transparency)


;; Search-Copy-Paste
(setq-default case-fold-search nil)   ;; make search case insensitive
(setq mouse-yank-at-point t) ;; paste at cursor pos

;; Colorful brackets
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Colorful colorcodes
(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode t))

;; Indentation
(setq-default indent-tabs-mode t)
(setq tab-width 2)
(define-key global-map (kbd "RET") 'newline-and-indent)


;;; LINE MOVEMENT

;; movement via avy
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))


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

(defun move-word-left ()
  "Move the current word left."
  (interactive)
  (forward-word -1)
  (transpose-words 1)
  (forward-word 1))

(defun move-word-right ()
  "Move the current word right"
  (interactive)
  (forward-word 1)
  (transpose-words 1)
  (forward-word -1))

;; Move lines up or down
(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)
(global-set-key [(control shift left)] 'move-word-left)
(global-set-key [(control shift right)] 'move-word-right)

;; EXPAND-REGION
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region))
  :config
  (pending-delete-mode t))

;; GOTO LINE
;;(global-set-key (kbd "M-g") 'goto-line)


;; LaTeX SETTINGS
;; AUCTeX settings
;; (use-package auctex
;  :ensure t
;  :config
;  (setq TeX-auto-save t)
;  (setq TeX-parse-self t)
;  (setq-default TeX-master nil))


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
(use-package company-bibtex
  :ensure t
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography '("~/Documents/University/TeX/uni.bib")))



;;; PROGRAMMING

;; CODE COMPLETION
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package company-bibtex
    :ensure t))


;; PROJECT MANAGEMENT
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; C/C++
(add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))


;; HASKELL STUFF
(setq haskell-process-log t)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


;;; VERSION CONTROL
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; open on startup
(find-file "~/.emacs.d/init.el")
(find-file "~/.emacs.d/welcome.org")


;;; ORG MODE
;; org bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme nord-theme auctex-latexmk magit projectile company-bibtex expand-region rainbow-delimiters use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
