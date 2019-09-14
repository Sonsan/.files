;; BASIC SETTINGS =================================
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "
LaTeX Commands:

Notes: 
	/_ M is Alt
	/_ C is Strg/Ctrl
	/_ X-Y means pressing keys X and Y together

TODO: Change Insert Enviroment aka `LaTeX-enviroment` to insert lables 

------- Enviroment Related -------
	Insert Enviroment: 			C-c C-e RET `env-name`
	Insert Labled Section:	C-c C-s RET `chapter|section|(sub^*)section`

	Close open Enviroment: 	C-c ]
	move to \\begin{..}: 		C-M-a
	move to \\end{..}:			C-M-e

------- Text Related -------
	Bold: 			C-c C-f C-b
	Italic: 		C-c C-f C-i
	Emphazised: C-c C-f C-e
	Slanted: 		C-c C-f C-s
	
	To reset the font format on cursor position,
	type: C-c C-f C-d
	i.e.: let | be the cursor position
		\\textbf{This is| bold} --> C-c C-f C-d --> This is| bold
	
	Matching Curly-braces: C-c {
	Properly sized braces: C-c C-m	


------- Completion -------
	Show possible completions: M-TAB
		i.e.: pressing M-TAB after ` \\begin{i ` will yield \\begin{itemize}
					pressing after `\\begin{ ` will open a buffer listing all possible completions


------- Selection -------
	Mark current section: 		C-c *
	Mark current Enviroment: 	C-c .

------- Output Format -------
	Toggle between DVI & PDF: C-C C-t C-p
	Toggle interactive mode: 	C-c C-t C-i
	
	To compile a specific region, do the following:
		1. Mark the region, which you wish to compile
		2. type C-c C-r
		3.1. open the file with a pdf viewer
		3.2. type C-c C-c View

	Note: 3.2 doesn't work FOR PDF in my case(i probably accidentaly chaged something in `TeX-evince-sync-view` config)
		/_ DVI works fine  
")

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

;; =================================================


;; MISC ================================================

;; set dump directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; HASKELL STUFF
(setq haskell-process-log t)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq default-directory "~/")

;; set the default widths
(setq-default tab-width 2)

;; allow normal use of TAB
(defun myTab ()
  "Insert tab char. (ASCII 2, \t)"
  (interactive)
  (insert "\t"))

(global-set-key (kbd "TAB") 'myTab) ; same as Ctrl+i


;; LATEX-RELATED =============================

;; AUCTeX settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

(global-set-key [down-mouse-3] 'imenu)

(setq-default TeX-PDF-from-DVI t)

;; ===========================================


;; PACKAGES(Auto-Generated) ======================================
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Move annoying custom-set-variables to external file
(setq annoying-customs-file "~/.emacs.d/custom.el")
(unless (file-exists-p annoying-customs-file)
  (write-region "" nil annoying-customs-file))

;; load the file containing the annoying to look at variables
(load annoying-customs-file nil t)

;; ============================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
	 (quote
		("84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "a317b11ec40485bf2d2046d2936946e38a5a7440f051f3fcc4cdda27bde6c5d4" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "5f27195e3f4b85ac50c1e2fac080f0dd6535440891c54fcfa62cdcefedf56b1b" default)))
 '(fci-rule-color "#3C3D37")
 '(haskell-interactive-popup-errors t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
	 (quote
		(("#3C3D37" . 0)
		 ("#679A01" . 20)
		 ("#4BBEAE" . 30)
		 ("#1DB4D0" . 50)
		 ("#9A8F21" . 60)
		 ("#A75B00" . 70)
		 ("#F309DF" . 85)
		 ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-archives
	 (quote
		(("gnu" . "http://elpa.gnu.org/packages/")
		 ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages
	 (quote
		(auctex-latexmk magit org-bullets el2org auctex pdf-tools zenburn-theme monokai-theme gruvbox-theme subatomic-theme haskell-mode)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#F92672")
		 (40 . "#CF4F1F")
		 (60 . "#C26C0F")
		 (80 . "#E6DB74")
		 (100 . "#AB8C00")
		 (120 . "#A18F00")
		 (140 . "#989200")
		 (160 . "#8E9500")
		 (180 . "#A6E22E")
		 (200 . "#729A1E")
		 (220 . "#609C3C")
		 (240 . "#4E9D5B")
		 (260 . "#3C9F79")
		 (280 . "#A1EFE4")
		 (300 . "#299BA6")
		 (320 . "#2896B5")
		 (340 . "#2790C3")
		 (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
