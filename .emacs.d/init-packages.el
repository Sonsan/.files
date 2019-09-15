;; ~/.emacs.d/init-packages.el
;; Installs the packages that I need
;; Author: Nils Sterz
;; Last changed: 09.15.2019


(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(setq package-list
			'(zenburn-theme pdf-tools org-bullets magit haskell-mode el2org auctex auctex-latexmk))

(package-initialize)

;; fetch list of available packages 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
