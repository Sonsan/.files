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
