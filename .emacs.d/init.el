;; My Personal Emacs config
;; - Nils

;; set dump directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))


;; load functions
(load "~/.emacs.d/util/init-packages")  ;; installs uninstalled packages on startup
(load "~/.emacs.d/util/movement")
(load "~/.emacs.d/util/latex")
(load "~/.emacs.d/util/prog")
(load "~/.emacs.d/util/style")

;; open on startup
(find-file "~/.emacs.d/init.el")
(find-file "~/.emacs.d/welcome.org")


