;; This is a collection of functions implemented with elisp

;; allow normal use of TAB
(defun myTab ()
  "Insert tab char. (ASCII 2, \t)"
  (interactive)
  (insert "\t"))

(global-set-key (kbd "TAB") 'myTab) ; same as Ctrl+i

;; set the default widths
(setq-default tab-width 2)


;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)
