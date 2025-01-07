;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   keybinding/self defined function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (key-translate "C-v" "<control-v>")
(keymap-global-set "M-<DEL>" 'my-backward-kill-same-syntax)
(keymap-global-set "M-d" 'my-forward-kill-same-syntax)
(keymap-global-set "M-o" 'new-previous-line)
(keymap-global-set "C-o" 'new-next-line)
(keymap-global-set "C-a" 'my-go-ahead)
(keymap-global-set "C-M-@" 'my-mark-sexp)

(defun my-go-ahead ()
  (interactive)
  (if (= (line-beginning-position) (point))
    (beginning-of-line-text)
    (beginning-of-line)))

(defun new-previous-line ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line))

(defun new-next-line()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (interactive)
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
      (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
      (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my-backward-kill-same-syntax (arg)
  (interactive "p")
  (kill-region (point) (progn (forward-same-syntax (- arg)) (point))))

(defun my-forward-kill-same-syntax (arg)
  (interactive "p")
  (kill-region (point) (progn (forward-same-syntax arg) (point))))

(defun my-mark-sexp (&optional arg allow-extend)
  (interactive "P\np")
  (if (eq (use-region-p) nil)
    (forward-same-syntax (- 1)))
  (mark-sexp arg allow-extend))
