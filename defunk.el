;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   keybinding/self defined function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-set "C-x 1" 'delete-window-counterclockwise)
(keymap-global-set "M-c" 'compile)
;; (key-translate "C-v" "<control-v>")
(keymap-global-set "M-<DEL>" 'my-backward-kill-same-syntax)
(keymap-global-set "M-d" 'my-forward-kill-same-syntax)
(keymap-global-set "M-o" 'new-previous-line)
(keymap-global-set "C-o" 'new-next-line)
(keymap-global-set "C-a" 'my-go-ahead)
(keymap-global-set "C-M-@" 'my-mark-sexp)
(keymap-global-set "M-p" 'my-prev-line-recenter)
(keymap-global-set "M-n" 'my-next-line-recenter)

(defun delete-window-counterclockwise()
  (interactive)
  (if (eq (delete-other-windows-vertically) nil)
    (progn
      (other-window 1)
      (if (eq (delete-other-windows-vertically) nil)
        (delete-window)
        (other-window 1)))))

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


(defvar paren-list '(40 123 91))        ; ( { [
(defun my-mark-sexp (&optional arg allow-extend)
  (interactive "P\np")
  (if (and
        (not (memq (char-after) paren-list))
        (not (use-region-p)))
    (forward-same-syntax (- 1))
    )
  (mark-sexp arg allow-extend))

(defun my-prev-line-recenter ()
  (interactive)
  (forward-line -1)
  (recenter nil t)
  )
(defun my-next-line-recenter ()
  (interactive)
  (forward-line 1)
  (recenter nil t)
  )
;; (advice-add 'keyboard-quit :before #'vmacs-bury-boring-windows)
