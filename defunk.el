;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   keybinding/self defined function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keymap-global-set "M-c" 'compile)
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


(setq vmacs-ignore-buffers
  (list
    "\\` "
    "\*Buffer List\*"
    "\*Helm" "\*helm"
    "*Launch" "\*Warnings\*"
    "\*RE-Builder\*"
    "\*dape-repl\*" "\*dape-info"
    "\*vc-diff\*" "\*magit-" "\*vc-" "\*vc*"
    "*Backtrace*" "*Package-Lint*" "\*Completions\*" "\*Compile-Log\*"
    "\*vc-change-log\*" "\*VC-log\*"
    "\*Async Shell Command\*" "\*Shell Command Output\*"
    "\*lsp" "\*ccls" "\*gopls" "\*bingo" "\*mspyls" "\*EGLOT"
    "\*sdcv\*" "\*tramp"  "\*Gofmt Errors\*"
    "\*Ido Completions\*" "\*Flycheck " "\*Flymake"
    "magit-process" "magit-diff" "magit-stash"))

(defvar boring-window-modes
  '(help-mode compilation-mode log-view-mode log-edit-mode
     gnus-article-mode
     org-agenda-mode magit-revision-mode ibuffer-mode))


(defun vmacs-filter(buf &optional ignore-buffers)
  (cl-find-if
    (lambda (f-or-r)
      (string-match-p f-or-r buf))
    (or ignore-buffers vmacs-ignore-buffers)))



(defun bury-boring-windows(&optional bury-cur-win-if-boring)
  "close boring *Help* windows with `C-g'"
  (let ((opened-windows (window-list))
         (cur-buf-win (get-buffer-window)))
    (dolist (win opened-windows)
      (with-current-buffer (window-buffer win)
        (when (or (memq  major-mode boring-window-modes)
                (vmacs-filter (buffer-name)))
          (when (and (>  (length (window-list)) 1)
                  (not (minibufferp))
                  (or bury-cur-win-if-boring
                    (not (equal cur-buf-win win)))
                  (delete-window win))))))))

(defun vmacs-bury-boring-windows ()
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))
    (minibuffer-keyboard-quit))
  (when (equal last-command 'keyboard-quit)
    (bury-boring-windows)))

(advice-add 'keyboard-quit :before #'vmacs-bury-boring-windows)
