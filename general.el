(setq gc-cons-percentage 0.5
  gc-cons-threshold (* 128 1024 1024))

;; Save history of minibuffer
;; (savehist-mode)

;; display-fill-column-indicator.el
(global-display-fill-column-indicator-mode)
;; display-line-numbers.el
(global-display-line-numbers-mode)

;; Automatically reread from disk if the underlying file changes
;; (setopt auto-revert-avoid-polling t)
;; ;; Some systems don't do file notifications well; see
;; ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
;; (setopt auto-revert-interval 5)
;; (setopt auto-revert-check-vc-info t)
;; (global-auto-revert-mode)
;; (global-auto-revert-mode t)


;; ;; Show the tab-bar as soon as tab-bar functions are invoked
;; (setopt tab-bar-show 1)

;; ;; Add the time to the tab-bar, if visible
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
;; (setopt display-time-format "%a %F %T")
;; (setopt display-time-interval 1)
;; (display-time-mode)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; treesit.el
(setopt treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")

     ;; (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
     ;; (go "https://github.com/tree-sitter/tree-sitter-go")
     ;; (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
     ;; (julia "https://github.com/tree-sitter/tree-sitter-julia")
     ;; (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     ;; (scala "https://github.com/tree-sitter/tree-sitter-scala")
     ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
     ))

(defun quickping (host)
  (= 0 (call-process "ping" nil nil nil "-c1" "-W50" "-q" host)))

(defvar saved-window-configuration nil)

(defun push-window-configuration ()
  (interactive)
  (push (current-window-configuration) saved-window-configuration))

(defun pop-window-configuration ()
  (interactive)
  (let ((config (pop saved-window-configuration)))
    (if config
      (set-window-configuration config)
      (if (> (length (window-list)) 1)
        (delete-window)
        (bury-buffer)))))

(add-hook 'after-init-hook #'garbage-collect t)
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
