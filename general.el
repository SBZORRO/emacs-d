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

(defun new-previous-line ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line))

(defun new-next-line()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
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

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
          (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
          (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setopt make-backup-file-name-function 'bedrock--backup-file-name)
(setq gc-cons-percentage 0.5
  gc-cons-threshold (* 128 1024 1024))

(setopt inhibit-startup-message t)
(setopt inhibit-splash-screen t)

(setopt menu-bar-mode nil)
(setopt tool-bar-mode nil)
;;(icomplete-vertical-mode t)


(setopt winner-mode t)
(setopt delete-selection-mode t)

(setopt line-number-mode t)
(setopt column-number-mode t)

(setopt indent-tabs-mode nil)


;; Save history of minibuffer
;; (savehist-mode)

(global-display-fill-column-indicator-mode)
(global-display-line-numbers-mode)
(setopt display-line-numbers-width 3)

;; Automatically reread from disk if the underlying file changes
;; (setopt auto-revert-avoid-polling t)
;; ;; Some systems don't do file notifications well; see
;; ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
;; (setopt auto-revert-interval 5)
;; (setopt auto-revert-check-vc-info t)
;; (global-auto-revert-mode)
(global-auto-revert-mode t)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; setup GDB
(setopt gdb-many-windows t)
(setopt gdb-show-main t)

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

(setopt truncate-lines 1)
(setopt tab-width 2)
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt fill-column 79)

;; simple
(setopt kill-ring-max 5000
  kill-whole-line t
  mark-ring-max 5000)
(setopt mode-require-final-newline t)

(setopt scroll-margin 3
  scroll-conservatively 101)
(setopt  scroll-preserve-screen-position t)

;; emacs source code
(setq source-directory
  (concat (file-name-parent-directory load-file-name) "emacs"))
(setq find-function-C-source-directory
  (concat (file-name-parent-directory load-file-name) "emacs/src"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer

(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates
(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 20)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)

(setq completion-auto-select 'second-tab)            ; Much more eager
;;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(setq icomplete-delay-completions-threshold 4000)

(setopt completion-ignore-case 1)
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

(defun new-previous-line ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line))

(defun new-next-line()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
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

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
          (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
          (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setopt make-backup-file-name-function 'bedrock--backup-file-name)
