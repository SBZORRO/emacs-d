;;
;;; build-in packages
;;
(use-package lisp-mode
  :config
  (setopt lisp-indent-offset 2))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
          ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light)
;;   (set-face-foreground 'font-lock-comment-delimiter-face "red"))

(use-package whitespace
  ;; :diminish
  ;; (global-whitespace-mode
  ;;   whitespace-mode
  ;;   whitespace-newline-mode)
  ;; :commands
  ;; (whitespace-buffer
  ;;   whitespace-cleanup
  ;;   whitespace-mode
  ;;   whitespace-turn-off)
  :custom
  (whitespace-auto-cleanup t t)
  (whitespace-line-column 78)
  (whitespace-rescan-timer-time nil t)
  (whitespace-silent t t)
  (whitespace-style
    '(face
       ;; indentation
       indentation::space
       indentation::tab
       tabs
       ;; spaces
       space-after-tab
       space-befer-tab
       trailing
       lines-tail
       newline
       ;; tab-mark
       ;; newline-mark
       ;; space-mark
       ))
  :custom-face
  (whitespace-indentation ((t (:background "grey11"))))
  ;; :defines
  ;; (whitespace-auto-cleanup
  ;;   whitespace-rescan-timer-time
  ;;   whitespace-silent)
  :config
  (global-whitespace-mode 1))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight the current line
(use-package hl-line
  :custom-face
  (hl-line ((t (:background "grey11"))))
  :config
  (global-hl-line-mode +1))
