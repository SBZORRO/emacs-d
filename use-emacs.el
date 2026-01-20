;;; -*- lexical-binding: t; -*-

(use-package emacs
  ;; :bind* ("<C-return>" . other-window)
  :custom
  ;; C source code
  (auto-hscroll-mode 'current-line)
  (auto-save-interval 64)
  (auto-save-timeout 2)
  (history-length 2000)
  (load-prefer-newer t)
  (message-log-max 16384)
  (undo-limit 100000000)
  (undo-strong-limit 100000000)
  (user-full-name "SBZORRO")
  (visible-bell t)
  (menu-bar-mode nil)
  (fill-column 78)
  (display-line-numbers-width 3)
  (truncate-lines nil)
  (tab-width 2)
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; startup.el
  ;; (auto-save-list-file-prefix (user-data "auto-save-list/.saves-"))
  (inhibit-startup-echo-area-message "sbzorro")
  (inhibit-startup-screen t)
  (inhibit-splash-screen t)
  (initial-buffer-choice t)
  (initial-major-mode 'fundamental-mode)
  ;; (initial-scratch-message "")
  (user-mail-address "sbzorro@gmail.com")

  ;; cus-edit.el
  (custom-file "~/.emacs")

  ;; files.el
  (auto-save-file-name-transforms '((".*" "~/.local/share/emacs/autosaves/" t)))
  (backup-directory-alist '(("." . "~/.local/share/emacs/backups/")))
  (delete-old-versions t)
  (directory-abbrev-alist
   '(("\\`/org" . "~/org")))
  (large-file-warning-threshold nil)
  (save-abbrevs 'silently)
  (version-control t)
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (cpp-mode . cpp-ts-mode)
     (elisp-mode . elisp-ts-mode)
     (java-mode . java-ts-mode)
     (make-mode . cmake-ts-mode)
     (markdown-mode . markdown-ts-mode)
     (rust-mode . rust-ts-mode)
     (markdown-mode . markdown-ts-mode)
     (yaml-mode . yaml-ts-mode)
     (html-mode . html-ts-mode)
     (javascript-mode . javascript-ts-mode)
     (js2-mode . js-ts-mode)
     (js-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (json-mode . json-ts-mode)
     (css-mode . css-ts-mode)
     (python-mode . python-ts-mode)))

  ;; simple.el
  (backward-delete-char-untabify-method 'untabify)
  (kill-do-not-save-duplicates t)
  (mail-user-agent 'gnus-user-agent)
  (next-line-add-newlines nil)
  (save-interprogram-paste-before-kill t)
  (blink-matching-paren-highlight-offscreen t)

  (column-number-mode t)
  (line-number-mode t)
  (indent-tabs-mode nil)
  (kill-ring-max 500)
  (kill-whole-line t)
  (mark-ring-max 5000)
  (mode-require-final-newline t)
  (completion-auto-select 'second-tab)

  ;; display-fill-column-indicator.el
  (global-display-fill-column-indicator-mode t)
  ;; display-line-numbers.el
  (global-display-line-numbers-mode t)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; text-mode.el
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; bytecomp.el
  (byte-compile-verbose nil)

  ;; scroll-bar.el
  (scroll-bar-mode nil)

  ;; paragraphs.el
  ;; (sentence-end-double-space nil)

  ;; paren.el
  (show-paren-delay 0)
  (show-paren-context-when-offscreen t)

  ;; mouse.el
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)

  ;; help.el
  ;; (help-window-keep-selected t)

  ;; warnings.el
  (warning-minimum-log-level :error)

  ;; compile.el
  (compilation-scroll-output t)
  (compile-command "")

  ;; frame.el
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  (frame-background-mode 'dark)

  ;; minibuffer.el
  (completion-auto-help nil)
  ;; (completion-cycle-threshold t)
  (completions-detailed t)
  ;; (completions-max-height 10)
  ;; (completions-format 'horizontal)
  ;; (completions-group t)
  ;; (completion-category-overrides '((file (styles partial-completion))))
  ;; (completion-category-defaults nil) ;; Disable defaults, use our settings
  ;; (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring

  ;; icomplete.el
  ;; (icomplete-mode t)
  ;; (icomplete-vertical-mode t)
  ;; (icomplete-delay-completions-threshold 0)
  ;; (icomplete-compute-delay 0)
  ;; (icomplete-show-matches-on-no-input t)
  ;; (icomplete-with-completion-tables t)
  ;; (icomplete-in-buffer nil)
  ;; (icomplete-max-delay-chars 0)
  ;; (icomplete-scroll t)
  ;; (icomplete-hide-common-prefix t)
  ;; (icomplete-matches-format nil)
  ;; (bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
  ;; (bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)
  ;; (advice-add 'completion-at-point
  ;;             :after #'minibuffer-hide-completions)

  ;; winner.el
  (winner-mode t)

  ;; delsel.el
  (delete-selection-mode t)

  ;; indent.el
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent

  ;; emacs source code
  (source-directory
   (concat (file-name-parent-directory load-file-name) "emacs")) ; C
  (find-function-C-source-directory
   (concat (file-name-parent-directory load-file-name) "emacs/src")) ; find-func.el

  ;; gdb-mi.el
  (gdb-many-windows t)
  (gdb-show-main t)

  ;; :init

  :config
  ;; (add-hook 'after-save-hook
  ;;   #'executable-make-buffer-file-executable-if-script-p)
  ;; Nice line wrapping when working with text
  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  ) ;; end use-package emacs

;;
;;; build-in packages
;;
(use-package lisp-mode
  :config
  (setopt lisp-indent-offset nil))

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
  :diminish
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :diminish
  :config
  (which-key-mode +1))

(use-package whitespace
  :diminish
  :custom
  (whitespace-line-column 78)
  (whitespace-style
   '(face
     indentation
     ;; indentation::space
     ;; indentation::tab
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
  :config
  (global-whitespace-mode 1))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; highlight the current line
(use-package hl-line
  :custom-face
  (hl-line ((t (:background "grey11"))))
  :config
  (global-hl-line-mode +1))

;;; Persist history
(use-package savehist
  :init
  (savehist-mode))

(use-package eldoc
  :diminish
  :hook (prog-mode . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p 3)
  (eldoc-echo-area-display-truncation-message nil))

(use-package window
  :custom
  (switch-to-buffer-preserve-window-point t)
  (window-sides-vertical t)
  (display-buffer-alist
   `(("\\*\\(Help\\|compilation\\)\\*"
      ;; "^\\*.*\\*"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected)
      (side . bottom)
      (window-height . 0.3)
      ;; (window-parameters
      ;;   (select . t)
      ;;   (quit . t)
      ;;   (popup . t)
      ;;   (mode-line-format . none)
      ;;   (no-other-window . t)
      ;;   )
      )))
  )

(use-package treesit
  :config
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
            )))

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-ts-mode))

(use-package hideshow
  :diminish 'hs-minor-mode)
(use-package isearch
  :diminish)
(use-package autorevert
  :diminish 'auto-revert-mode)

(use-package package-vc
  :custom
  (package-vc-allow-build-commands t))

(use-package repeat
  :config
  (repeat-mode))
