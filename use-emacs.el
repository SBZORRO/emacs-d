(use-package emacs
  ;; :bind* ("<C-return>" . other-window)
  :custom
  ;; C source code
  (auto-hscroll-mode 'current-line)
  (auto-save-interval 64)
  (auto-save-timeout 2)
  (history-delete-duplicates t)
  (history-length 200)
  (load-prefer-newer t)
  (message-log-max 16384)
  (undo-limit 800000)
  (user-full-name "SBZORRO")
  (visible-bell t)

  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (fill-column 78)
  (enable-recursive-minibuffers t)
  (display-line-numbers-width 3)
  (truncate-lines nil)
  (tab-width 2)
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (completion-ignore-case t)

  ;; startup.el
  ;; (auto-save-list-file-prefix (user-data "auto-save-list/.saves-"))
  (inhibit-startup-echo-area-message "sbzorro")
  (inhibit-startup-screen t)
  (inhibit-splash-screen t)
  (initial-buffer-choice t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (user-mail-address "sbzorro@gmail.com")

  ;; files.el
  (auto-save-file-name-transforms '((".*" "~/.local/share/emacs/autosaves/" t)))
  (backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
  (delete-old-versions t)
  (directory-abbrev-alist
    '(("\\`/org" . "~/org")))
  (large-file-warning-threshold nil)
  (save-abbrevs 'silently)
  (version-control t)
  (major-mode-remap-alist
    '(
       (bash-mode . bash-ts-mode)
       (c-mode . c-ts-mode)
       (c++-mode . c++-ts-mode)
       (cpp-mode . cpp-ts-mode)
       (elisp-mode . elisp-ts-mode)
       (html-mode . html-ts-mode)
       (javascript-mode . javascript-ts-mode)
       (make-mode . cmake-ts-mode)
       (markdown-mode . markdown-ts-mode)
       (rust-mode . rust-ts-mode)
       (markdown-mode . markdown-ts-mode)
       (yaml-mode . yaml-ts-mode)
       (js2-mode . js-ts-mode)
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

  (column-number-mode t)
  (line-number-mode t)
  (indent-tabs-mode nil)
  (kill-ring-max 500)
  (kill-whole-line t)
  (mark-ring-max 5000)
  (mode-require-final-newline t)
  (completion-auto-select 'second-tab)            ; Much more eager
  
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

  ;; window.el
  ;;   (same-window-buffer-names
  ;;  '("*eshell*"
  ;;    "*shell*"
  ;;    "*mail*"
  ;;    "*inferior-lisp*"
  ;;    "*ielm*"
  ;;    "*scheme*"))
  (switch-to-buffer-preserve-window-point t)
  ;; (display-buffer-alist
  ;;   '(("^\\*.*\\*"
  ;;       (display-buffer-reuse-window
  ;;         display-buffer-in-side-window)
  ;;       (side . bottom)
  ;;       (window-parameters
  ;;         (select . t)
  ;;         (quit . t)
  ;;         (popup . t)
  ;;         (mode-line-format . none)
  ;;         (no-other-window . t)
  ;;         )
  ;;       )))
  
  ;; warnings.el
  (warning-minimum-log-level :error)

  ;; frame.el
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  (frame-background-mode 'dark)

  ;; minibuffer.el
  (completion-cycle-threshold 1)                  ; TAB cycles candidates
  (completions-detailed t)                        ; Show annotations
  ;; (completion-styles '(basic initials substring)) ; Different styles to match input to candidates
  (completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (completions-max-height 10)                     ; This is arbitrary
  ;; (completions-format 'horizontal)
  (completions-group t)

  ;; icomplete.el
  (icomplete-mode t)
  ;; (icomplete-delay-completions-threshold 4000)
  ;; (icomplete-vertical-mode t)

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
  )

