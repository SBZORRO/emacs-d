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
  (redisplay-dont-pause t)
  (undo-limit 800000)
  (user-full-name "SBZORRO")
  (visible-bell t)
  (x-stretch-cursor t)

  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (fill-column 78)
  (enable-recursive-minibuffers t)
  (display-line-numbers-width 3)
  (truncate-lines 1)
  (tab-width 2)
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (completion-ignore-case 1)

  ;; ;; Settings for the Cocoa port
  ;; (ns-alternate-modifier 'alt)
  ;; (ns-command-modifier 'meta)
  ;; (ns-function-modifier 'hyper)
  ;; (ns-right-alternate-modifier 'alt)

  ;; ;; Settings for the Emacs Mac-port
  ;; (mac-command-modifier 'meta)
  ;; (mac-option-modifier 'alt)
  ;; (mac-pass-command-to-system nil)

  (frame-title-format
    '(:eval
       (concat
         (if buffer-file-name default-directory "%b")
         "    "
         (number-to-string
           (cdr
             (assq 'width
               (frame-parameters))))
         "x"
         (number-to-string
           (cdr
             (assq 'height
               (frame-parameters)))))))

  (completion-ignored-extensions
    '(".a"
       ".aux"
       ".bbl"
       ".bin"
       ".blg"
       ".class"
       ".cp"
       ".cps"
       ".elc"
       ".fmt"
       ".fn"
       ".fns"
       ".git/"
       ".glo"
       ".glob"
       ".gmo"
       ".hg/"
       ".idx"
       ".ky"
       ".kys"
       ".la"
       ".lib"
       ".ln"
       ".lo"
       ".lof"
       ".lot"
       ".mem"
       ".mo"
       ".o"
       ".pg"
       ".pgs"
       ".pyc"
       ".pyo"
       ".so"
       ".tfm"
       ".toc"
       ".tp"
       ".tps"
       ".v.d"
       ".vio"
       ".vo" ".vok" ".vos"
       ".vr"
       ".vrs"
       "~"))

  ;; startup.el
  ;; (auto-save-list-file-prefix (user-data "auto-save-list/.saves-"))
  (inhibit-startup-echo-area-message "sbzorro")
  (inhibit-startup-screen t)
  (inhibit-splash-screen t)
  (initial-buffer-choice t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (user-mail-address "sbzorro@gmail.com")

  
  ;; advice.el
  (ad-redefinition-action 'accept)

  ;; files.el
  (auto-save-file-name-transforms '(("\\`/[^/]*:.*" "/tmp" t)))
  (backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
  (delete-old-versions t)
  (directory-abbrev-alist
    '(("\\`/org" . "~/org")))
  (directory-free-space-args "-kh")
  (large-file-warning-threshold nil)
  (save-abbrevs 'silently)
  (trash-directory "~/.Trash")
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
  ;; (setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

  ;; bytecomp.el
  (byte-compile-verbose nil)

  ;; (custom-buffer-done-function 'kill-buffer)
  ;; (default-major-mode 'text-mode)

  ;; prog-mode.el
  (prettify-symbols-unprettify-at-point 'right-edge)

  ;; scroll-bar.el
  (scroll-bar-mode nil)

  ;; paragraphs.el
  (sentence-end-double-space nil)

  ;; paren.el
  (show-paren-delay 0)

  ;; window.el
  (same-window-buffer-names
    '("*eshell*"
       "*shell*"
       "*mail*"
       "*inferior-lisp*"
       "*ielm*"
       "*scheme*"))
  (switch-to-buffer-preserve-window-point t)

  ;; warnings.el
  (warning-minimum-log-level :error)

  ;; frame.el
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  (frame-background-mode 'dark)

  ;; nsm.el
  ;; (nsm-settings-file (user-data "network-security.data"))

  ;; minibuffer.el
  (completion-cycle-threshold 1)                  ; TAB cycles candidates
  (completions-detailed t)                        ; Show annotations
  (completion-styles '(basic initials substring)) ; Different styles to match input to candidates
  (completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (completions-max-height 20)                     ; This is arbitrary
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)

  ;; icomplete.el
  (icomplete-delay-completions-threshold 4000)
  ;;(icomplete-vertical-mode t)

  ;; winner.el
  (winner-mode t)

  ;; delsel.el
  (delete-selection-mode t)

  ;; indent.el
  ;; (setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent

  ;; emacs source code
  (source-directory
    (concat (file-name-parent-directory load-file-name) "emacs")) ; C
  (find-function-C-source-directory
    (concat (file-name-parent-directory load-file-name) "emacs/src")) ; find-func.el

  ;; gdb-mi.el
  (gdb-many-windows t)
  (gdb-show-main t)

  ;; :custom-face
  ;; (cursor ((t (:background "hotpink"))))
  ;; (highlight ((t (:background "blue4"))))
  ;; (minibuffer-prompt ((t (:foreground "grey80"))))
  ;; (mode-line-inactive ((t (:background "grey50"))))
  ;; (nobreak-space ((t nil)))
  ;; (variable-pitch ((t (:height 1.2 :family "Bookerly"))))

  ;; :init
  ;; (setq disabled-command-function nil) ;; enable all commands

  ;; :config
  ;; (add-hook 'after-save-hook
  ;;   #'executable-make-buffer-file-executable-if-script-p)

  ;; (define-key input-decode-map [?\C-m] [C-m])

  ;; ;; Setup keymaps that are bound into by many declarations below.

  ;; (eval-and-compile
  ;;   (mapc #'(lambda (entry)
  ;;             (define-prefix-command (cdr entry))
  ;;             (bind-key (car entry) (cdr entry)))
  ;;     '(("C-,"   . my-ctrl-comma-map)
  ;;        ("<C-m>" . my-ctrl-m-map)
  ;;        ("C-h e" . my-emacs-lisp-help-map)
  ;;        ("C-c b" . my-bookmarks-bibliography-map)
  ;;        ("C-c e" . my-emacs-lisp-map)
  ;;        ("C-c m" . my-ctrl-c-m-map)
  ;;        ("C-c n" . my-org-roam-map)
  ;;        ("C-c t" . my-multi-term-map)
  ;;        ("C-c w" . my-web-map)
  ;;        ("C-c y" . my-yasnippet-map)
  ;;        ("C-c H" . my-highlight-map)
  ;;        ("C-c N" . my-ctrl-c-N-map))))
  )
