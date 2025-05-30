(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light)
;;   (set-face-foreground 'font-lock-comment-delimiter-face "red"))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package yasnippet
  :ensure t
  :demand t
  :diminish yas-minor-mode
  :commands yas-minor-mode-on
  ;; :bind (("C-c y d" . yas-load-directory)
  ;;        ("C-c y i" . yas-insert-snippet)
  ;;        ("C-c y f" . yas-visit-snippet-file)
  ;;        ("C-c y n" . yas-new-snippet)
  ;;        ("C-c y t" . yas-tryout-snippet)
  ;;        ("C-c y l" . yas-describe-tables)
  ;;        ("C-c y g" . yas-global-mode)
  ;;        ("C-c y m" . yas-minor-mode)
  ;;        ("C-c y r" . yas-reload-all)
  ;;        ("C-c y x" . yas-expand)
  ;;        :map yas-keymap
  ;;        ("C-i" . yas-next-field-or-maybe-expand))
  ;; :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (prog-mode . yas-minor-mode-on)
  :custom
  (yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
  (yas-snippet-dirs '("~/git-repo/emacs-d/site-lisp/yasnippet-snippets/snippets/"))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  ;; :custom-face
  ;; (yas-field-highlight-face ((t (:background "#e4edfc"))))
  ;; :config
  ;; (yas-load-directory (emacs-path "snippets"))
  )
