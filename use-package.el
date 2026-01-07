(use-package rainbow-delimiters
  :load-path "rainbow-delimiters"
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package modus-themes
  :load-path "modus-themes"
  :config
  (load-theme 'modus-vivendi-tinted))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light)
;;   (set-face-foreground 'font-lock-comment-delimiter-face "red"))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :load-path "orderless"
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package yasnippet
  :load-path "yasnippet"
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

(use-package gnuplot
  :load-path "gnuplot"
  :mode ("\\.gp\\'" . gnuplot-mode))

;; (use-package nano-modeline
;;   :init
;;   :custom
;;   (nano-modeline-position 'nano-modeline-footer)
;;   :config
;;   (setq-default mode-line-format nil)
;;   (setq mode-line-format nil)
;;   (nano-modeline-text-mode t)
;;   (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;;   (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;;   (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;;   (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
;;   (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
;;   (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
;;   (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
;;   (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
;;   (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
;;   (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;;   (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;;   (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;;   (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))

;; (use-package reader
;;   :vc t
;;   :load-path  "site-lisp/emacs-reader/"
;;   )

;; (use-package reader
;;   :vc (:url "https://codeberg.org/divyaranjan/emacs-reader"
;;         :make "all"))

(use-package xdg-launcher
  :load-path "xdg-launcher")

