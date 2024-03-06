(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
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
       (python-mode . python-ts-mode))))

(setq treesit-language-source-alist
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

(use-package magit
  :ensure t)

(use-package geiser-guile
  :ensure t
  :init
  (add-hook 'scheme-mode-hook 'geiser-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;; Markup languages support
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
          ("\\.erb\\'" . web-mode)
          ("\\.hbs\\'" . web-mode)
          ("\\.vue\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :preface
  (defun jekyll-insert-image-url ()
    (interactive)
    (let* ((files (directory-files "../assets/images"))
            (selected-file (completing-read "Select image: " files nil t)))
      (insert (format "![%s](/assets/images/%s)" selected-file selected-file))))

  (defun jekyll-insert-post-url ()
    (interactive)
    (let* ((project-root (projectile-project-root))
            (posts-dir (expand-file-name "_posts" project-root))
            (default-directory posts-dir))
      (let* ((files (remove "." (mapcar #'file-name-sans-extension (directory-files "."))))
              (selected-file (completing-read "Select article: " files nil t)))
        (insert (format "{%% post_url %s %%}" selected-file))))))

(use-package ggtags :ensure t)
;; (dolist (map (list ggtags-mode-map dired-mode-map))
;;   (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
;;   (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
;;   (define-key map (kbd "C-c g r") 'ggtags-find-reference)
;;   (define-key map (kbd "C-c g f") 'ggtags-find-file)
;;   (define-key map (kbd "C-c g c") 'ggtags-create-tags)
;;   (define-key map (kbd "C-c g u") 'ggtags-update-tags)
;;   (define-key map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;;   (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
;;   (define-key map (kbd "M-,") 'pop-tag-mark)
;;   (define-key map (kbd "C-c <") 'ggtags-prev-mark)
;;   (define-key map (kbd "C-c >") 'ggtags-next-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;; eglot and dependency
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure
  :hook
  ;;  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  ((c-ts-mode c++-mode c-mode cc-mode) . eglot-ensure)
  :custom
  (eglot-send-changes-idle-time 0.1)
  :config
  ;;  (fset #'jsonrpc--log-event #'ignore)
  ;;  (setq eglot-report-progress nil)
  (add-to-list 'eglot-server-programs
    '((c-ts-mode c++-mode c-mode cc-mode)
       "clangd"
       "-j=2"
       "--malloc-trim"
       "--background-index"
       "--clang-tidy"
       "--completion-style=bundled"
       "--pch-storage=disk"
       "--header-insertion=iwyu"
       "--header-insertion-decorators"
       "--query-driver=/usr/bin/**")))
;; (use-package projectile
;;   :ensure t
;;   :init
;;   ;;  (setq projectile-project-search-path '("~/git-repo/" "~/svn-repo/"))
;;   (setq projectile-enable-caching t)
;;   :config
;;   (projectile-mode +1))

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-idle-delay 0.5)
;;   (setq company-show-numbers t)
;;   (setq company-tooltip-limit 10)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-tooltip-align-annotations t)
;;   ;; invert the navigation direction if the the completion popup-isearch-match
;;   ;; is displayed on top (happens near the bottom of windows)
;;   (setq company-tooltip-flip-when-above t)
;;   (global-company-mode))

;; (use-package company-posframe
;;   :ensure t
;;   :config (company-posframe-mode t))

;; ;; company-c-headers
;; (use-package company-c-headers
;;   :init
;;   (add-to-list 'company-backends 'company-c-headers))

