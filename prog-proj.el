(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
    ("\\.erb\\'" . web-mode)
    ("\\.hbs\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

;; Vue-mode setup
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

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
       "--query-driver=/usr/bin/**"))
  (add-to-list 'eglot-server-programs
    `(vue-mode . ("vue-language-server" "--stdio"
                   :initializationOptions ,(vue-eglot-init-options))))
  )

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . gfm-mode)
    ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package magit
  :ensure t)

(use-package geiser-guile
  :ensure t
  :init
  (add-hook 'scheme-mode-hook 'geiser-mode))

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

(defun vue-eglot-init-options ()
  (let ((tsdk-path
          (expand-file-name
            "lib"
            (string-trim
              (shell-command-to-string
                "npm list --global --parseable typescript | head -n1"))
            )))
    `(:typescript
       (:tsdk ,tsdk-path
         :languageFeatures
         (:completion
           (:defaultTagNameCase "both"
             :defaultAttrNameCase "kebabCase"
             :getDocumentNameCasesRequest nil
             :getDocumentSelectionRequest nil)
           :diagnostics
           (:getDocumentVersionRequest nil))
         :documentFeatures
         (:documentFormatting
           (:defaultPrintWidth 100
             :getDocumentPrintWidthRequest nil)
           :documentSymbol t
           :documentColor t)))))
