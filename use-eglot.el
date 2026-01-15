(use-package eglot
  :hook
  ;;  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  ((c-ts-mode c++-ts-mode c++-mode c-mode cc-mode) . eglot-ensure)
  ((vue-mode) . eglot-ensure)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; (setq eglot-report-perogress nil)
  ;; (eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode c++-mode c-mode cc-mode)
                 "clangd"
                 "--log=error"
                 "-j=2"
                 "--malloc-trim"
                 "--pch-storage=memory"
                 "--background-index"
                 "--clang-tidy"
                 "--completion-style=detailed"
                 "--header-insertion=iwyu"
                 "--header-insertion-decorators"
                 "--query-driver=/usr/bin/**"))
  ;; (add-to-list 'eglot-server-programs
  ;;   '((java-mode java-ts-mode)
  ;;      "java"
  ;;      "-Declipse.application=org.eclipse.jdt.ls.core.id1"
  ;;      "-Dosgi.bundles.defaultStartLevel=4"
  ;;      "-Declipse.product=org.eclipse.jdt.ls.core.product"
  ;;      "-Dlog.level=ALL"
  ;;      ;; "-Xmx1G "
  ;;      "--add-modules=ALL-SYSTEM"
  ;;      "--add-opens" "java.base/java.util=ALL-UNNAMED"
  ;;      "--add-opens" "java.base/java.lang=ALL-UNNAMED"
  ;;      "-jar" "/home/sbzorro/jdtls/plugins/org.eclipse.equinox.launcher_1.7.0.v20250519-0528.jar"
  ;;      "-configuration" "/home/sbzorro/jdtls/config_linux"
  ;;      "-data" "/home/sbzorro/jdtls-ws"))
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio"
                             :initializationOptions ,(vue-eglot-init-options))))
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ("pylsp")))
  ;; (add-to-list 'eglot-server-programs
  ;;   '((java-ts-mode) .
  ;;      ("jdtls"
  ;;        :initializationOptions
  ;;        (:bundles ["/home/sbzorro/.m2/repository/com/microsoft/java/com.microsoft.java.debug.plugin/0.53.2/com.microsoft.java.debug.plugin-0.53.2.jar"]))))
  :bind
  (:map eglot-mode-map
        ("C-x f" . eglot-format))
  )


;; 告诉 JDTLS 我们的偏好（会通过 workspace/didChangeConfiguration 发给服务端）
;; (setq-default eglot-workspace-configuration
;;   '(:java
;;      ;; 变更依赖/pom 时自动更新项目（也可 "interactive"/"disabled"）
;;      (:configuration (:updateBuildConfiguration "automatic"))

;;      ;; 启用 Maven 导入（通常默认就启用，这里显式打开）
;;      (:import (:maven (:enabled t)))

;;      ;; Maven 下载源码/文档，补全/跳转更舒服
;;      (:maven (:downloadSources t :updateSnapshots t))

;;      ;; 代码里的一些小增强（可选）
;;      (:referencesCodeLens (:enabled t))
;;      (:signatureHelp (:enabled t))
;;      (:contentProvider (:preferred "fernflower"))))

;; site-lisp/eglot-java/
(use-package eglot-java
  :load-path "eglot-java"
  :hook (java-ts-mode . eglot-java-mode)
  )

;; Vue-mode setup
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

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
              :documentColor t))
      :vue (:hybridMode :json-false)
      )))

(use-package dape
  :load-path "dape"
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  ;; (dape-buffer-window-arrangement 'gud)
  ;; (dape-info-hide-mode-line nil)

  ;; Projectile users
  ;; (dape-cwd-function #'projectile-project-root)

  :config
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook #'kill-buffer)

  (with-eval-after-load 'dape
    (add-to-list
     'dape-configs
     '(gdb-sudo
       modes (c-mode c-ts-mode c++-mode c++-ts-mode hare-mode hare-ts-mode)
       command-cwd dape-command-cwd
       command "sudo"
       command-args ("gdb" "--interpreter=dap")
       :request "launch"
       :program "a.out"
       :args []
       :stopAtBeginningOfMainSubprogram nil))
    )
  )

(use-package web-mode
  :load-path "web-mode"
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.hbs\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package markdown-mode
  :load-path "markdown-mode"
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package gtags-mode
  :load-path "gtags-mode"
  )

(use-package ggtags
  :load-path "ggtags")
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

