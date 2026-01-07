(use-package eglot
  :ensure t
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
  :ensure nil
  :load-path "site-lisp"
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
