;;; init.el --- Minimal Java + Eglot config -*- lexical-binding: t; -*-

;; ---- Eglot ----
(use-package eglot
  :hook (java-mode . eglot-ensure)
  :config
  ;; 定义 jdtls 启动参数，workspace 独立保存

  (defun my/eglot-jdtls-contact (_interactive project)
  (let* ((root (if project (project-root project) default-directory))
         (workspace (expand-file-name ".jdtls-workspace" root)))
    (list "jdtls" "-data" workspace)))
;; 绑定到 java-mode
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(java-mode . my/eglot-jdtls-contact)))

  (add-to-list 'eglot-server-programs `(java-mode . ,#'my/eglot-jdtls-contact)))


;; ---- 格式化保存 ----
(defun my/java-eglot-format-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'java-mode-hook #'my/java-eglot-format-on-save)

(provide 'init)
;;; init.el ends here
