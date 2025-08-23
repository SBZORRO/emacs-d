;;; init.el --- Minimal Java + Eglot config -*- lexical-binding: t; -*-

;; 包管理 (如果你还没用 use-package)
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

;; ---- 补全 (Corfu) ----
(use-package corfu
  :init
  (global-corfu-mode))

;; ---- 语法高亮/语义增强 ----
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :config
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode)))

;; ---- 格式化保存 ----
(defun my/java-eglot-format-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'java-mode-hook #'my/java-eglot-format-on-save)

(provide 'init)
;;; init.el ends here
