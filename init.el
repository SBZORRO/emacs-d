(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)


;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 1337 5))

;;(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(load-file (expand-file-name "general.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "base-emacs.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "base-package.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "minibuffer.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "prog-proj.el" (file-name-directory load-file-name)))
;; (load-file (expand-file-name "helm.el" (file-name-directory load-file-name)))
;; (add-to-list 'load-path (concat (file-name-directory load-file-name) "extra"))
;; (load-file (expand-file-name "extras/researcher.el" user-emacs-directory))
