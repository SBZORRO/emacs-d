(require 'package)

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "site-lisp/vertico/" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "site-lisp/yasnippet/" (file-name-directory load-file-name)))

(package-initialize)

(require 'vertico)
(require 'yasnippet)

;; (package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(load-file (expand-file-name "use-emacs.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-package.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-vertico.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-corfu.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-consult.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "prog-proj.el" (file-name-directory load-file-name)))
;; (load-file (expand-file-name "helm.el" (file-name-directory load-file-name)))
;; (add-to-list 'load-path (concat (file-name-directory load-file-name) "extra"))
;; (load-file (expand-file-name "extras/researcher.el" user-emacs-directory))
(load-file (expand-file-name "defunk.el" (file-name-directory load-file-name)))

(load-file custom-file)
