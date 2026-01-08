;;; -*- lexical-binding: t; -*-

(require 'package)

(package-initialize)

(unless (package-installed-p 'use-package) (package-install 'use-package))

(dolist
  (item (file-expand-wildcards (expand-file-name "site-lisp/*" (file-name-directory load-file-name))))
  (add-to-list 'load-path item))
(dolist
  (item (file-expand-wildcards (expand-file-name "site-lisp/corfu/*" (file-name-directory load-file-name))))
  (add-to-list 'load-path item))
(dolist
  (item (file-expand-wildcards (expand-file-name "site-lisp/magit/*" (file-name-directory load-file-name))))
  (add-to-list 'load-path item))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(use-package diminish
  :load-path "diminish.el")

(load-file (expand-file-name "use-emacs.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-package.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-app.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-minad.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "use-eglot.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "defunk.el" (file-name-directory load-file-name)))

(load-file custom-file)
