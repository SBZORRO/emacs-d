;; Enable vertico
(use-package vertico
  :ensure t
  :load-path "site-lisp"
  ;; :bind
  ;; (:map vertico-map
  ;;   ("TAB" . #'minibuffer-complete))
  :init
  (vertico-mode))

;; (use-package vertico-directory
;;   :after vertico
;;   :bind (:map vertico-map
;;           ("M-DEL" . vertico-directory-delete-word)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :load-path "site-lisp"
  :init
  (marginalia-mode))
