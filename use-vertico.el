;; Enable vertico
(use-package vertico
  :load-path "vertico"
  :demand t
  ;; :bind
  ;; (:map vertico-map
  ;;   ("TAB" . #'minibuffer-complete))
  :config
  (vertico-mode))

;; (use-package vertico-directory
;;   :after vertico
;;   :bind (:map vertico-map
;;           ("M-DEL" . vertico-directory-delete-word)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :load-path "marginalia"
  :demand t
  :config
  (marginalia-mode))
