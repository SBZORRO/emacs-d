(use-package helm
  :ensure t
  :init

  (setq
   helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
   helm-quick-update t ; do not display invisible candidates
   helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
   helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
   helm-echo-input-in-header-line t
   ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
   ;; helm-ff-file-name-history-use-recentf t
   helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
   helm-buffer-skip-remote-checking t
   helm-mode-fuzzy-match t
   helm-M-x-fuzzy-match t
   ;;   helm-recentf-fuzzy-match t
   helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
   helm-org-headings-fontify t
   helm-find-files-sort-directories t
   ido-use-virtual-buffers t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-lisp-fuzzy-completion t
   ;; helm-apropos-fuzzy-match t
   helm-locate-fuzzy-match t
   helm-display-header-line nil)

  ;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;;; Save current position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

  ;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  ;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
  ;; (global-unset-key (kbd "C-x c"))

  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
  ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  ;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-b") 'helm-mini)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c r") 'helm-recentf)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "C-c h x") 'helm-register)
  ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

  ;; (define-key 'help-command (kbd "C-f") 'helm-apropos)
  ;; (define-key 'help-command (kbd "r") 'helm-info-emacs)
  ;; (define-key 'help-command (kbd "C-l") 'helm-locate-library)

  ;; show minibuffer history with Helm
  (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
  (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

  ;; (define-key global-map [remap find-tag] 'helm-etags-select)
  ;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
  :config (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-hmethod 'alien))

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
;;(setq helm-gtags-prefix-key "\C-cg")

;; (use-package helm-gtags
;;   :init
;;   (progn
;;     (setq helm-gtags-ignore-case t
;;           helm-gtags-auto-update t
;;           helm-gtags-use-input-at-cursor t
;;           helm-gtags-pulse-at-cursor t
;;           helm-gtags-prefix-key "\C-cg"
;;           helm-gtags-suggested-key-mapping t)

;;     ;; Enable helm-gtags-mode in Dired so you can jump to any tag
;;     ;; when navigate project tree with Dired
;;     (add-hook 'dired-mode-hook 'helm-gtags-mode)

;;     ;; Enable helm-gtags-mode in Eshell for the same reason as above
;;     (add-hook 'eshell-mode-hook 'helm-gtags-mode)

;;     ;; Enable helm-gtags-mode in languages that GNU Global supports
;;     (add-hook 'c-mode-hook 'helm-gtags-mode)
;;     (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;     (add-hook 'java-mode-hook 'helm-gtags-mode)
;;     (add-hook 'asm-mode-hook 'helm-gtags-mode)

;;     ;; key bindings
;;     (with-eval-after-load 'helm-gtags
;;       (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;;       (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
;;       (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;;       (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;;       (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;       (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

