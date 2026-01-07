;; App-Launcher
;; The 'app-launcher' is a better run launcher since it reads the desktop applications on your system and you can search them by their names as defined in their desktop file.  This means that sometimes you have to search for a generic term rather than the actual binary command of the program.
(use-package app-launcher
  :load-path "site-lisp"
  :config
  ;; (defun emacs-run-launcher ()
  ;;   "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  ;;   (interactive)
  ;;   (with-selected-frame
  ;;     (make-frame '((name . "emacs-run-launcher")
  ;;                    (minibuffer . only)
  ;;                    (fullscreen . nil) ; no fullscreen
  ;;                    (undecorated . t) ; remove title bar
  ;;                    ;;(auto-raise . t) ; focus on this frame
  ;;                    ;;(tool-bar-lines . nil)
  ;;                    ;;(menu-bar-lines . nil)
  ;;                    (internal-border-width . 10)
  ;;                    (width . 80)
  ;;                    (height . 11)))
  ;;     (unwind-protect
  ;;       (app-launcher-run-app)
  ;;       (delete-frame))
  ;;     ))
  (defun emacs-run-launcher ()
    "Run app launcher in a temporary minibuffer-only frame, then delete both frames."
    (interactive)
    (let ((client-frame (selected-frame))   ; emacsclient -c 创建的（你现在是隐藏的）
           launcher-frame)
      (setq launcher-frame
        (make-frame '((name . "emacs-run-launcher")
                       (minibuffer . only)
                       (fullscreen . nil)
                       (undecorated . t)
                       (internal-border-width . 10)
                       (width . 80)
                       (height . 11))))
      (select-frame-set-input-focus launcher-frame)
      (unwind-protect
        (app-launcher-run-app)
        (keyboard-quit)(keyboard-quit)
        (when (frame-live-p launcher-frame) (delete-frame launcher-frame t))
        (when (frame-live-p client-frame)   (delete-frame client-frame t)))))

  ;; (defun emacs-run-launcher ()
  ;;   "Create a minibuffer-only frame, run app-launcher-run-app, then delete the frame."
  ;;   (interactive)
  ;;   (let ((frame (make-frame
  ;;                  '((name . "emacs-run-launcher")
  ;;                     (minibuffer . only)
  ;;                     (fullscreen . nil)
  ;;                     (undecorated . t)
  ;;                     ;;(auto-raise . t) ; focus on this frame
  ;;                     ;;(tool-bar-lines . nil)
  ;;                     ;;(menu-bar-lines . nil)
  ;;                     (internal-border-width . 10)
  ;;                     (width . 80)
  ;;                     (height . 11)))))
  ;;     (select-frame-set-input-focus frame)
  ;;     (unwind-protect
  ;;       (with-selected-frame frame
  ;;         (app-launcher-run-app))
  ;;       (when (frame-live-p frame)
  ;;         (delete-frame frame t)))))
                                        ; t = 不要确认


  ;; create a global keyboard shortcut with the following code
  ;; emacsclient -cF "((visibility . nil))" -e "(emacs-run-launcher)"
  )
