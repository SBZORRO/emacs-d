;; App-Launcher
;; The 'app-launcher' is a better run launcher since it reads the desktop applications on your system and you can search them by their names as defined in their desktop file.  This means that sometimes you have to search for a generic term rather than the actual binary command of the program.
(use-package app-launcher
  :ensure nil
  :load-path "site-lisp"
  :config
  (defun emacs-run-launcher ()
    "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
    (interactive)
    (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                     (minibuffer . only)
                     (fullscreen . 0) ; no fullscreen
                     (undecorated . t) ; remove title bar
                     ;;(auto-raise . t) ; focus on this frame
                     ;;(tool-bar-lines . 0)
                     ;;(menu-bar-lines . 0)
                     (internal-border-width . 10)
                     (width . 80)
                     (height . 11)))
      (unwind-protect
        (app-launcher-run-app)
        (delete-frame))
      ))

  ;; create a global keyboard shortcut with the following code
  ;; emacsclient -cF "((visibility . nil))" -e "(emacs-run-launcher)"
  )
