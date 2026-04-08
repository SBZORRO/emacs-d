;; ** Manual
;; This section is about how to install this package manually. Intended to be used by developers and testers.n

;; After cloning the repository, follow the instructions from the previous section to install dependencies on your respective operating system.

;; Then, you run =make= in the git repository, as noted earlier this may take a few depending on if it is fetching and building =mupdf=.

;; After this, you add the path to emacs-reader git repository to =load-path=,
;; #+begin_src emacs-lisp
;;   (add-to-list 'load-path "/path/to/emacs-reader")
;; #+end_src

;; You can also utilize =use-package= to do the same,
;; #+begin_src emacs-lisp
;;   (use-package reader
;;     :vc t
;;     :load-path "/path/to/emacs-reader")
;; #+end_src

;; To test emacs-reader in a default Emacs config, first generate the auto loads using =make autoloads=.
;; #+begin_src shell
;;   emacs -Q -L . -l reader-autoloads.el
;; #+end_src
;; This command adds the current directory to path, and loads =reader-autoloads.el=.

;; You can also try using =package-vc-install-from-checkout=, [[https://codeberg.org/attachments/2555c252-0977-484e-b369-38b18a321a48][this video]] demonstrates how to do that.

(use-package reader
  )

;; (setq package-vc-allow-build-commands t)
;; (use-package reader
;;   :vc (:url "https://codeberg.org/MonadicSheep/emacs-reader"
;;             :make "all"))
