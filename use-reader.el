;; Key Bindings

;;     n for going to next page
;;     p for going to previous page
;;     C-n for scrolling down.
;;     C-p for scrolling up.
;;     C-b for scrolling left.
;;     C-f for scrolling right.
;;     Q for closing the EMuReader buffer.
;;     M-< for going to the first page.
;;     M-> for going to the last page.
;;     M-g g for going to a particular page.
;;     M-v or Page Up for scrolling to the top of the page.
;;     C-v or Page Down for scrolling to the end of the page.
;;     SPC, S-SPC , and DEL make the above two commands keep scrolling the pages.
;;     =, +, and C-<wheel-up> for zooming into the page.
;;     - and C-<wheel-down> for zooming out of the page.
;;     H to make the page fit the height of the current window.
;;     W to make the page fit the width of the current window.

;; You also need to build the dynamic module, for this we provide the M-x emureader-build RET. But you can also run make directly yourself.
(use-package emureader)
