;;; -*- lexical-binding:t -*-

(defun my-cursor-position ()
  "Return the current cursor position as (line, column)."
  (interactive)
  (my-cursor-position-in))
(defun my-cursor-position-in ()
  (let ((line (line-number-at-pos))
         (column (current-column)))
    (cons line column)))

(defun my-create-text-area ()
  "Create a floating text area like Office using a child frame."
  (interactive)
  (let* ((buf (get-buffer-create "*Text Area*"))
          (frame (make-frame
                   `((parent-frame . ,(selected-frame))
                      (width . 50)
                      (height . 10)
                      (minibuffer . nil)
                      (undecorated . t)
                      (no-accept-focus . nil)
                      (border-width . 2)
                      (internal-border-width . 10)
                      (top . 5)
                      (left . 10)))))
    ;; (setq my-text-box-frame frame)
    (select-frame-set-input-focus frame)
    (set-window-buffer (frame-root-window frame) buf)
    ))

(defun my-nudge-text-area ()
  "Nudge the text area by 1 pixel in both X and Y."
  (interactive)
  (when (frame-live-p (selected-frame))
    (let* ((frame (selected-frame))
            (current-left (frame-parameter frame 'left))
            (current-top (frame-parameter frame 'top)))
      (set-frame-parameter frame 'left (1+ current-left))
      (set-frame-parameter frame 'top (1+ current-top))
      )))

(defun my-resize-text-area (width height)
  "Resize the text area to WIDTH and HEIGHT."
  (interactive "nWidth: \nnHeight: ")
  (when (frame-live-p my-text-box-frame)
    (set-frame-size my-text-box-frame width height)))

(defun my-move-text-area (dx dy)
  "Move the text area by DX and DY."
  (interactive "nMove X: \nnMove Y: ")
  (when (frame-live-p my-text-box-frame)
    (let* ((current-left (frame-parameter my-text-box-frame 'left))
            (current-top (frame-parameter my-text-box-frame 'top))
            (new-left (+ current-left dx))
            (new-top (+ current-top dy)))
      (set-frame-position my-text-box-frame new-left new-top))))

(global-set-key (kbd "C-c t") 'my-create-text-area)
(global-set-key (kbd "C-c m") 'my-move-text-area)
(global-set-key (kbd "C-c n") 'my-nudge-text-area)
(global-set-key (kbd "C-c r") 'my-resize-text-area)

;; (let* ((buf (get-buffer-create "*Text Area*"))
;;         (frame (make-frame
;;                  `((parent-frame . ,(selected-frame))
;;                     (width . 50)
;;                     (height . 10)
;;                     (minibuffer . nil)
;;                     (undecorated . t)
;;                     (no-accept-focus . nil)
;;                     (border-width . 2)
;;                     (internal-border-width . 10)
;;                     (top . ,(line-number-at-pos))
;;                     (left . ,(current-column))
;;                     ))))
;;   (select-frame-set-input-focus frame)
;;   (set-window-buffer (frame-root-window frame) buf)
;;   )
;; (with-current-buffer buf
;;   (erase-buffer)
;;   (insert "Type here... (C-g to close)")
;;   (local-set-key (kbd "C-g")
;;     (lambda ()
;;       (interactive)
;;       (delete-frame frame))))


;; (use-package window
;;   :custom
;;   (switch-to-buffer-preserve-window-point t)
;;   (display-buffer-alist
;;     `(("^\\*.*\\*"
;;         (my-create-text-area)
;;         ;; (window-parameters
;;         ;;   (select . t)
;;         ;;   (quit . t)
;;         ;;   (popup . t)
;;         ;;   (mode-line-format . none)
;;         ;;   (no-other-window . t)
;;         ;;   )
;;         )))
;;   )

;; (defun my-create-text-area ()
;;   "Create a floating text area like Office using a child frame."
;;   (interactive)
;;   (let* ((frame
;;            (make-frame
;;              ;; (append
;;              ;;   alist
;;              `((parent-frame . ,(selected-frame))
;;                 (width . 50)
;;                 (height . 10)
;;                 (minibuffer . nil)
;;                 (undecorated . t)
;;                 (no-accept-focus . nil)
;;                 (border-width . 2)
;;                 (internal-border-width . 10)
;;                 (top . ,(cdr (nth 2 (posn-at-point))))
;;                 (left . ,(car (nth 2 (posn-at-point))))
;;                 )
;;              ;; )
;;              )))
;;     (select-frame-set-input-focus frame)
;;     (local-set-key (kbd "C-g")
;;       (lambda ()
;;         (interactive)
;;         (delete-frame frame)))
;;     ;; (setq my-text-box-frame frame)
;;     ;; (print (with-selected-window (car (window-list))
;;     ;;          (line-number-at-pos (window-point (car (window-list)) ))
;;     ;;          ))
;;     ;; (print (posn-at-point nil (car (window-list))))
;;     ;; (print (posn-at-point))
;;     ;; (set-frame-parameter frame 'top
;;     ;;   (cdr (nth 2 (posn-at-point nil (car (window-list))))))
;;     ;; (set-frame-parameter frame 'top (with-selected-window (car (window-list))
;;     ;;                                   (line-number-at-pos (window-point (car (window-list)) ))
;;     ;;                                   ))
;;     ;; (message  (window-point (car )))
;;     ;; (set-window-buffer (frame-root-window frame) buffer)
;;     ))

(global-set-key (kbd "C-c t") 'eldoc)
(line-number-at-pos)
(count-screen-lines)
(window-start)


;; window.el
;;   (same-window-buffer-names
;;  '("*eshell*"
;;    "*shell*"
;;    "*mail*"
;;    "*inferior-lisp*"
;;    "*ielm*"
;;    "*scheme*"))
