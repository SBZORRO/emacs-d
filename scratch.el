;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

;; (defun my-create-text-area ()
;;   "Create a floating text area like Office using a child frame."
;;   (interactive)
;;   (let* ((buf (get-buffer-create "*Text Area*"))
;;           (frame ))
;;     (select-frame-set-input-focus frame)
;;     (set-window-buffer (frame-root-window frame) buf)
;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (insert "Type here... (C-g to close)")
;;       (local-set-key (kbd "C-g")
;;         (lambda ()
;;           (interactive)
;;           (delete-frame frame))))))

;; (global-set-key (kbd "C-c t") 'my-create-text-area)


;; (make-frame
;;   `((parent-frame . ,(selected-frame))
;;      (width . 50)
;;      (height . 10)
;;      (minibuffer . nil)
;;      (undecorated . t)
;;      (no-accept-focus . nil)
;;      (border-width . 2)
;;      (internal-border-width . 10)
;;      (background-color . "white")))



;; (defun my-create-text-area ()
;;   "Create a floating text area like Office using a child frame."
;;   (interactive)
;;   (let* ((buf (get-buffer-create "*Text Area*"))
;;           (frame (make-frame
;;                    `((parent-frame . ,(selected-frame))
;;                       (width . 50)
;;                       (height . 10)
;;                       (minibuffer . nil)
;;                       (undecorated . t)
;;                       (no-accept-focus . nil)
;;                       (border-width . 2)
;;                       (internal-border-width . 10)
;;                       (top . ,(line-number-at-pos))
;;                       (left . ,(current-column))
;;                       ))))
;;     (select-frame-set-input-focus frame)
;;     (set-window-buffer (frame-root-window frame) buf)
;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (insert "Type here... (C-g to close)")
;;       (local-set-key (kbd "C-g")
;;         (lambda ()
;;           (interactive)
;;           (delete-frame frame))))))



;; (defun my-cursor-position ()
;;   "Return the current cursor position as (line, column)."
;;   (interactive)
;;   (my-cursor-position-in))
;; (defun my-cursor-position-in ()
;;   (let ((line (line-number-at-pos))
;;          (column (current-column)))
;;     (cons line column)))

















(defvar my-text-box-frame nil
  "The current child frame for the text box.")

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
    (setq my-text-box-frame frame)
    (select-frame-set-input-focus frame)
    (set-window-buffer (frame-root-window frame) buf)
    (with-current-buffer buf
      (erase-buffer)
      (insert "Type here... (C-g to close)")
      (local-set-key (kbd "C-g")
                     (lambda ()
                       (interactive)
                       (delete-frame frame))))))

(defun my-move-text-area (dx dy)
  "Move the text area by DX and DY."
  (interactive "nMove X: \nnMove Y: ")
  (when (frame-live-p my-text-box-frame)
    (let* ((current-left (frame-parameter my-text-box-frame 'left))
           (current-top (frame-parameter my-text-box-frame 'top))
           (new-left (+ current-left dx))
           (new-top (+ current-top dy)))
      (set-frame-position my-text-box-frame new-left new-top))))

(defun my-move-text-area ()
  "Move the text area by DX and DY."
  (interactive)
  (when (frame-live-p my-text-box-frame)
    (let* ((current-left (frame-parameter my-text-box-frame 'left))
           (current-top (frame-parameter my-text-box-frame 'top))
            )
      (set-frame-position my-text-box-frame (1+ current-left) (1+ current-top))
      (print current-left)(print current-top)

      )))

(defun my-resize-text-area (width height)
  "Resize the text area to WIDTH and HEIGHT."
  (interactive "nWidth: \nnHeight: ")
  (when (frame-live-p my-text-box-frame)
    (set-frame-size my-text-box-frame width height)))

(global-set-key (kbd "C-c t") 'my-create-text-area)
(global-set-key (kbd "C-c m") 'my-move-text-area)
(global-set-key (kbd "C-c r") 'my-resize-text-area)
