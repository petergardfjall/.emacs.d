;;; package --- Assorted buffer-related functions.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(defun my-byte-offset ()
  "Report the byte offset (0-indexed) at point (cursor position)."
  (interactive)
  (message "byte offset: %d" (1- (position-bytes (point)))))

(defun my-sha256-checksum ()
  "Copies the checksum for the selected region to the clipboard.
The cursor position itself is not included in the selection."
  (interactive)
  (unless (region-active-p)
    (error "You need to select a region of text"))
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
         (start-byte (1- (position-bytes (region-beginning)))) ;; 0-indexed
         (end-byte (1- (position-bytes (1- (region-end)))))    ;; 0-indexed
         (checksum (secure-hash 'sha256 text)))
    (clipboard-kill-ring-save nil nil t) ;; copy region to clipboard.
    (message "[%d,%d] checksum: %s" start-byte end-byte checksum)))

(defun my-rename-file-and-buffer ()
  "Rename the current buffer and the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (let ((new-name (read-file-name "Rename file: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun my-strip-on-save-hook ()
  "Register a buffer-local `before-save-hook' to run `delete-trailing-whitespace'."
  ;; note: last argument makes this save-hook local to the buffer
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(defun my-untabify-buffer ()
  "Run `untabify' on the whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun my-untabify-on-save-hook ()
  "Register a buffer-local `before-save-hook' to `untabify' entire buffer."
  ;; note: last argument makes this save-hook local to the buffer
  (add-hook 'before-save-hook #'my-untabify-buffer nil t))

(provide 'my-buffer-functions)
;;; my-buffer-functions.el ends here.
