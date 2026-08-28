;;; package --- Assorted display-related functions.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(defun my-color-lighten (hex-color percent)
  "Determine a brighter/darker shade of a hex color.
For a HEX-COLOR (such as `#3cb878`) return the hex color that is
PERCENT percent brighter (or darker if percent value is
negative)."
  (interactive "sHex color: \nnPercent brighter/darker (-): ")
  (require 'immaterial-theme)
  (if (fboundp 'immaterial-color-lighten)
      (message "%s" (immaterial-color-lighten hex-color percent))))

(defun my-display-width ()
  "Return the width (in pixels) of the display showing the selected frame."
  (interactive)
  ;; geometry -- Position and size in pixels in the form of (X Y WIDTH HEIGHT)
  (let ((width-pixels (nth 2 (alist-get 'geometry (car (display-monitor-attributes-list))))))
    (when (called-interactively-p 'any)
      (message "Display width is %d pixels." width-pixels))
    width-pixels))

(defun my-reset-size ()
  "Reset the size of the selected frame to its default size."
  (interactive)
  (set-frame-height (selected-frame) my-frame-height)
  (set-frame-width (selected-frame) my-frame-width))

(defun my-resolution-resize ()
  "Resize frame to its default size and scale the font after screen resolution."
  (interactive)
  (defun 4k-resolution-p ()
    (and (display-graphic-p)
         (>= (my-display-width) 3840)))
  (my-reset-size)
  (if (4k-resolution-p)
      (progn
        (message "4K resolution")
        (my-scale-font 'high-dpi))
    (message "low resolution")
    (my-scale-font 'low-dpi)))

(defun my-scale-font (resolution)
  "Scale the frame font according to screen RESOLUTION.
For a RESOLUTION of `high-dpi' the default font is scaled to twice its
size.  For any other RESOLUTION (`low-dpi') the default font is set to
its original size."
  (interactive
   (list (completing-read "Select target resolution: " '(high-dpi low-dpi))))
  (let* ((new-size (if (string= resolution 'high-dpi) (* 2 my-font-size) my-font-size)))
    (set-frame-font (format "%s-%f" my-font new-size))))

(defun my-set-default-font-height ()
  "Reset the font height for the selected frame to the default font size."
  (interactive)
  ;; calculate delta between current face height and default font height and
  ;; apply difference.
  (let* ((font-height (face-attribute 'default :height))
         (default-font-height (truncate (* 10 my-font-size)))
         (delta-to-default (- default-font-height font-height)))
    (default-text-scale-increment delta-to-default)))

(provide 'my-dislpay-functions)
;;; my-display-functions.el ends here.
