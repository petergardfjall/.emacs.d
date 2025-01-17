;;; package --- General settings and editor appearance.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
;; System locale for formatting time values -- ensures that weekdays in the
;; `org-mode' timestamps appear in English.
(setq system-time-locale "C")

;; Set default font.
;; See https://www.freedesktop.org/software/fontconfig/fontconfig-user.html
(set-frame-font (format "%s-%f" my-font my-font-size))
(when (and (display-graphic-p) (> (display-pixel-width) 3000))
  (my-scale-font 'high-dpi))
;; Set increment for `text-scale-adjust' ("C-x C-+", "C-x C--" "C-x C-0").
(setq text-scale-mode-step 1.1)

;; Wrap long lines.
(set-default 'truncate-lines nil)
;; Sets the fill column (where to break paragraphs on M-q).
(setq-default fill-column 80)

;; Entering a character replaces the selected (active) region.
(delete-selection-mode 1)

;; Allow copy/paste to/from system clipboard.
(setq select-enable-clipboard t)
;; Middle mouse button inserts the clipboard (rather than emacs primary).
(global-set-key (kbd "<mouse-2>") #'x-clipboard-yank)
;; Use spaces for indentation.
(setq-default indent-tabs-mode nil)

;; No sudden jumps when cursor moves off top/bottom of screen. If the value is
;; greater than 100, redisplay will never recenter point, but will always
;; scroll just enough text to bring point into view
;; (setq scroll-conservatively 101)

;; Show column in mode-line.
(setq column-number-mode t)
;; Display line numbers in the buffer margin.
(add-hook 'text-mode-hook        #'display-line-numbers-mode)
(add-hook 'prog-mode-hook        #'display-line-numbers-mode)

(defun my-highlight-todos ()
  "Highlight TODO markers in code."
  (font-lock-add-keywords nil '(("\\(TODO\\)" 1 'font-lock-warning-face prepend)) 'append))
(add-hook 'prog-mode-hook #'my-highlight-todos)

;; Highlight the line where cursor is at.
(global-hl-line-mode t)
;; disable current line highlighting while selecting/marking a region
(add-hook 'activate-mark-hook (lambda () (global-hl-line-mode 0)))
(add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode t)))

;; Make yes/no prompts shorter (y/n)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Cursor appearance.
(set-default 'cursor-type 'box)
(blink-cursor-mode 0) ;; No blinking cursor.

;; Set initial frame width (in characters).
(if (display-graphic-p)
    (setq initial-frame-alist `((width . ,my-frame-width)
                                (height . ,my-frame-height))))
;; automatically revert current buffer when visited file changes on disk
(global-auto-revert-mode)

;; Prevent emacs from writing customized settings to .emacs
;; By setting it as a temporary file, we effectively disable it.
;; Any changes become session-local.
(setq custom-file (make-temp-file "emacs-custom"))

;; centralize emacs backups (avoid littering with files ending in `~`).
(setq make-backup-files t    ; yes, we want backups
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;; don't create lockfiles for buffers being edited (`.#<file>`) as it
;; interferes with hot reloading in reactjs.
(setq create-lockfiles nil)

;; Show matching paranthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t)


;; Used to selectively hide mode-line display of certain minor modes. For
;; example: (use-package :diminish).
(use-package diminish
  :straight t
  :demand t)


;; Remove "ElDoc" from modeline.
(use-package eldoc
  :straight (:type built-in)
  :diminish eldoc-mode)


(use-package immaterial-theme
  ;; :load-path "~/dev/git/emacs-immaterial-theme"
  :straight (immaterial-theme
             :type git :host github
             :repo "petergardfjall/emacs-immaterial-theme"
             :branch "master")
  :config
  (load-theme 'immaterial-dark t))


;; Highlights occurences of color codes (in text) with a background of that
;; color. For example, "#aaaaaa" will be displayed with a gray background.
;; Activate via "M-x rainbow-mode".
(use-package rainbow-mode
  :straight t
  :commands (rainbow-mode)
  :diminish
  :config
  ;; don't highlight color words such as "white", "blue"
  (setq rainbow-x-colors nil))


(provide 'my-appearance)
;;; my-appearance.el ends here.
