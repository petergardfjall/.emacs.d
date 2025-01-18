;;; package --- Navigation-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Built-in `browse-url' package.
(use-package browse-url
  :straight (:type built-in)
  :commands (browse-url browse-url-xdg-open)
  ;; "URL open"
  :bind (("C-c u o" . browse-url-xdg-open)))


;; Supports definition of hydras - families of commands with a common prefix.
(use-package hydra
  :straight t
  :commands (defhydra)
  :config
  ;; Window navigation/resizing hydra.
  (defhydra hydra-windows (:hint nil)
    "
windmove: ← → ↑ ↓      resize: shift + {↤ ⭲ ⭱ ↧}"
    ("<left>"    windmove-left)
    ("<right>"   windmove-right)
    ("<up>"      windmove-up)
    ("<down>"    windmove-down)
    ("S-<left>"  shrink-window-horizontally)
    ("S-<right>" enlarge-window-horizontally)
    ("S-<up>"    enlarge-window)
    ("S-<down>"  shrink-window)
    ("q"         nil))
  (define-key global-map (kbd "C-c C-w") 'hydra-windows/body))


;; Leave trace points to navigate back and forth along important code paths.
(use-package postrace
  ;; :load-path "~/dev/git/emacs-postrace"
  :straight (postrace :type git :host github
                      :repo "petergardfjall/emacs-postrace"
                      :branch "main")
  ;; Lazily load when called for.
  :bind (("C-c p p" . postrace-push)
	 ("C-c p b" . postrace-browse)))


;; Visualized undo tree.
(use-package vundo
  :straight t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  ;; Display visual undo tree.
  (global-set-key (kbd "C-c u t") #'vundo)
  (let ((m vundo-mode-map))
    (define-key m (kbd "d") #'vundo-diff)
    (define-key m (kbd "C-x u") #'undo)))


(provide 'my-navigation)
;;; my-navigation.el ends here.
