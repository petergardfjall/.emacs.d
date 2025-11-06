;;; package --- VCS-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; A Git porcelain inside Emacs.
(use-package magit
  :straight t
  ;; Lazily load when called for.
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all))


;; Highlight diffs (in the fringe) for version-controlled buffers.
(use-package diff-hl
  :straight t
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :diminish
  :config
  ;; Refresh highlighting if magit does an update.
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package git-when
  ;; :load-path "~/dev/git/emacs-git-when"
  :straight (git-when
             :type git :host github
             :repo "petergardfjall/emacs-git-when"
             :branch "wip"))



(provide 'my-version-control)
;;; my-version-control.el ends here.
