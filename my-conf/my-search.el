;;; package --- Search-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Incremental buffer search configured to support navigation with up/down key.
(use-package isearch
  :straight (:type built-in)
  ;; Lazily load when called for.
  :bind (("C-S-s" . isearch-forward)
	 ("C-r"   . isearch-backward))
  :config
  (let ((m isearch-mode-map))
    (define-key m (kbd "<up>") #'isearch-repeat-backward)
    (define-key m (kbd "<down>") #'isearch-repeat-forward)))


;; Configure consult to provide `completing-read'-based search with live
;; preview.
(use-package consult
  :straight t
  :bind (("M-g g"   . consult-goto-line) ;; goto-line
	 ;; Search buffer with live preview. Replacement for `isearch'.
	 ("C-s"     . consult-line)
	 ;; "Search git": free-text search in version-controlled files.
	 ("C-c s g" . consult-git-grep)
	 ;; "Search project": free-text search in all project files.
	 ("C-c s p" . consult-ripgrep))
  :init
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Delay before starting a new async search (for example for `consult-grep').
  (setq consult-async-input-debounce 0.2)
  ;; Avoid displaying search results grouped by file (separated by headers).
  (consult-customize
   consult-git-grep consult-ripgrep
   :group nil)
  )


;; The embark-consult provides the `embark-export' command which collects output
;; candidates from `consult-grep' (or `consult-git-grep', `consult-ripgrep')
;; into a buffer where it, for example, can be edited with `wgrep'.
(use-package embark-consult
  :straight t
  :config
  (let ((m minibuffer-mode-map))
    ;; Exports any candidates currently in the minibuffer to a separate buffer.
    (define-key m (kbd "C-c e") #'embark-export)))


;; Makes search results editable by offering in-place editing of grep-mode
;; buffers (like ones produced by `grep' or `embark-export' (together with
;; `consult-grep').
;;
;; For example, run `consult-git-grep', enter a search string, and
;; `embark-export' to get all candidates into a separate buffer. Then "C-c C-p"
;; (`wgrep-change-to-wgrep-mode') makes the buffer editable. Edit the search
;; results and "C-x C-s" to save all changes.
(use-package wgrep
  :straight t)


(provide 'my-search)
;;; my-search.el ends here.
