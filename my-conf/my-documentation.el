;;; package --- Configuration of documentation packages.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Major mode for AsciiDoc (.adoc) file editing.
(use-package adoc-mode
  :straight (adoc-mode :type git :host github
                       :repo "bbatsov/adoc-mode" :branch "master")
  :commands (adoc-mode)
  :mode (("\\.adoc$" . adoc-mode))
  :config
  (add-hook 'adoc-mode-hook #'my-highlight-todos)
  ;; Don't want orgtable "help" with formatting tables.
  (add-hook 'adoc-mode-hook (lambda () (orgtbl-mode -1))))


;; Built-in on-the-fly spell checking for text buffers.
(use-package flyspell
  :straight (:type built-in)
  :diminish
  :hook ((text-mode . flyspell-mode)))


;; Major mode for markdown (.md) file editing.
(use-package markdown-ts-mode
  :straight (:type built-in)
  :commands (markdown-ts-mode)
  :mode (("README\\.md$" . markdown-ts-mode)
         ("\\.md$" . markdown-ts-mode)
         ("\\.markdown$" . markdown-ts-mode)
         ("\\.mdx$" . markdown-ts-mode)
         ;; Cheat sheets under ~/dotfiles/cheat/sheets.
         ("\\.cheat$" . markdown-ts-mode))
  :hook ((markdown-ts-mode . prettier-mode))
  :config
  (require 'markdown-ts-mode-x))


(provide 'my-documentation)
;;; my-documentation.el ends here.
