;;; package --- Programming-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


(use-package c-mode
  :straight (:type built-in)
  :commands (c-mode c-ts-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  ;; Set up on-save hooks to have eglot format and organize imports.
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook
            (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t))))


(use-package c++-mode
  :straight (:type built-in)
  :commands (c++-mode c++-ts-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  ;; Set up on-save hooks to have eglot format and organize imports.
  (add-hook 'c++-ts-mode-hook #'eglot-ensure)
  (add-hook 'c++-ts-mode-hook
            (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t))))


(use-package clang-format
  :straight t
  :commands (clang-format clang-format-buffer clang-format-region)
  :config
  ;; Style to use when calling `clang-format-buffer' unless the project has a
  ;; .clang-format file.
  (setq clang-format-fallback-style "WebKit"))


(use-package cmake-mode
  :straight t
  :commands (cmake-mode)
  :mode (("CMakeLists.txt$" . cmake-mode)
         ("\\.cmake$" . cmake-mode))
  :config)


;; Dockerfile editing
(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'dockerfile-ts-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'dockerfile-ts-mode-hook #'my-strip-on-save-hook))


(defun my--add-eglot-format-on-save-hook ()
  "Register a buffer-local `before-save-hook' to format and organize imports."
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  ;; See https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook (lambda () (call-interactively #'eglot-code-action-organize-imports)) -9 t))

;; Eglot is the Emacs client for the Language Server Protocol (LSP).
;; To use a language server with a certain type of source file first make sure
;; the language server is installed (and present on the PATH), then open a
;; source file buffer and run `eglot' or `eglot-ensure'. All source files in the
;; project will be managed by Eglot.
(use-package eglot
  :straight (:type built-in)
  :commands (eglot eglot-ensure)
  :diminish (eldoc-mode)
  :config
  ;; Automatically shut down server after killing last managed buffer.
  (setq eglot-autoshutdown t)
  ;; Require a manual restart (`eglot') when language servers crash.
  (setq eglot-autoreconnect nil)
  ;; Prevent long identifier documentation to be shown when cursor "hovers" over
  ;; an identifier.
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; Be explicit about which LSP servers to use.
  (add-to-list 'eglot-server-programs '((c-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '((c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '((cmake-mode) . ("cmake-language-server")))
  (add-to-list 'eglot-server-programs '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
                                        . ("typescript-language-server" "--stdio")))
  ;; See https://rust-analyzer.github.io/manual.html#emacs
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  ;; Additional gopls settings.
  ;; See https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
  ;;     https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (setq-default eglot-workspace-configuration
                '((:gopls
                   (:ui.completion.usePlaceholders . t)
                   ;; (:ui.diagnostic.staticcheck . t)
                   ;; For proper operation in files with certain build tags.
                   (:build.buildFlags . ["-tags=integration,db"]))))

  ;; If `xref-find-definitions' lands in a file outside the project, momentarily
  ;; consider that file managed by the same language server. This avoids
  ;; starting a new language server for such external files (startup cost).
  (setq eglot-extend-to-xref t)

  (defun my--find-workspace-symbol ()
    "Look up a workspace symbol by name.
Prompts the user for input. It does the equivalent of `C-u M-.'."
    (interactive)
    (setq current-prefix-arg '(1)) ;; programatically calls with prefix argument "C-u".
    (call-interactively 'xref-find-definitions))

  ;; Define key-bindings.
  (let ((m eglot-mode-map))
    (define-key m (kbd "<M-down>") #'xref-find-definitions)
    (define-key m (kbd "<M-up>")   #'xref-go-back)
    (define-key m (kbd "C-c f d")  #'xref-find-definitions)
    ;; find workspace symbol
    (define-key m (kbd "C-c f s")  #'my--find-workspace-symbol)
    (define-key m (kbd "C-c f i")  #'eglot-find-implementation)
    (define-key m (kbd "C-c f r")  #'xref-find-references)
    (define-key m (kbd "C-c C-r")  #'eglot-rename)
    (define-key m (kbd "C-c d")    #'eldoc)))


;; On-the-fly syntax checking, which highlights erroneous lines.
(use-package flymake
  :straight (:type built-in)
  :diminish
  :hook ((prog-mode text-mode) . flymake-mode)
  :config
  (let ((m flymake-mode-map))
    ;; "show errors in project"
    (define-key m (kbd "C-c s e p") #'flymake-show-project-diagnostics)
    ;; "show show errors in file"
    (define-key m (kbd "C-c s e f") #'flymake-show-buffer-diagnostics)))


;; golangci-lint support via Flymake.
(use-package flymake-golangci
  :straight t
  :disabled t
  :commands (flymake-golangci-load)
  :hook (go-ts-mode . flymake-golangci-load))


;; Emacs frontend for GNU Global (gtags) to generate and search code tags.
;; Wraps the gtags and global command-line tools.
(use-package ggtags
  :straight t
  :diminish
  ;; add ggtags as a xref backend in emacs-lisp-mode (xref-find-definitions)
  :hook ((emacs-lisp-mode . ggtags-mode))
  ;; Lazily load when called for.
  :bind (("C-c t c"   . my-ggtags-create)
	 ("C-c t f d" . my-ggtags-find-definition)
	 ("C-c t f r" . my-ggtags-find-reference))
  :config
  ;; interferes with beginning/end of buffer key bindings
  (define-key ggtags-navigation-map (kbd "M->") nil)
  (define-key ggtags-navigation-map (kbd "M-<") nil))


(use-package groovy-mode
  :straight t
  :disabled t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gvy$" . groovy-mode)
	 ("\\.gy$" . groovy-mode)
	 ("^Jenkinsfile$" . groovy-mode)))


(use-package go-ts-mode
  :straight (:type built-in)
  :commands (go-mode go-ts-mode)
  :mode (("\\.go$" . go-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  ;; Set up on-save hooks to have eglot format and organize imports.
  (add-hook 'go-ts-mode-hook #'my--add-eglot-format-on-save-hook)
  ;; Sets the fill column (where to break paragraphs on M-q)
  (add-hook 'go-ts-mode-hook (lambda () (setq-local fill-column 100)))
  (add-hook 'go-ts-mode-hook #'eglot-ensure))


(use-package graphql-mode
  :straight t
  :mode (("\\.gql$" . graphql-mode)
         ("\\.graphql$" . graphql-mode))
  :init
  (add-hook 'graphql-mode #'prettier-mode))


(use-package highlight-indent-guides
  :straight t
  :diminish
  :hook ((emacs-lisp-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 100))


;; Major mode for JavaScript and React/JSX (built-into Emacs). `js-ts-mode'
;; comes with syntax highlighting/indent support for JSX.
(use-package js-ts-mode
  :straight (:type built-in)
  :commands (js-mode js-ts-mode)
  :mode (("\\.js$" . js-ts-mode)
         ("\\.jsx$" . js-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  (add-hook 'js-ts-mode-hook #'eglot-ensure)
  (add-hook 'js-ts-mode-hook #'prettier-mode))

;; Provides a `jsdoc' command to produce a jsdoc [1] template for a function.
;; [1] https://jsdoc.app
(use-package jsdoc
  :straight (:host github :repo "isamert/jsdoc.el")
  :after js-ts-mode)


;; Enable the Prettier code-formatter's minor mode to format on save whenever we
;; edit JavaSciprt/JSX.  https://prettier.io/.
(use-package prettier
  :straight t
  ;; Will `autoload' whenever `prettier-mode' is invoked.
  :commands (prettier-mode))


(use-package protobuf-mode
  :straight t
  :mode (("\\.proto$" . protobuf-mode))
  :init
  (add-hook 'protobuf-mode-hook #'my-highlight-todos)
  ;; Add buffer-local save hook only for buffers in this mode.
  (add-hook 'protobuf-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'protobuf-mode-hook #'my-strip-on-save-hook))


(use-package python-mode
  :straight (:type built-in)
  :mode (("\\.py$" . python-mode))
  :init
  ;; Add settings for buffers in this mode.
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local
               indent-tabs-mode nil ;; No tabs for indentation.
               ;; PEP-8 calls for max 79 characters per line.
               fill-column 79
               python-indent-offset 4
               python-indent-guess-indent-offset nil)))
  (add-hook 'python-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'python-mode-hook #'my-strip-on-save-hook)
  (add-hook 'python-mode-hook #'eglot-ensure))


(use-package ruby-mode
  :straight (:type built-in)
  :mode (("\\.rb$"  . ruby-mode))
  :config)


(use-package rust-mode
  :straight t
  :commands (rust-ts-mode rust-mode)
  :mode (("\\.rs$" . rust-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  ;; Set up on-save hooks to have eglot format and organize imports.
  (add-hook 'rust-ts-mode-hook #'my--add-eglot-format-on-save-hook)
  (add-hook 'rust-ts-mode-hook (lambda ()
                                 (setq-local
                                  indent-tabs-mode nil
                                  rust-format-on-save t)))
  (add-hook 'rust-ts-mode-hook #'eglot-ensure))


(use-package sh-script
  :straight (:type built-in)
  :commands (sh-mode)
  :mode (("\\.sh$" . sh-mode)
         ("\\.env$" . sh-mode))
  :init
  (add-hook 'sh-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'sh-mode-hook #'my-strip-on-save-hook))


;; Use `sphinx-doc' when `python-mode' is activated. Gives a templated docstring
;; when pressing "C-c M-d" in function head.
(use-package sphinx-doc
  :straight t
  :commands python-mode
  :config
  (sphinx-doc-mode))


(use-package terraform-mode
  :straight t
  :mode (("\\.tf$" . terraform-mode))
  :init
  ;; Add buffer-local save hook only for buffers in this mode.
  (add-hook 'terraform-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'terraform-mode-hook #'my-strip-on-save-hook))


;; Integrates the tree-sitter incremental language parsing library. It supports
;; syntax highlighting and comes with replacement major-modes for many languages
;; '<language>-ts-mode'.
(use-package treesit
  :straight (:type built-in)
  :defer t
  :init
  ;; Use treesit-based major-modes where grammars are available.
  (add-to-list 'major-mode-remap-alist '(bash-mode   . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode    . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(html-mode   . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode   . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sql-mode    . sql-ts-mode))
  (add-to-list 'major-mode-remap-alist '(toml-mode   . toml-ts-mode))
  ;; Specify which tree-sitter language grammar defintions to use.
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :config
  ;; Install language grammars if not already present.
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
        (display-warning 'init.el (format "Installing language grammar for `%s' ..." lang) :warning)
        (sleep-for 0.5)
        (treesit-install-language-grammar lang)
        (message "`%s' treesit language grammar installed." lang)))))


(use-package typescript-ts-mode
  :straight (:type built-in)
  :commands (typescript-mode typescript-ts-mode)
  :mode (("\\.ts$" . typescript-ts-mode)
         ("\\.tsx$" . typescript-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'prettier-mode))


;; Varnish .vcl file editing.
(use-package vcl-mode
  :straight t
  :disabled t
  :mode (("\\.vcl$" . vcl-mode))
  :init
  ;; Add buffer-local save hook only for buffers in this mode.
  (add-hook 'vcl-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'vcl-mode-hook #'my-strip-on-save-hook))


;; A language template system for emacs. lsp-mode auto-configures yasnippet for
;; use with a given language server.  Write a snippet key and press the key
;; associated with yas-expand (TAB by default) to have the snippet expanded. To
;; see available snippets: M-x yas-describe-tables.
;; Custom (per-mode) snippets can be placed under ~/.emacs.d/snippets/.
(use-package yasnippet
  :straight t
  :defer 2
  :diminish yas-minor-mode ; don't display on modeline
  :config
  ;; use yasnippet as a global minor mode
  ;; note: it can also be activated per language/major-mode
  ;;    see https://github.com/joaotavora/yasnippet
  (yas-global-mode 1))

;; A collection of snippets for many languages.
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)


(provide 'my-progmodes)
;;; my-progmodes.el ends here.
