;;; emacs-init.el --- Main entry-point for emacs configuration.

;; See http://wikemacs.org/wiki/Package.el

;;; Code:

;; Time the loading of this file.
(defconst emacs-start-time (current-time))
;; Function to print the elapsed time since start of loading.
(defun elapsed-time ()
  (float-time (time-subtract (current-time) emacs-start-time)))


;; Use the package.el package manager that comes bundled with Emacs24
(require 'package)
(package-initialize)

(message "Loading emacs-init.el ...")

;; Add package archives:
;; Note that packages will always be picked from melpa unless specifically
;; pinned to use melpa-stable due to versioning schemes (20190101-1200 > 2.4).
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)


;; Prevent emacs from writing customized settings to .emacs
;; By setting it as a temporary file, we effectively disable it.
;; Any changes become session-local.
(setq custom-file (make-temp-file "emacs-custom"))

;; Common Lisp for Emacs
(require 'cl-lib)

(defvar my-packages
  '(use-package       ;; package declaration macro
     ;;ggtags            ;; work with GNU Global source code tagging (via gtags)
    )
  "A list of packages that are to be installed at launch (unless present).")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
	   when (not (package-installed-p p))  do (cl-return nil)
	   finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;;
;; General settings
;;
(defun general-settings ()
  (set-language-environment "UTF-8")
  (setq inhibit-startup-screen t)
  (setq column-number-mode t)
  ;; Sets the fill column (where to break paragraphs on M-q)
  (setq-default fill-column 80)
					; set the default font to use
  (add-to-list 'default-frame-alist
	       '(font . "DejaVu Sans Mono-10"))
  ;; Allow copy/paste to/from system clipboard
  (setq select-enable-clipboard t)
  ;; Middle mouse button inserts the clipboard (rather than emacs primary)
  (global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)
  ;; Hide vertical scrollbar on right
  (scroll-bar-mode -1)
  ;; Hide tool-bar (icons, such as open file, cut, paste, etc)
  (tool-bar-mode -1)
  ;; Display line numbers (toggle with M-x linum-mode)
  (global-linum-mode -1)
  ;; Make yes/no prompts shourter (y/n)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; no blinking cursor
  (blink-cursor-mode 0)
  ;; set initial frame width (in characters)
  (if (display-graphic-p)
      (setq initial-frame-alist '((width . 80) )))
  ;; Comment line(s)
  (global-set-key (kbd "C-c c") 'comment-line)
  (global-set-key (kbd "C-c d w") 'delete-trailing-whitespace)
  ;; Show matching paranthesis
  (show-paren-mode 1)
  (setq show-paren-delay 0)
)

(general-settings)

;;;
;;; Start for custom package installation/configuration.
;;;

(require 'use-package)

;;
;; Theme-related settings
;;

(use-package diminish
  :ensure t
  :demand t)


(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode ; don't display on modeline
  :init
  (global-undo-tree-mode)
  :bind
  (:map undo-tree-map
	("C-x u" . undo)
	("C-z" . undo-tree-undo)
	("C-Z" . undo-tree-redo)
	;; show undo tree (can select state and press 'q')
	("C-c u t" . undo-tree-visualize))
   )

(use-package projectile
  :defer t ;; actually implied by :commands
  :commands projectile-mode
  :init
  (global-set-key [f7] 'projectile-mode))

;; generic auto-completion functionality
(use-package company
    :ensure t
    :diminish ; don't display on modeline
    :init
    (add-hook 'after-init-hook 'global-company-mode)
    :config
    (setq company-tooltip-limit 20) ; bigger popup window
    (setq company-idle-delay .1)    ; decrease delay 'til completion popup shows
    (setq company-echo-delay 0)     ; remove annoying blinking
    (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
    )

;; show auto-completion candidates in popup
(use-package company-quickhelp
  :ensure t
  :hook (company-quickhelp-mode))

;; On-the-fly syntax checking (support for different languages)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :diminish ; don't display on modeline
  :config
  ;; list errors in current buffer
  (global-set-key (kbd "C-c e") 'list-flycheck-errors))

;; A languate template system for emacs. lsp-mode auto-configures yasnippet for
;; use with a given language server.  Write a snippet key and press the key
;; associated with yas-expand (TAB by default) to have the snippet expanded. To
;; see available snippets: M-x yas-describe-tables
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode ; don't display on modeline
  :config
  ;; use yasnippet as a global minor mode
  ;; note: it can also be activated per language/major-mode
  ;;    see https://github.com/joaotavora/yasnippet
  (yas-global-mode 1))

;; A collection of snippets for many languages.
(use-package yasnippet-snippets
  :ensure t
  :defer t)

;; File navigator on the left via F8
(use-package neotree
  :ensure t
  :defer t
  :commands (neotree-toggle neotree-toggle-project-aware)
  :init
  (defun neotree-project-dir-toggle ()
    "Projectile-aware neotree-toggle."
    (interactive)
    (let ((cw (selected-window))
	  (project-dir
	   (ignore-errors (projectile-project-root)))
	  (file-name (buffer-file-name))
	  (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
	       (neo-global--window-exists-p))
	  (neotree-hide)
	(progn
	  (neotree-show)
	  (if project-dir
	      (neotree-dir project-dir))
	  (if file-name
	      (neotree-find file-name))
	  (select-window cw)))))

  ;; hide/show neotree
  (global-set-key [f8] 'neotree-project-dir-toggle)
  :config
  (message "neotree config ...")
  ;; change theme for neotree when running in x mode
  (setq neo-theme (if (display-graphic-p) 'arrow))
  ;; when tree is opened, find current file and jump to tree node
  (setq neo-smart-open t)
  ;; Auto-refresh the neotree buffer
  (setq neo-autorefresh t)
  ;; do not switch to neotree window on toggle
  (setq neo-toggle-window-keep-p t)
  ;; fix for https://github.com/jaypei/emacs-neotree/issues/209
  ;; for example, godoc-at-point would open in new frame
  (setq split-window-preferred-function 'neotree-split-window-sensibly)
  )

;;;
;;; Development/coding
;;;

(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :init
  (message "lsp-mode init ...")
  ;; Set to t to have eldoc display hover info when present.
  (setq lsp-eldoc-enable-hover nil)
  ;; Set to t to have eldoc display signature help when present.
  (setq lsp-eldoc-enable-signature-help nil)
  ;; display signature info when both signature and hover info present
  (setq lsp-eldoc-prefer-signature-help t)
  ;; Define whether all of the returned by document/onHover will be displayed.
  ;; If set to nil eldoc will show only the symbol information.
  (setq lsp-eldoc-render-all t)
  ;; Seconds to wait for a response from the language server before timing out.
  (setq lsp-response-timeout 5))


(use-package lsp-ui
  :ensure t
  :defer t
  ;; gets started by lsp-mode
  :commands lsp-ui-mode
  :init
  ;; show informations of the symbols on the current line?
  (setq lsp-ui-sideline-enable nil)
  ;; show object documentation at point in a child frame?
  (progn
    ;; enable (t)/disable (nil) lsp-ui-doc: indicate if a separate frame for
    ;; is to be used for rendering docs on hover
    (setq lsp-ui-doc-enable nil)
    ;; set background color for ui-doc popup
    (custom-set-faces '(lsp-ui-doc-background ((t (:background "#003366"))))))
  ;; enable lsp-ui-peek feature: M-x lsp-ui-peek-find-{references,definitions}
  (progn
    (setq lsp-ui-peek-enable t)
    ;; show peek view even if there is only one candidate
    (setq lsp-ui-peek-always-show t))
  ;; add lsp as company completion engine backend to get completion-at-point
  (push 'company-lsp company-backends)
  :config
  ;; keybindings for Language Server Protocol features
  (global-set-key (kbd "<M-down>") 'lsp-find-definition)
  (global-set-key (kbd "<M-up>")   'xref-pop-marker-stack)
  (global-set-key (kbd "C-c p d")  'lsp-ui-peek-find-definitions)
  (global-set-key (kbd "C-c p r")  'lsp-ui-peek-find-references)
  (global-set-key (kbd "C-c h")    'lsp-hover)
  (global-set-key (kbd "C-c f d")  'lsp-find-definition)
  (global-set-key (kbd "C-c f r")  'lsp-find-references)
  (global-set-key (kbd "C-c C-r")  'lsp-rename)
  (global-set-key (kbd "C-c C-d")  'lsp-describe-thing-at-point)
  )

(use-package company-lsp
  :ensure t
  :defer t
  :commands company-lsp)


(use-package python-mode
  :ensure t
  :defer t
  :mode (("\\.py\\'" . python-mode))
  :config

  ;; Templated docstring when pressing C-c M-d in function head
  (use-package sphinx-doc
    :ensure t)

  (message "python buffer setup hook ...")
  (linum-mode t)
  ;; no tabs for indentation
  (setq indent-tabs-mode nil)
  ;; NOTE: relies on python-language-server[all] being installed
  (unless (executable-find "pyls")
    (user-error "pyls language server not on path. In your (v)env run:\n  pip3 install python-language-server[all]\n"))
  (sphinx-doc-mode t)
  ;; start lsp-mode
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package go-mode
  :ensure t
  :defer t
  :mode (("\\.go\\'" . go-mode))
  :config
  (message "go-mode config ...")
  (linum-mode)
  ;; NOTE: relies on bingo lsp server being on the PATH
  (unless (executable-find "bingo")
    (user-error "bingo LSP server is not on PATH\n"))
  ;; run gofmt (or actually, goimports) on save
  ;; note: requires ${GOROOT}/bin to be on PATH
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; start lsp-mode
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))


;; Major mode for json file editing.
(use-package json-mode
  :ensure t
  :defer t
  :mode (("\\.json\\'" . json-mode)
	 ("\\.js\\'" . json-mode))
  :config
  (message "json buffer config ...")
  (setq indent-tabs-mode nil js-indent-level 4) ; use 4 space indentation
  (setq indent-tabs-mode nil) ; no tabs for indentation
  (linum-mode t) ; show line numbers
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Major mode for yaml file editing.
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yaml\\'" . yaml-mode)
	 ("\\.yml\\'" . yaml-mode))
  :config
  (message "yaml buffer config ...")
  (setq indent-tabs-mode nil) ; no tabs for indentation
  (linum-mode t)) ; show line numbers

;; Major mode for markdown (.md) file editing.
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (message "markdown buffer config ...")
  ;; no tabs for indentation
  (setq indent-tabs-mode nil))

;; Varnish .vcl file editing.
(use-package vcl-mode
  :ensure t
  :defer t
  :mode (("\\.vcl\\'" . vcl-mode))
  :config (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Dockerfile editing
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-mode))
  :config (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; TOML editing
(use-package toml-mode
  :ensure t
  :defer t
  :mode (("\\.toml\\'" . toml-mode))
  :config (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package terraform-mode
  :ensure t
  :defer t
  :mode (("\\.tf\\'" . terraform-mode))
  :config
  (message "terraform-mode config ...")
  ;; show line numbers
  (linum-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Rust-mode
(use-package rust-mode
  :ensure t
  :defer t
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (message "rust-mode config ...")
  (linum-mode t)
  (setq rust-format-on-save t)
  ;; start rust LSP server.
  (add-hook 'rust-mode-hook 'lsp))

;; LSP server for C/C++17
(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode objc-mode) .
	 (lambda () (require 'ccls) (lsp)))
  :config
  (message "C/C++ (ccls) config ...")
  (linum-mode t)
  (setq ccls-executable "/opt/bin/ccls")
  ;; For proper operation, a .ccls or compile_commands.json file is needed in
  ;; the project root.
  ;; For CMake projects, a compile_commands.json is created via:
  ;;   mkdir build
  ;;   (cd build; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES ...)
  ;;   ln -s build/compile_commands.json
  (add-hook 'before-save-hook 'delete-trailing-whitespace))


(use-package lsp-java
  :ensure t
  :defer t
  :hook ((java-mode) .
	 (lambda () (require 'lsp-java) (lsp)))
  :config
  (message "lsp-java config ...")
  (linum-mode t)
  ;; disable completion cache
  (setq company-lsp-cache-candidates nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; remove "ElDoc" from modeline
(use-package eldoc
  :diminish eldoc-mode)

;;
;; Load any local modules from module directory in lexicographical order.
;;

(setq modules (file-expand-wildcards "~/dotfiles/emacs.modules/*.el"))
(setq sortedmodules (sort (copy-sequence modules) #'string-lessp))
;; Note: messages are logged in *Messages* buffer
(message "Loading local modules: %s" sortedmodules)
(dolist (module sortedmodules)
  (load-file module)
)

;;; Finalization


;; Output the time at which the loading of the init-file itself completed.
(message "Loaded %s after %.3fs." load-file-name (elapsed-time))

;; Output the time at which the loading of all init-hooks completed.
(add-hook 'after-init-hook
	  (lambda () (message "init-hooks done after %.3fs." (elapsed-time)))
	  t)
