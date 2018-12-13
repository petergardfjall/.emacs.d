;; Main entry-point for emacs configuration.
;; See http://wikemacs.org/wiki/Package.el


;; Use the package.el package manager that comes bundled with Emacs24
(require 'package)
(package-initialize)

(message "Loading init.el ...")

;; add package archives
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; Prevent emacs from writing customized settings to .emacs
;; By setting it as a temporary file, we effectively disable it.
;; Any changes become session-local.
(setq custom-file (make-temp-file "emacs-custom"))

;; Common Lisp for Emacs
(require 'cl-lib)

(defvar my-packages
  '(material-theme    ;; color theme
    company           ;; generic auto-completion functionality
    company-quickhelp ;; show auto-completion candidates in popup
    powerline         ;; Prettier mode line at bottom of screen
    projectile        ;; Make aware of git/VCS projects on F7
    neotree           ;; File navigator on the left via F8
    flycheck          ;; pluggable on-the-fly syntax checking
    ggtags            ;; work with GNU Global source code tagging (via gtags)
    ;; Markdown (.md) editing
    markdown-mode
    ;; Yaml editing
    yaml-mode
    ;; TOML editing
    toml-mode
    ;; emacs Language Server Protocol client
    lsp-mode       ;; client library for the Language Server Protocol
    lsp-ui
    company-lsp
    ;; Python editing
    lsp-python     ;; python support for lsp-mode
    sphinx-doc     ;; Templated docstring when pressing C-c M-d in function head
    ;; Go editing
    go-mode
    lsp-go         ;; go support for lsp-mode
    ;; Java editing
    lsp-java
    ;; Terraform editing
    terraform-mode
    ;; Rust editing
    rust-mode
    lsp-rust
    ;; C editing
    ccls     ;; LSP server for C/C++
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
(set-language-environment "UTF-8")
(setq inhibit-startup-screen t)
(setq column-number-mode t)
; set the default font to use
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))
;; Allow copy/paste to/from system clipboard
(setq x-select-enable-clipboard t)
;; Middle mouse button inserts the clipboard (rather than emacs primary)
(global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)
;; Hide vertical scrollbar on right
(scroll-bar-mode -1)
;; Hide tool-bar (icons, such as open file, cut, paste, etc)
(tool-bar-mode -1)
;; Display line numbers (toggle with M-x linum-mode)
(global-linum-mode -1)

;;
;; Package configs that can be set before the packages have been loaded
;; (happens on exit of init.el)
;;

;;
;; Set up hooks for configuration that is to take place after packages have
;; been loaded (loading happens on exit of init.el).
;;

(add-hook 'after-init-hook 'theme-setup-hook)
(add-hook 'after-init-hook 'python-setup-hook)
(add-hook 'after-init-hook 'go-setup-hook)
(add-hook 'after-init-hook 'js-setup-hook)
(add-hook 'after-init-hook 'yaml-setup-hook)
(add-hook 'after-init-hook 'markdown-setup-hook)
(add-hook 'after-init-hook 'terraform-setup-hook)
(add-hook 'after-init-hook 'rust-setup-hook)
(add-hook 'after-init-hook 'c-setup-hook)
(add-hook 'after-init-hook 'java-setup-hook)

(defun theme-setup-hook ()
  (message "theme-setup-hook ...")
  (require 'material-theme)
  (load-theme 'material t)

  ;; set initial frame width (in characters)
  (if (display-graphic-p)
      (setq initial-frame-alist '((width . 80) )))

  (require 'powerline)
  (powerline-default-theme)

  (require 'projectile)
  (global-set-key [f7] 'projectile-mode)

  (require 'neotree)
  ;; change theme for neotree when running in x mode
  (setq neo-theme (if (display-graphic-p) 'arrow))
  ;; when tree is opened, find current file and jump to tree node
  ;;(setq neo-smart-open t)
  ;; do not switch to neotree window on toggle
  (setq neo-toggle-window-keep-p t)
  ;; fix for https://github.com/jaypei/emacs-neotree/issues/209
  ;; for example, godoc-at-point would open in new frame
  (setq split-window-preferred-function 'neotree-split-window-sensibly)
  (defun neotree-show-project-aware ()
    "make neotree project-aware => open neotree at git/VCS root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name))
	  (cw (selected-window)))
      (neotree-show)
      (if project-dir
	  (progn
	    (neotree-dir project-dir)
	    (neotree-find file-name))
	(message "Could not find git project root."))
      ;; keep focus in buffer
      (when neo-toggle-window-keep-p
	(select-window cw))))
  (defun neotree-toggle-project-aware ()
    "Toggle show the NeoTree window."
    (interactive)
    (if (neo-global--window-exists-p)
	(neotree-hide)
      (neotree-show-project-aware)))
  ;; hide/show neotree
  (global-set-key [f8] 'neotree-toggle-project-aware)
  ;; refresh neotree: show entire project, set position to current buffer
  (global-set-key [f9] 'neotree-show-project-aware)


  (require 'company)
  (global-company-mode)
  (require 'company-quickhelp)
  (company-quickhelp-mode 1)
  (setq company-tooltip-limit 20) ; bigger popup window
  (setq company-idle-delay .1)    ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)     ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing


  ;; On-the-fly syntax checking (support for different languages)
  ;;(require 'flycheck)
  ;;(global-flycheck-mode)
  )

;;
;; Set up emacs Language Server Protocol client UI and keyboard shortcuts.
;;
(defun lsp-ui-setup ()
  (require 'lsp-ui)
  (require 'company-lsp)

  ;; show informations of the symbols on the current line?
  (setq lsp-ui-sideline-enable nil)
  ;; show object documentation at point in a child frame?
  (progn
    (setq lsp-ui-doc-enable t)
    ;; set background color for ui-doc popup
    (custom-set-faces '(lsp-ui-doc-background ((t (:background "#003366"))))))
  ;; enable lsp-ui-peek feature: M-x lsp-ui-peek-find-{references,definitions}
  (progn
    (setq lsp-ui-peek-enable t)
    ;; show peek view even if there is only one candidate
    (setq lsp-ui-peek-always-show t))

  ;; add lsp as company completion engine backend to get completion-at-point
  (push 'company-lsp company-backends)

  ;; keybindings for Language Server Protocol features
  (local-set-key (kbd "<M-down>") 'xref-find-definitions)
  (local-set-key (kbd "C-c d")    'lsp-ui-peek-find-definitions)
  (local-set-key (kbd "<M-up>")   'xref-pop-marker-stack)
  (local-set-key (kbd "<M-?>")    'xref-find-references)
  (local-set-key (kbd "C-c r")    'lsp-ui-peek-find-references)
  (local-set-key (kbd "C-c C-r")  'lsp-rename)

  ;; when entering lsp-mode, enable lsp-ui-mode
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )


(defun python-setup-hook ()
  (message "python-setup-hook ...")

  ;; mode hooks are evaluated once per buffer
  (defun py-buffer-setup ()
    (message "python buffer setup hook ...")
    (linum-mode t)
    ;; no tabs for indentation
    (setq indent-tabs-mode nil)

    (require 'lsp-mode)
    ;; NOTE: relies on python-language-server[all] being installed
    (unless (executable-find "pyls")
      (user-error "pyls language server not on path. In your (v)env run:\n  pip3 install python-language-server[all]\n"))
    (require 'lsp-python)
    (lsp-ui-setup)
    ;;(lsp-define-stdio-client lsp-python "python"
    ;;			     #'projectile-project-root
    ;;			     '("pyls"))
    (lsp-python-enable)

    ;; C-c M-d with cursor in method signature to generate docstring template
    (sphinx-doc-mode t)
    )
  (add-hook 'python-mode-hook 'py-buffer-setup)
)


(defun go-setup-hook ()
  ;; http://yousefourabi.com/blog/2014/05/emacs-for-go/
  (message "go-setup-hook ...")
  (require 'go-mode)

  ;; mode hooks are evaluated once per buffer
  (defun go-buffer-setup ()
    (message "go buffer setup hook ...")
    (linum-mode t)

    ;; NOTE: relies on go-langserver being on the PATH
    (unless (executable-find "go-langserver")
      (user-error "go-langserver is not on path. Run:\n  go get -u github.com/sourcegraph/go-langserver\n"))
    (require 'lsp-go)
    (lsp-ui-setup)
    (lsp-go-enable)

    ;; note: requires ${GOROOT}/bin to be on PATH
    (add-hook 'before-save-hook 'gofmt-before-save)
    )

  (add-hook 'go-mode-hook 'go-buffer-setup)
  )


(defun js-setup-hook ()
  (message "js-setup-hook ...")
  (defun js-buffer-setup ()
    (message "js buffer setup hook ...")
    ;; use 4 space indentation
    (setq indent-tabs-mode nil js-indent-level 4)
    ;; no tabs for indentation
    (setq indent-tabs-mode nil)
    (linum-mode t))
  (add-hook 'js-mode-hook 'js-buffer-setup))


(defun yaml-setup-hook ()
  (message "yaml-setup-hook ...")
  (defun yaml-buffer-setup ()
    (message "yaml buffer setup hook ...")
    ;; no tabs for indentation
    (setq indent-tabs-mode nil)
    ;; show line numbers
    (linum-mode t))
  (add-hook 'yaml-mode-hook 'yaml-buffer-setup))


(defun markdown-setup-hook ()
  (message "markdown-setup-hook ...")
  (defun markdown-buffer-setup ()
    (message "markdown buffer setup hook ...")
    ;; no tabs for indentation
    (setq indent-tabs-mode nil)
    ;; automatically break lines exceeding 80 characters
    (set-fill-column 80)
    (auto-fill-mode))
  (add-hook 'markdown-mode-hook 'markdown-buffer-setup))

(defun terraform-setup-hook ()
  (message "terraform-setup-hook ...")
  ;; default indent-level is 2
  ;; (custom-set-variables '(terraform-indent-level 2))
  (defun terraform-buffer-setup ()
    (message "terraform buffer setup hook ...")
    ;; show line numbers
    (linum-mode t))
  (add-hook 'terraform-mode-hook 'terraform-buffer-setup))

(defun rust-setup-hook ()
  ;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
  (message "rust-setup-hook ...")
  (require 'rust-mode)
  (require 'lsp-rust)

  ;; mode hooks are evaluated once per buffer
  (defun rust-buffer-setup ()
    (message "rust buffer setup hook ...")
    (linum-mode t)

    (setq rust-format-on-save t)
    (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
    (lsp-ui-setup)
    (lsp-rust-enable))

  (add-hook 'rust-mode-hook 'rust-buffer-setup)
  )


(defun c-setup-hook ()
  (message "c-setup-hook ...")
  (require 'ccls)

  ;; mode hooks are evaluated once per buffer
  (defun c-buffer-setup ()
    (message "c buffer setup hook ...")
    (linum-mode t)

    (setq ccls-executable "/opt/bin/ccls")
    (lsp-ui-setup)
    (lsp-ccls-enable)

    ;; For proper operation, a .ccls or compile_commands.json file is needed in
    ;; the project root.
    ;; For CMake projects, a compile_commands.json is created via:
    ;;   mkdir build
    ;;   (cd build; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES ...)
    ;;   ln -s build/compile_commands.json
    )

  (add-hook 'c-mode-hook 'c-buffer-setup)
  (add-hook 'c++-mode-hook 'c-buffer-setup)
  )

(defun java-setup-hook ()
  (message "java-setup-hook ...")
  (require 'lsp-java)
  ;; mode hooks are evaluated once per buffer
  (defun java-buffer-setup ()
    (message "java buffer setup hook ...")
    (linum-mode t)

    ;; disable completion cache
    (setq company-lsp-cache-candidates nil)
    (lsp-ui-setup)
    (lsp-java-enable)
    )
  (add-hook 'java-mode-hook 'java-buffer-setup))


;
; load any local modules from module directory in lexicographical order
;
(setq modules (file-expand-wildcards "~/dotfiles/emacs.modules/*.el"))
(setq sortedmodules (sort (copy-sequence modules) #'string-lessp))
;; Note: messages are logged in *Messages* buffer
(message "Loading local modules: %s" sortedmodules)
(dolist (module sortedmodules)
  (load-file module)
)

(message "%s" "emacs.init done.")
