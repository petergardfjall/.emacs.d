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
    ;; Python editing
    python         ;; Python mode
    company-jedi   ;; Python auto-completion and method signature help
    sphinx-doc     ;; Templated docstring when pressing C-c M-d in function head
    ;; Go editing
    go-mode        ;; Golang mode
    company-go     ;; auto-complete
    go-eldoc       ;; shows type info for var/func/curr arg position.
    ;; TODO: install YAsnippet for template support (see https://dominik.honnef.co/posts/2013/03/emacs-go-1/)
    terraform-mode
    ;; Rust editing
    rust-mode
    racer          ;; code completion and source navigation for Rust
    flycheck-rust  ;; on-the-fly syntax checking
    ;; C editing
    company-c-headers ;; code-completion for C/C++ includes
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

(defun theme-setup-hook ()
  (message "theme-setup-hook ...")
  (require 'material-theme)
  (load-theme 'material t)

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
  (setq company-idle-delay .2)    ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)     ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing


  ;; On-the-fly syntax checking (support for different languages)
  ;;(require 'flycheck)
  ;;(global-flycheck-mode)
  )


(defun python-setup-hook ()
  (message "python-setup-hook ...")
  (require 'python)
  (require 'sphinx-doc)

  ;; mode hooks are evaluated once per buffer
  (defun py-buffer-setup ()
    (message "python buffer setup hook ...")
    (linum-mode t)
    ;; C-c M-d with cursor in method signature to generate docstring template
    (sphinx-doc-mode t)

    (add-to-list 'company-backends 'company-jedi)
    ;; set up jedi to use python3
    (setq jedi:environment-virtualenv
	  (list "virtualenv" "--python" "/usr/bin/python3"))
    (setq jedi:environment-root "~/.emacs.d/.python-environments/default")
    ;; use minibuffer instead of pop-up to display method signature
    (setq jedi:tooltip-method nil)
    ;; Set up recommended key bindings (optional)
    (setq jedi:setup-keys t)
    ;; Start auto-completion when entering a '.' (optional)
    (setq jedi:complete-on-dot t)
    ;; enable auto-completion and method signature help
    (jedi:setup)
    (local-set-key (kbd "C-c C-d")  'jedi:show-doc)
    (local-set-key (kbd "<M-down>") 'jedi:goto-definition)
    (local-set-key (kbd "<M-up>")   'jedi:goto-definition-pop-marker)
    )
  (add-hook 'python-mode-hook 'py-buffer-setup))


(defun go-setup-hook ()
  ;; http://yousefourabi.com/blog/2014/05/emacs-for-go/
  (message "go-setup-hook ...")
  (require 'go-mode)

  ;; mode hooks are evaluated once per buffer
  (defun go-buffer-setup ()
    (message "go buffer setup hook ...")
    (linum-mode t)

    ;; auto-completion
    (require 'company)
    (require 'company-go)
    (add-to-list 'company-backends 'company-go)
    ;(setq company-go-show-annotation t)
    ;; type information in minibuffer
    (require 'go-eldoc)
    (go-eldoc-setup)


    (add-hook 'before-save-hook 'gofmt-before-save)
    (local-set-key (kbd "C-c C-d")  'godoc-at-point)
    (local-set-key (kbd "<M-down>") 'godef-jump)
    (local-set-key (kbd "<M-up>")   'pop-tag-mark)
    (local-set-key (kbd "C-c C-r")  'go-remove-unused-imports)
    (local-set-key (kbd "C-c C-g")  'go-goto-imports)
    (local-set-key (kbd "C-c C-f")  'gofmt)
    )
  (add-hook 'go-mode-hook 'go-buffer-setup))


(defun js-setup-hook ()
  (message "js-setup-hook ...")
  (defun js-buffer-setup ()
    (message "js buffer setup hook ...")
    ;; use 4 space indentation
    (setq indent-tabs-mode nil js-indent-level 4)
    (linum-mode t))
  (add-hook 'js-mode-hook 'js-buffer-setup))


(defun yaml-setup-hook ()
  (message "yaml-setup-hook ...")
  (defun yaml-buffer-setup ()
    (message "yaml buffer setup hook ...")
    ;; show line numbers
    (linum-mode t))
  (add-hook 'yaml-mode-hook 'yaml-buffer-setup))


(defun markdown-setup-hook ()
  (message "markdown-setup-hook ...")
  (defun markdown-buffer-setup ()
    (message "markdown buffer setup hook ...")
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

  ;; mode hooks are evaluated once per buffer
  (defun rust-buffer-setup ()
    (message "rust buffer setup hook ...")
    (linum-mode t)
    (global-flycheck-mode)

    ;; note: must install racer (code completion) with 'cargo install racer'
    (setq racer-cmd "~/.cargo/bin/racer")
    (setq racer-rust-src-path (substitute-in-file-name "${RUST_SRC}"))

    (local-set-key (kbd "<M-down>") 'racer-find-definition)
    (local-set-key (kbd "<M-up>")   'pop-tag-mark)
    )
  (add-hook 'rust-mode-hook 'rust-buffer-setup)
  ;; activate racer code-completion when rust-mode starts
  (add-hook 'rust-mode-hook #'racer-mode)
  ;; activate eldoc-mode (show fn arg list in echo area) when racer-mode starts
  (add-hook 'racer-mode-hook #'eldoc-mode)
  ;; activate company-mode (completion mode) when racer-mode starts
  (add-hook 'racer-mode-hook #'company-mode)
  ;; syntax checking when fly-mode starts
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(defun c-setup-hook ()
  (message "c-setup-hook ...")
  (require 'ggtags)

  ;; mode hooks are evaluated once per buffer
  (defun c-buffer-setup ()
    (message "c buffer setup hook ...")
    (linum-mode t)

    ;; company auto-completion settings
    (add-to-list 'company-backends 'company-clang)
    (add-to-list 'company-backends 'company-c-headers)
    ;; make autocomplete candidates appear immediately
    (setq company-idle-delay 0)

    ;; Enable ggtags package (and code navigation via gtags).
    ;; First generate GTAGS database:
    ;;     cd /proj/path; gtags
    ;; Or, for out-of-tree/read-only directories:
    ;;     mkdir /var/dbpath
    ;;     cd /usr/src/linux-source-4.15.0/linux-source-4.15.0
    ;;     gtags /var/dbpath
    ;;     export GTAGSROOT=/usr/src/linux-source-4.15.0/linux-source-4.15.0
    ;;     export GTAGSDBPATH=/var/dbpath
    ;;     global inet_csk_accept
    ;;     emacs net/ipv4/inet_connection_sock.c
    ;; After this, ggtags in emacs should be able to find definitions, etc.    
    ;; See: https://www.gnu.org/software/global/manual/global.html#Applied-usage
    (ggtags-mode 1)
    ;; find definition works as follows:
    ;; - if tag at point is a definition, ggtags jumps to a reference.
    ;;   If there is more than one reference, it displays a list of references.
    ;;   (use M-n/M-p to move to next/previous entry).
    ;; - if the tag at point is a reference, ggtags jumps to tag definition.
    ;; - if the tag at point is an include header, it jumps to that header.
    (local-set-key (kbd "<M-down>") 'ggtags-find-tag-dwim)
    (local-set-key (kbd "<M-up>")   'pop-tag-mark)
    )
  (add-hook 'c-mode-hook 'c-buffer-setup))

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
