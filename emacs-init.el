;;; emacs-init.el --- Main entry-point for emacs configuration.
;;; Commentary:
;;;
;;; Makes heavy use of the use-package macro.  For bootstrapping, and installing
;;; the use-package package a procedure similar to this is followed:
;;; http://wikemacs.org/wiki/Package.el

;;; Code:

;; Time for starting to load this file.
(defconst emacs-start-time (current-time))

;;
;; Declarations
;;

(defconst treemacs-min-width 120
  "Minimum frame width when treemacs is enabled (in characters).")

;;
;; Tricks to reduce startup time. These need to be set at an eary stage.
;;

;; avoid GC performance-penalty on startup by temporarily bumping the memory
;; threshold for GC. This effectively defers garbage collection.
(defvar gc-cons-threshold-default gc-cons-threshold)
(setq gc-cons-threshold 268435456) ;; 256 mb
;; don't run (package-initialize)
(setq package-enable-at-startup nil)
;; at startup we don't want emacs to look for a handler for every opened file.
(defvar file-name-handler-alist-default file-name-handler-alist)
(setq file-name-handler-alist nil)

;; re-set temporarily disabled features once init is complete.
(add-hook 'after-init-hook
          (lambda ()
	    ;; re-set default GC threshold.
            (setq gc-cons-threshold gc-cons-threshold-default)
	    ;; re-enable file handler associations.
	    (setq file-name-handler-alist file-name-handler-alist-default)))

;;
;; Utility functions
;;

(defun elapsed-time ()
  "Get the elapsed time since start of loading."
  (float-time (time-subtract (current-time) emacs-start-time)))

(defun read-file (path)
  "Read the file at the specified PATH (may contain ~) and return as string."
  (f-read (car (file-expand-wildcards path))))

(defun untabify-buffer ()
  "Run 'untabify' on the whole buffer."
  (untabify (point-min) (point-max)))

(defun strip-buffer ()
  "Run 'delete-trailing-whitespace' on the whole buffer."
  (delete-trailing-whitespace))

(defun untabify-on-save-hook ()
  "Register a buffer-local 'before-save-hook' to run 'generic-fmt-buffer'."
  ;; note: last argument makes this save-hook local to the buffer
  (add-hook 'before-save-hook 'untabify-buffer nil t))

(defun strip-on-save-hook ()
  "Register a buffer-local 'before-save-hook' to run 'strip-buffer'."
  ;; note: last argument makes this save-hook local to the buffer
  (add-hook 'before-save-hook 'strip-buffer nil t))

(defun close-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (let ((new-name (read-file-name "Rename file: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;
;; Start of actual initialization.
;;

(message "Loading emacs-init.el ...")

;; Use the package.el package manager that comes bundled with Emacs24
(require 'package)
(package-initialize)

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
  '(use-package)
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
;; Load any local modules from module directory in lexicographical order.
;;

(setq modules (file-expand-wildcards "~/dotfiles/emacs.modules/*.el"))
(setq sortedmodules (sort (copy-sequence modules) #'string-lessp))
;; Note: messages are logged in *Messages* buffer
(message "Loading local modules: %s" sortedmodules)
(dolist (module sortedmodules)
  (load-file module)
  )



;;
;; General settings
;;
(defun general-settings ()
  "Apply appearance and general editor settings."
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8)
  (setq inhibit-startup-screen t)
  (setq column-number-mode t)
  ;; Sets the fill column (where to break paragraphs on M-q)
  (setq-default fill-column 80)
  ;; set the default font to use
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono:pixelsize=14"))
  ;; (add-to-list 'default-frame-alist '(font . "Ubuntu Mono:pixelsize=16"))
  ;; Allow copy/paste to/from system clipboard
  (setq select-enable-clipboard t)
  ;; Middle mouse button inserts the clipboard (rather than emacs primary)
  (global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)
  ;; Hide vertical scrollbar on right
  (scroll-bar-mode -1)
  ;; Hide tool-bar (icons, such as open file, cut, paste, etc)
  (tool-bar-mode -1)
  ;; Display line numbers (toggle with M-x display-line-numbers-mode)
  (global-display-line-numbers-mode -1)
  ;; Make yes/no prompts shorter (y/n)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; no blinking cursor
  (blink-cursor-mode 0)
  ;; set initial frame width (in characters)
  (if (display-graphic-p)
      (setq initial-frame-alist '((width . normal-width) )))
  ;;
  ;; generic key bindings
  ;;

  ;; Comment line(s)
  (global-set-key (kbd "C-c c") 'comment-line)
  (global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
  ;; find definition of thing at point (if supported by mode)
  (global-set-key (kbd "<M-down>") 'xref-find-definitions)
  (global-set-key (kbd "<M-up>")   'xref-pop-marker-stack)
  (global-set-key (kbd "C-c f d")  'xref-find-definitions)
  (global-set-key (kbd "C-c f r")  'xref-find-references)
  ;; see if documentation can be found for thing at point
  (global-set-key (kbd "C-c C-d")  'describe-symbol)
  ;; find file (in project)
  (global-set-key (kbd "C-c f f")  'projectile-find-file)
  ;; Show matching paranthesis
  (setq show-paren-delay 0)
  (show-paren-mode 1)

  ;; Move between windows with Shift-<up|down|left|right>
  (windmove-default-keybindings))

(general-settings)

(defun vc-backend-or-nil (path)
  "Determines the type of version control (e.g. 'Git') a certain
PATH is under.  Returns nil for paths not under version control."
  (condition-case err
      (vc-responsible-backend path)
    (error
     (message "%s does not look version controlled: %s" path err)
     nil)))

(defun enable-desktop-if-started-in-git-repo ()
  "Enable desktop save mode if Emacs was started in a
version-controlled (project) directory. The desktop is saved
to/restored from ~/.emacs.d/desktops/<path>/.emacs.desktop."
  ;; Find out if directory emacs was started in is version controlled and, if
  ;; so, find the repo root dir and use that as desktop save <path>.
  (let* ((cwd default-directory)
	 (vc-backend (vc-backend-or-nil cwd)))
    (message "emacs started in directory %s" cwd)
    (when vc-backend
      (message "start directory is a %s repo" vc-backend)
      (let ((repo-root-dir (vc-call-backend vc-backend 'root cwd)))
	(message "start directory repo root is at %s" repo-root-dir)
	(message "enabling desktop save mode ...")
	(setq my-desktops-dir (expand-file-name (concat user-emacs-directory "desktops")))
	(setq desktop-dir (concat my-desktops-dir (expand-file-name repo-root-dir)))
	(message "setting desktop save dir to: %s" desktop-dir)
	(make-directory desktop-dir t)
	;; Automatic saving of the desktop when Emacs exits, and automatic
	;; restoration of the last saved desktop when Emacs starts. At start, it
	;; looks for a saved desktop in the the directories in desktop-path.
	(desktop-save-mode 1)
	;; Directory where desktop state is to be stored.
	(setq desktop-path (list desktop-dir))
	;; Save desktop on exit without prompting.
	(setq desktop-save t)
	;; Max number of buffers to restore immediately. The rest are lazily loaded.
	(setq desktop-restore-eager 10)
	(setq desktop-auto-save-timeout 30))
      ;; disable desktop-save-mode if desktop cannot be loaded (e.g. when locked
      ;; by another process)
      (add-hook 'desktop-not-loaded-hook
		(lambda ()
		  (message "desktop appears locked, disabling desktop-save-mode ...")
		  (setq desktop-save-mode nil))))))

(enable-desktop-if-started-in-git-repo)



;;;
;;; Start of custom package installation/configuration.
;;;

(require 'use-package)

;;
;; Theme-related settings
;;

(use-package diminish
  :ensure t
  :demand t)

;; make font larger/smaller globally (entire frame, not just per buffer)
(use-package default-text-scale
  :ensure t
  :demand t
  :config
  (setq default-text-scale-amount 10)
  (global-set-key (kbd "C-x C-+") 'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") 'default-text-scale-decrease)
  (global-set-key (kbd "C-x C-0") 'default-text-scale-reset))

(use-package immaterial-theme
  :ensure t
  :config
  ;; default: https://material.io/resources/color/#!/?view.left=0&view.right=0&primary.color=4DB6AC&secondary.color=AED581
  (setq immaterial-color-override-alist
	'(;; solarized dark background colors
	  ("background-primary"   . "#002b36")
	  ("background-secondary" . "#00212b")
	  ("background-tertiary"  . "#003644")
	  ;; purple
	  ("primary"         . "#b39ddb")
	  ("primary-light"   . "#e6ceff")
	  ("primary-dark"    . "#836fa9")
	  ;; bright
	  ;; ("background-primary"   . "#fafafa")
	  ;; ("background-on"        . "#cfd8dc")
	  ;; ("background-off"       . "#eeeeee")
	  ;; ("foreground-primary"   . "#263238")
	  ;; ("foreground-secondary" . "#4f5b62")
	  ;; ("foreground-tertiary"  . "#000a12")
	  ;; ("primary"              . "#311b92")
	  ;; ("primary-light"        . "#6746c3")
	  ;; ("primary-dark"         . "#000063")
	  ;; ("secondary"            . "#33691e")
	  ;; ("secondary-light"      . "#629749")
	  ;; ("secondary-dark"       . "#003d00")
	))
  (load-theme 'immaterial t))

(use-package powerline
  :ensure t
  :if window-system
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'contour))

;; highlights occurences of colors (in text) with a background of that
;; color. For example, "#aaaaaa" will be displayed with a gray background.
;; Activate via M-x rainbow-mode
(use-package rainbow-mode
  :ensure t
  :diminish
  :config
  ;; don't highlight color words such as "white", "blue"
  (setq rainbow-x-colors nil))


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
  ;; defer loading of module until this function is called *and* set up key
  ;; binding to invoke.
  :bind ([f7] . projectile-mode))

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
  ;; minimum number of letters to type before triggering autocompletion
  (setq company-minimum-prefix-length 2)
  ;; trigger completion
  (global-set-key (kbd "C-<tab>") 'company-complete)
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
  ;; show errors in current buffer
  (global-set-key (kbd "C-c s e") 'list-flycheck-errors))

;; built-in on-the-fly syntax checking (use flycheck instead)
(use-package flymake
  :diminish ; don't display on modeline
  )


;; A language template system for emacs. lsp-mode auto-configures yasnippet for
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

(defun toggle-treemacs ()
  "Enable or disable the treemacs project explorer.  When treemacs
is enabled in graphical mode, ensure that the frame width is
sufficiently large."
  (interactive)
  (treemacs)
  (when (display-graphic-p)
    (when (and (eq (treemacs-current-visibility) 'visible)
	       (< (frame-width) treemacs-min-width))
      (set-frame-width (selected-frame) treemacs-min-width))))

;; File navigator
;; Pressing '?' will show help hydra.
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-indentation 2)
  (setq treemacs-indentation-string " ")
  (setq treemacs-missing-project-action 'ask)
  ;; use textual icons
  (setq treemacs-no-png-images t)
  ;; keep only the current project expanded and all others closed.
  (setq treemacs-project-follow-cleanup t)
  ;; path where workspace state (all added projects) is saved (a separate
  ;; treemacs state file is kept for each location where emacs is opened --
  ;; assumed to be project root most of the time):
  ;; ~/.emacs.d/treemacs-persist/<working-dir>
  (setq treemacs-persist-file (concat user-emacs-directory "treemacs" (getenv "PWD") "/treemacs-persist"))
  (setq treemacs-show-hidden-files t)
  (setq treemacs-width 35)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
	       (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind
  (:map global-map
	;; hide/show treemacs file explorer
	("<f8>" . toggle-treemacs)))

;; Quickly add your projectile projects to the treemacs workspace.
(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; A small utility package to fill the small gaps left by using filewatch-mode
;; and git-mode in conjunction with magit: it will inform treemacs about
;; (un)staging of files and commits happening in magit.
(use-package treemacs-magit
  :after treemacs magit
  :ensure t)


;;;
;;; Development/coding
;;;

;; ag is an Emacs frontend to the silver searcher (ag) command-line tool.
;; The following M-x commands are available
;;   ag|ag-files|ag-regexp|ag-project|ag-project-files|ag-project-regexp
;; *-project commands detects the git root.
;; *-regexp allows PCRE patterns for the search term.
;; see: https://agel.readthedocs.io/en/latest/usage.html
;;
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  ;; reuse the same *ag* buffer for all searches
  (setq ag-reuse-buffers t)
  ;; free-text "search-in-project"
  (global-set-key (kbd "C-c s p") 'ag-project))

;; Emacs frontend for GNU global/gtags to search code tags.
;; On a soure tree run gtags from the root. Then e.g. use
;; M-x ggtags-find-definition to find a certain symbol
;; On multiple hits use M-n/M-p to navigate hits.
(use-package ggtags
  :ensure t
  :config
  ;; "find-tag", "find-type"
  (global-set-key (kbd "C-c f t") 'ggtags-find-definition))

;; Enable display-line-numbers-mode whenever we are in prog-mode
(use-package linum
  :ensure t
  :defer t
  :hook (prog-mode . display-line-numbers-mode))

;; when saving a buffer in sh-mode: untabify and delete trailing whitespace
(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
	 ("\\.env\\'" . sh-mode))
  :config
  (add-hook 'sh-mode-hook 'untabify-on-save-hook)
  (add-hook 'sh-mode-hook 'strip-on-save-hook))

(use-package lsp-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :commands lsp
  :config
  (message "lsp-mode config ...")
  ;; If non-nil, print all messages to/from lang server in *lsp-log*.
  (setq lsp-log-io t)
  ;; Define whether all of the returned by document/onHover will be displayed.
  ;; If set to nil eldoc will show only the symbol information.
  (setq lsp-eldoc-render-all nil)
  ;; prefer flymake over lsp-ui if both are present
  (setq lsp-prefer-flymake t)
  ;; Set to t to have eldoc display hover info when present.
  (setq lsp-eldoc-enable-hover nil)
  ;; Seconds to wait for a response from the language server before timing out.
  (setq lsp-response-timeout 5))

(use-package lsp-ui
  :ensure t
  ;; gets started by lsp-mode
  :commands lsp-ui-mode
  :config
  ;; display information about symbols on the current line as we type?
  (setq lsp-ui-sideline-enable nil)
  ;; indicate if lsp-ui-doc should be rendered on hover at every symbol. if nil
  ;; `(lsp-ui-doc-show)` can still be used to open the docs for a symbol.
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-max-width 70)
  (setq lsp-ui-doc-delay 0.0)
  ;; disaply doc in a WebKit widget?
  (setq lsp-ui-doc-use-webkit nil)
  ;; enable lsp-ui-peek feature: M-x lsp-ui-peek-find-{references,definitions}
  (setq lsp-ui-peek-enable t)
  ;; show peek view even if there is only one candidate
  (setq lsp-ui-peek-always-show t)
  ;; keybindings for Language Server Protocol features
  ;; NOTE: these can be overridden by keymaps for a particular major mode
  ;;       such as "C-c C" in c++-mode. If so, disable the keybinding locally
  ;;       in that mode's keymap.
  (global-set-key (kbd "<M-down>") 'lsp-find-definition)
  (global-set-key (kbd "<M-up>")   'xref-pop-marker-stack)
  (global-set-key (kbd "C-c p d")  'lsp-ui-peek-find-definitions)
  (global-set-key (kbd "C-c p r")  'lsp-ui-peek-find-references)
  (global-set-key (kbd "C-c h")    'lsp-document-highlight)
  (global-set-key (kbd "C-c f d")  'lsp-find-definition)
  (global-set-key (kbd "C-c f i")  'lsp-goto-implementation)
  (global-set-key (kbd "C-c f r")  'lsp-find-references)
  (global-set-key (kbd "C-c C-r")  'lsp-rename)
  (global-set-key (kbd "C-c d")    'lsp-ui-doc-show)
  (global-set-key (kbd "C-c e")    'lsp-ui-doc-hide) ; "end doc show"
  (global-set-key (kbd "C-c C-d")  'lsp-describe-thing-at-point))

(use-package company-lsp
  :ensure t
  ;; gets started by lsp-mode
  :commands company-lsp
  :config
  ;; add lsp as company completion engine backend to get completion-at-point
  (push 'company-lsp company-backends))

;; treemacs LSP integration. provides a few functions to enable views:
;; - (lsp-treemacs-errors-list): tree-like error list.
;; - (lsp-treemacs-symbols): open a view that shows symbols declared in buffer
(use-package lsp-treemacs
  :ensure t
  ;; defer loading of module until any of these functions are called *and* set
  ;; up key bindings to invoke them.
  :bind (("C-c t s" . lsp-treemacs-symbols)
	 ("C-c t e" . lsp-treemacs-errors-list))
  :config)

;; Use microsoft's (dotnet-based) language server for python (appears to be
;; better than the default pyls). Downloads the language server under
;; ~/.emacs.d/mspyls.
(use-package lsp-python-ms
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook (lambda () (require 'lsp-python-ms) (lsp)))
  :config
  ;; language server to download
  (setq lsp-python-ms-nupkg-channel "stable")
  ;; override python-mode keybindings
  (bind-keys*
   ("C-c C-d" . lsp-describe-thing-at-point)
   ("C-c C-r" . lsp-rename)))

(use-package python
  ;; note: no :ensure since it is already built into emacs
  :config
  (message "python buffer setup hook ...")

  ;; no tabs for indentation
  (setq indent-tabs-mode nil)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'python-mode-hook 'untabify-on-save-hook)
  (add-hook 'python-mode-hook 'strip-on-save-hook))

;; Use sphinx-doc when python-mode is activated. Gives a templated docstring
;; when pressing C-c M-d in function head.
(use-package sphinx-doc
  :ensure t
  :commands python-mode
  :config
  (sphinx-doc-mode))


(use-package go-mode
  :ensure t
  :defer t
  :mode (("\\.go\\'" . go-mode))
  :config
  (message "go-mode config ...")
  ;; run gofmt (or actually, goimports) on save
  ;; note: requires ${GOROOT}/bin to be on PATH
  (setq gofmt-command "goimports")
  ;; Reuse a single *godoc* buffer to display godoc-at-point calls.
  (setq godoc-reuse-buffer t)
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; start lsp-mode
  ;; NOTE: relies on gopls lsp server being on the PATH
  (add-hook 'go-mode-hook 'lsp))


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
  (add-hook 'json-mode-hook 'display-line-numbers-mode) ; show line numbers

  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'json-mode-hook 'untabify-on-save-hook)
  (add-hook 'json-mode-hook 'strip-on-save-hook))


;; Major mode for yaml file editing.
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yaml\\'" . yaml-mode)
	 ("\\.yml\\'" . yaml-mode))
  :config
  (message "yaml buffer config ...")
  (setq indent-tabs-mode nil) ; no tabs for indentation
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'yaml-mode-hook 'untabify-on-save-hook)
  (add-hook 'yaml-mode-hook 'strip-on-save-hook))

;; Major mode for markdown (.md) file editing.
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
	 ;; cheat sheets under ~/dotfiles/cheat/sheets
	 ("\\.cheat\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  ;; no tabs for indentation
  (setq indent-tabs-mode nil)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'markdown-mode-hook 'untabify-on-save-hook))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :config
  (global-set-key (kbd "C-c p m")  'markdown-preview-mode)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-preview-stylesheets
	(list
	 ;; style very similar to github's for markdown rendering
	 "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"
	 ;; style that adds some margins
	 "https://petergardfjall.github.io/css/emacs-markdown/github-markdown-body.css"
	 ;; style for syntax highlighting of fenced codeblocks
	 "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"))
  (setq markdown-preview-javascript
	(list
	 ;; javascript lib for syntax highlighting of fenced code blocks
	 "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
	 ;; javascript that applies the highlight js lib to the doc
	 "https://petergardfjall.github.io/js/emacs-markdown/github-markdown-block-highlight.js")))

;; Varnish .vcl file editing.
(use-package vcl-mode
  :ensure t
  :defer t
  :mode (("\\.vcl\\'" . vcl-mode))
  :config
  (add-hook 'vcl-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'vcl-mode-hook 'untabify-on-save-hook)
  (add-hook 'vcl-mode-hook 'strip-on-save-hook))

;; Dockerfile editing
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-mode))
  :config
  (add-hook 'dockerfile-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'dockerfile-mode-hook 'untabify-on-save-hook)
  (add-hook 'dockerfile-mode-hook 'strip-on-save-hook))

;; TOML editing
(use-package toml-mode
  :ensure t
  :defer t
  :mode (("\\.toml\\'" . toml-mode))
  :config
  (add-hook 'toml-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'toml-mode-hook 'untabify-on-save-hook)
  (add-hook 'toml-mode-hook 'strip-on-save-hook))

(use-package terraform-mode
  :ensure t
  :defer t
  :mode (("\\.tf\\'" . terraform-mode))
  :config
  (message "terraform-mode config ...")
  (add-hook 'terraform-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'terraform-mode-hook 'untabify-on-save-hook)
  (add-hook 'terraform-mode-hook 'strip-on-save-hook))

(use-package protobuf-mode
  :ensure t
  :defer t
  :mode (("\\.proto\\'" . protobuf-mode))
  :config
  (message "protobuf-mode config ...")
  (add-hook 'protobuf-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'protobuf-mode-hook 'untabify-on-save-hook)
  (add-hook 'protobuf-mode-hook 'strip-on-save-hook))

;; Rust-mode
(use-package rust-mode
  :ensure t
  :defer t
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (message "rust-mode config ...")
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
  (setq ccls-executable "/opt/bin/ccls")
  ;; disable completion cache
  (setq company-transformers nil)
  (setq company-lsp-async t)
  (setq company-lsp-cache-candidates nil)
  ;; disable keymap bindings that would override lsp ones.
  (define-key c++-mode-map (kbd "C-c C-d") nil)
  (define-key c-mode-map (kbd "C-c C-d") nil)
  ;; For proper operation, a .ccls or compile_commands.json file is needed in
  ;; the project root.
  ;; For CMake projects, a compile_commands.json is created via:
  ;;   mkdir build
  ;;   (cd build; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES ...)
  ;;   ln -s build/compile_commands.json
  (use-package clang-format
    :ensure t)
  ;; add buffer-local save hooks
  (add-hook 'before-save-hook 'clang-format-buffer nil t))


(use-package lsp-java
  :ensure t
  :defer t
  :hook ((java-mode) .
	 (lambda () (require 'lsp-java) (lsp)))
  :config
  (message "lsp-java config ...")
  ;; disable completion cache
  (setq company-lsp-cache-candidates nil)
  ;; disable keymap bindings that would override lsp ones.
  (define-key java-mode-map (kbd "C-c C-d") nil)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'java-mode-hook 'untabify-on-save-hook)
  (add-hook 'java-mode-hook 'strip-on-save-hook))

;; remove "ElDoc" from modeline
(use-package eldoc
  :diminish eldoc-mode)

;; can be used for working with .groovy and Jenkinsfile
(use-package groovy-mode
  :ensure t
  :defer t
  :commands (groovy-mode))

;; emacs mode to edit GraphQL schemas and queries (automtically enabled when
;; opening .graphql and .gql files)
(use-package graphql-mode
  :ensure t
  :defer t)


;;; Finalization


;; Output the time at which the loading of the init-file itself completed.
(message "Loaded %s after %.3fs." load-file-name (elapsed-time))

;; Output the time at which the loading of all init-hooks completed.
(add-hook 'after-init-hook
	  (lambda () (message "init-hooks done after %.3fs." (elapsed-time)))
	  t)
