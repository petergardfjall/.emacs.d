;;; emacs-init.el --- Main entry-point for emacs configuration.
;;; Commentary:
;;;
;;; Makes heavy use of the use-package macro.  For bootstrapping, and installing
;;; the use-package package a procedure similar to this is followed:
;;; http://wikemacs.org/wiki/Package.el

;;; Code:

(message "Loaded early-init.el after %.3fs." my-early-init-duration)

;; Time for starting to load this file.
(defconst emacs-start-time (current-time))

;;
;; Declarations
;;

(defvar my-frame-width 85
  "Initial width of Emacs frame.")
(defvar my-treemacs-min-width 120
  "Minimum frame width when treemacs is enabled (in characters).")
(defvar my-font "Roboto Mono"
  "Text font to use.
For example, `Source Code Pro`, `Ubuntu Mono`,`Cousine`, `JetBrains Mono`).")
(defvar my-font-size 10.5 "Font size to use in points (for example, 10.5).")

(defvar my-emacs-start-dir default-directory
  "Directory from which Emacs was launched.")

;;
;; Tricks to reduce startup time. These need to be set at an early stage.
;;

;;
;; avoid GC performance-penalty on startup by temporarily bumping the memory
;; threshold for GC. This effectively defers garbage collection.
;;
(defvar gc-cons-threshold-custom (* (expt 2 7) gc-cons-threshold)
  "Garbage collection memory threshold to use after init.")
;; set high (256 mb) temporarily during init
(setq gc-cons-threshold 268435456)

;; Adjust default setting default (4K) to allow emacs to read larger output
;; chunks from processes. Language server responses can be in the order of a MB.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; don't run (package-initialize)
(setq package-enable-at-startup nil)
;; at startup we don't want emacs to look for a handler for every opened file.
(defvar file-name-handler-alist-default file-name-handler-alist)
(setq file-name-handler-alist nil)

;; re-set temporarily disabled features once init is complete.
(add-hook 'after-init-hook
          (lambda ()
            ;; set sensible GC threshold.
            (setq gc-cons-threshold gc-cons-threshold-custom)
            ;; re-enable file handler associations.
            (setq file-name-handler-alist file-name-handler-alist-default)))

;; suppress warnings from asynchronous native compilation
(setq native-comp-async-report-warnings-errors nil)

;;
;; Utility functions
;;

(defun my-bootstrap-straight-el ()
  "Ensures straight.el is installed and loaded."
  (message "bootstrapping straight.el ...")
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (message "loading straight.el bootstrapper ...")
    (load bootstrap-file nil 'nomessage)))

(defun my-find-project-root (path)
  "Return the project root directory for PATH.
If PATH is not in a version-controlled directory, nil is returned."
  (let* ((proj (project-try-vc path)))
    (if proj
	(project-root proj)
      nil)))

(defun my-project-root ()
  "Return the project root directory of the current buffer.
If the buffer is visiting a file not in a project directory, the
buffer's directory is returned."
  (if (project-current)
      (project-root (project-current))
    default-directory))

(defun my-elapsed-time ()
  "Get the elapsed time since start of loading."
  (float-time (time-subtract (current-time) emacs-start-time)))

(defun my-untabify-buffer ()
  "Run 'untabify' on the whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))


(defun my-untabify-on-save-hook ()
  "Register a buffer-local 'before-save-hook' to run 'generic-fmt-buffer'."
  ;; note: last argument makes this save-hook local to the buffer
  (add-hook 'before-save-hook #'my-untabify-buffer nil t))


(defun my-strip-on-save-hook ()
  "Register a buffer-local 'before-save-hook' to run 'delete-trailing-whitespace'."
  ;; note: last argument makes this save-hook local to the buffer
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))


(defun my-close-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(defun my-rename-file-and-buffer ()
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


(defun my-set-default-font-height ()
  "Reset the font height for the selected frame to the default font size."
  (interactive)
  ;; calculate delta between current face height and default font height and
  ;; apply difference.
  (let* ((font-height (face-attribute 'default :height))
         (default-font-height (truncate (* 10 my-font-size)))
         (delta-to-default (- default-font-height font-height)))
    (default-text-scale-increment delta-to-default)))


(defun my-toggle-treemacs ()
  "Enable or disable the treemacs project explorer.
When Treemacs is enabled in graphical mode, ensure that the frame
width is sufficiently large."
  (interactive)
  (treemacs)
  (when (display-graphic-p)
    (when (and (eq (treemacs-current-visibility) 'visible)
               (< (frame-width) my-treemacs-min-width))
      (set-frame-width (selected-frame) my-treemacs-min-width))))


(defun my-color-lighten (hex-color percent)
  "Determine a brighter/darker shade of a hex color.
For a HEX-COLOR (such as `#3cb878`) return the hex color that is
PERCENT percent brighter (or darker if percent value is
negative)."
  (interactive "sHex color: \nnPercent brighter/darker (-): ")
  (let ((color-transform-fn (if (> percent 0)
                                'color-lighten-hsl
                              'color-darken-hsl))
        (percent-unsigned (abs percent)))
    (message "%s"
     (apply 'color-rgb-to-hex
            (append
             (apply 'color-hsl-to-rgb
                    (apply color-transform-fn
                           (append
                            (apply 'color-rgb-to-hsl (color-name-to-rgb hex-color))
                            (list percent-unsigned))))
             (list 2))))))

(defun my-enable-line-numbers-mode ()
  "Enable display-line-numbers-mode in buffer."
  (display-line-numbers-mode 1))

(defun my-disable-line-numbers-mode ()
  "Disable display-line-numbers-mode in buffer."
  (display-line-numbers-mode -1))

(defun my-enable-orgtbl-mode ()
  "Enable orgtbl-mode. orgtbl-mode is a minor mode that makes
Org-modes table editor commands available."
  (require 'org)
  (orgtbl-mode 1))

(defun my-highlight-todos ()
  "Mark occurences of TODO with warning face."
  (font-lock-add-keywords nil
    '(("\\(TODO\\)" 1 'font-lock-warning-face prepend)) 'append))

(defun my-byte-offset ()
  "Report the byte offset (0-indexed) at point (cursor position)."
  (interactive)
  (message "byte offset: %d" (1- (position-bytes (point)))))

;;
;; Start of actual initialization.
;;

(message "Loading %s ..." load-file-name)

;; Ensure that straight.el is installed and loaded.  Instead of the package.el
;; package manager that comes bundled with Emacs, we use straight.el to manage
;; packages with better control over package versions.
;;
;; To get a consistent and repeatable configuration across multiple machines,
;; one can create a "lockfile" (pinning each package to a specific commit) with
;; the `M-x straight-freeze-versions` command. It generates
;; `straight/versions/default.el` which can then be version controlled.
(my-bootstrap-straight-el)

;; Install and load use-package.
(straight-use-package 'use-package)
(setq use-package-verbose nil) ;; set to t to see when packages are loaded

;;
;; General settings
;;
(defun my-general-settings ()
  "Apply appearance and general editor settings."

  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8)
  ;; system locale for formatting time values -- ensures that weekdays in the
  ;; org-mode timestamps appear in English.
  (setq system-time-locale "C")

  (setq column-number-mode t)
  ;; wrap long lines
  (set-default 'truncate-lines nil)
  ;; Sets the fill column (where to break paragraphs on M-q)
  (setq-default fill-column 80)
  ;; set the default font to use on all frames (see
  ;; https://www.freedesktop.org/software/fontconfig/fontconfig-user.html)
  (add-to-list 'default-frame-alist `(font . ,(format "%s-%f" my-font my-font-size)))
  ;; Allow copy/paste to/from system clipboard
  (setq select-enable-clipboard t)
  ;; Middle mouse button inserts the clipboard (rather than emacs primary)
  (global-set-key (kbd "<mouse-2>") #'x-clipboard-yank)

  ;; No sudden jumps when cursor moves off top/bottom of screen. If the value is
  ;; greater than 100, redisplay will never recenter point, but will always
  ;; scroll just enough text to bring point into view
  ;; (setq scroll-conservatively 101)

  ;; enable line numbers in all text-mode/prog-mode buffers
  (add-hook 'text-mode-hook    #'my-enable-line-numbers-mode)
  (add-hook 'prog-mode-hook    #'my-enable-line-numbers-mode)
  ;; disable line numbers in special mode buffers (magit, treemacs)
  (add-hook 'special-mode-hook #'my-disable-line-numbers-mode)

  (add-hook 'prog-mode-hook #'my-highlight-todos)

  ;; highlight the current line
  (global-hl-line-mode t)
  ;; disable current line highlighting while selecting/marking a region
  (add-hook 'activate-mark-hook (lambda () (global-hl-line-mode 0)))
  (add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode t)))

  ;; Make yes/no prompts shorter (y/n)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; cursor appearance, default is 'box.
  (set-default 'cursor-type 'box)
  ;; blinking cursor?
  (blink-cursor-mode 0)
  ;; set initial frame width (in characters)
  (if (display-graphic-p)
      (setq initial-frame-alist `((width . ,my-frame-width))))
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

  ;;
  ;; generic key bindings
  ;;

  ;; unbind unneeded/disturbing keybindings
  (global-set-key (kbd "C-t") nil) ;; transpose characters
  (global-set-key (kbd "M-t") nil) ;; transpose words
  (global-set-key (kbd "M-h") nil) ;; mark-paragraph

  ;; Comment line(s)
  (global-set-key (kbd "C-c c") #'comment-line)
  (global-set-key (kbd "C-c w") #'delete-trailing-whitespace)
  ;; find definition of thing at point (if supported by mode)
  (global-set-key (kbd "<M-down>") #'xref-find-definitions)
  (global-set-key (kbd "<M-up>")   #'xref-pop-marker-stack)
  (global-set-key (kbd "C-c f d")  #'xref-find-definitions)
  (global-set-key (kbd "C-c f r")  #'xref-find-references)
  ;; see if documentation can be found for thing at point
  (global-set-key (kbd "C-c C-d")  #'describe-symbol)

  ;; Move between windows with C-x w <up|down|left|right>
  (global-set-key (kbd "C-x w <up>")    #'windmove-up)
  (global-set-key (kbd "C-x w <down>")  #'windmove-down)
  (global-set-key (kbd "C-x w <right>") #'windmove-right)
  (global-set-key (kbd "C-x w <left>")  #'windmove-left))




(my-general-settings)


;;
;; Start of custom package installation/configuration.
;;

;;
;; Theme-related settings
;;

;; Used to selectively hide mode-line display of certain minor modes. For
;; example: (use-package :diminish).
(use-package diminish
  :straight t
  :demand t)

;; make font larger/smaller globally (entire frame, not just per buffer)
(use-package default-text-scale
  :straight t
  :demand t
  :config
  ;; increment delta (in tenths of points), so needs to be divisible by 10.
  (setq default-text-scale-amount 10)
  (global-set-key (kbd "C-x C-+") #'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") #'default-text-scale-decrease)
  (global-set-key (kbd "C-x C-0") #'my-set-default-font-height))

(use-package immaterial-theme
  :straight (immaterial-theme
	     :type git :host github
	     :repo "petergardfjall/emacs-immaterial-theme"
	     :branch "master")
  :config
  (load-theme 'immaterial-dark t))

;;
;; completing-read setup: vertico + consult + orderless + marginalia
;;
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :config
  ;; enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; use page-up/down to move one page up/down among completion candidates
  (define-key vertico-map (kbd "<next>") #'scroll-up-command)
  (define-key vertico-map (kbd "<prior>") #'scroll-down-command)
  (define-key vertico-map "?" #'minibuffer-completion-help)
  (define-key vertico-map (kbd "TAB") #'minibuffer-complete))

;; This package provides an orderless completion style that divides the pattern
;; into space-separated components, and matches candidates that match all of the
;; components in any order. Each component can match in any one of several ways:
;; literally, as a regexp, as an initialism, in the flex style, or as multiple
;; word prefixes. By default, regexp and literal matches are enabled. Works with
;; the built-in icomplete package or with some third party minibuffer completion
;; frameworks such as Vertico and Selectrum.
(use-package orderless
  :straight t
  :init
  (setq
   completion-styles '(partial-completion orderless))
  :config
  ;; use orderless for lsp completion. See
  ;; https://github.com/minad/corfu/issues/41#issuecomment-974724805
  (add-hook 'lsp-completion-mode-hook
	    (lambda ()
	      (setf (alist-get 'styles
			       (alist-get 'lsp-capf completion-category-defaults))
		    '(orderless)))))


(use-package consult
  :straight t
  :bind (("C-x b"   . consult-buffer)    ;; switch-to-buffer
	 ("M-g g"   . consult-goto-line) ;; goto-line
	 ("C-s"     . consult-line)      ;; isearch
	 ;; "search git": free-text search in version-controlled files
	 ("C-c s g" . consult-git-grep)
	 ;; "search project": free-text search in all project files
	 ("C-c s p" . consult-ripgrep))
  :init
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Needed to make consult project-aware (for example for `consult-grep`).
  (setq consult-project-root-function #'my-project-root)

  ;; Some consult commands support live previews. Here we disable them for cases
  ;; where we don't want automatic live preview of a selected
  ;; candidate. Instead, make this preview manually triggered by M-.
  (consult-customize
   consult-buffer ;; can add more space-separated commands here
   :preview-key (kbd "M-.")))

;; uses consult to display/select lsp-provided symbols and diagnostics.
(use-package consult-lsp
  :straight t
  :after lsp-mode
  :config
  ;; additional decorations for candidates
  (consult-lsp-marginalia-mode +1)
  ;; select diagnostics from the current lsp workspace
  (define-key lsp-mode-map (kbd "C-c l d") #'consult-lsp-diagnostics)
  ;; select symbols from the current lsp workspace
  (define-key lsp-mode-map (kbd "C-c l w") #'consult-lsp-symbols)
  ;; select symbols from the current lsp file
  (define-key lsp-mode-map (kbd "C-c l s") #'consult-lsp-file-symbols))

;;
;; Adds richer annotations for minibuffer completions for any completing-read
;; compatible framework (such as selectrum or vertico). Similar to ivy-rich.
(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode)

  (defun my-project-buffer-annotator (cand)
    (let* ((buffer (get-buffer cand)))
      (when-let (buffer-file (buffer-file-name buffer))
	(when-let ((project-dir (my-find-project-root buffer-file)))
	  (let* ((project-short (file-name-base (directory-file-name project-dir)))
		 (project-rel-dir (file-name-directory (file-relative-name buffer-file project-dir))))
	    (marginalia--fields
	     (project-short :truncate 0.4 :face 'marginalia-value)
	     (project-rel-dir :truncate 0.4 :face 'marginalia-documentation)))))))

  ;; update annotator-registry to use my custom annotator for buffers
  (add-to-list 'marginalia-annotator-registry
               '(buffer my-project-buffer-annotator none))
  )



;; highlights occurences of colors (in text) with a background of that
;; color. For example, "#aaaaaa" will be displayed with a gray background.
;; Activate via M-x rainbow-mode
(use-package rainbow-mode
  :straight t
  :commands (rainbow-mode)
  :diminish
  :config
  ;; don't highlight color words such as "white", "blue"
  (setq rainbow-x-colors nil))

(use-package undo-tree
  :straight t
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

;; built-in project.el
(use-package project
  :config
  (define-key global-map (kbd "C-c f f") #'project-find-file))

(use-package wsp
  :straight (emacs-wsp :type git :host github
		       :repo "petergardfjall/emacs-wsp")
  :bind (("C-x w o"   . wsp-workspace-open)
	 ("C-x w k"   . wsp-workspace-close)
	 ("C-x w c"   . wsp-workspace-current)
	 ("C-x w d"   . wsp-workspace-delete)
	 ("C-x w p a" . wsp-project-add)
	 ("C-x w p d" . wsp-project-delete)
	 ("C-x w p s" . wsp-project-switch)
	 ("C-x w p c" . wsp-project-close)
	 ("C-x w p k" . wsp-project-close-current)
	 ("C-x w p K" . wsp-project-close-other)))

(use-package postrace
  :straight (postrace :type git :host github
		      :repo "petergardfjall/emacs-postrace"
		      :branch "initial-version")
  :bind (("C-c p p" . postrace-push)
	 ("C-c p b" . postrace-browse)))


;; allows definition of hydras - families of commands with a common prefix
(use-package hydra
  :straight t
  :config

  ;; window navigation/resizing hydra
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

;; Transparent Remote Access, Multiple Protocols -- edit remote files
(use-package tramp
  :defer 5 ;; wait 5 seconds before loading
  :config
  ;; default method for transferring files (scp, ssh)
  (customize-set-variable 'tramp-default-method "ssh"))


;; completion-at-point UI. Candidates are shown in a popup frame. Completions
;; are provided by commands like `dabbrev-completion' or
;; `completion-at-point-functions' (capf).
(use-package corfu
  :straight t
  :init
  (corfu-global-mode)
  :config
  (setq
   ;; enable auto-completion
   corfu-auto t
   ;; minimum prefix length before triggering auto-completion
   corfu-auto-prefix 1
   ;; delay in seconds after typing until popup appears
   corfu-auto-delay 0.0
   ;; show candidate documentation in echo area
   corfu-echo-documentation t
   ;; quit when no remaining candidates
   corfu-quit-no-match t
   ;; maximum number of candidates to display
   corfu-count 15
   ;; miniumum popup width (in characters)
   corfu-min-width 20)

  ;; trigger completion
  (define-key global-map (kbd "C-<tab>") #'completion-at-point))

(use-package dabbrev)

;; adds `completion-at-point-functions', used by `completion-at-point'.
(use-package cape
  :straight t
  :init
  ;; complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; built-in on-the-fly syntax checking, which highlights erroneous lines.
(use-package flymake
  :diminish ;; don't display on modeline
  :hook ((prog-mode text-mode) . flymake-mode)
  :config
  ;; "show errors in project"
  (define-key flymake-mode-map (kbd "C-c s e p")
	      #'flymake-show-project-diagnostics)
  ;; "show show errors in file"
  (if (fboundp #'consult-flymake)
      (define-key flymake-mode-map (kbd "C-c s e f")
	      #'consult-flymake)
    (define-key flymake-mode-map (kbd "C-c s e f")
		#'flymake-show-buffer-diagnostics)))


;; built-in on-the-fly spell checking for text or code comments.
(use-package flyspell
  :disabled
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

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


;; A Git porcelain inside Emacs.
(use-package magit
  :straight t
  ;; defer loading of module until any of these functions are called *and* set
  ;; up key bindings to invoke them.
  :bind (("C-x g" . magit-status)))

;; Highlight diffs (in the fringe) for version-controlled buffers.
(use-package diff-hl
  :straight t
  :defer 5
  :diminish
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :config
  ;; refresh if magit does an update
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; File navigator
;; Pressing '?' will show help hydra.
(use-package treemacs
  :straight t
  :defer t
  :bind (:map treemacs-mode-map
	      ;; disable treemacs workspace keymap (C-c C-w ..), since it
	      ;; conflicts with hydra-windows
	      ("C-c C-w" . nil))
  :config
  (setq treemacs-indentation 2)
  (setq treemacs-indentation-string " ")
  (setq treemacs-missing-project-action 'ask)
  ;; use textual icons
  (setq treemacs-no-png-images t)
  ;; if true: keep only the current project expanded and all others closed.
  (setq treemacs-project-follow-cleanup nil)
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
        ("<f8>" . my-toggle-treemacs)))


;; Quickly add your projectile projects to the treemacs workspace.
(use-package treemacs-projectile
  :straight t
  :after treemacs projectile)

;; A small utility package to fill the small gaps left by using filewatch-mode
;; and git-mode in conjunction with magit: it will inform treemacs about
;; (un)staging of files and commits happening in magit.
(use-package treemacs-magit
  :straight t
  :after treemacs magit)

;; transparently open compressed files
(use-package auto-compression-mode
  :defer 5
  :config
  (auto-compression-mode t))

;;;
;;; Development/coding
;;;

(defun my-ggtags-find-definition  ()
  "Prompting replacement for `ggtags-find-definition`.
This function always prompts (default behavior is to just search
for symbol at point if there is one)."
  (interactive)
  ;; can read 'definition, 'symbol, 'reference, 'id, 'path
  (let ((tag (ggtags-read-tag 'definition t "Find definition [GTAGS]")))
    (ggtags-find-definition tag)))

(defun my-ggtags-find-reference  ()
  "Prompting replacement for `ggtags-find-reference`.
This function always prompts (default behavior is to just search
for symbol at point if there is one)."
  (interactive)
  ;; can read 'definition, 'symbol, 'reference, 'id, 'path
  (let ((ref (ggtags-read-tag 'reference t "Find reference [GTAGS]")))
    (ggtags-find-reference ref)))

(defun my-ggtags-create ()
  "Create GTAGS in the root directory of the current buffer's project."
  (interactive)
  (if-let (proj-root (my-project-root))
      (progn
        (message "Generating tags in %s ..." proj-root)
        (ggtags-create-tags proj-root))
    (error "Did not find a project root dir in which to generate tags")))

;; Emacs frontend for GNU global/gtags to generate and search code tags.
;; Wraps the 'gtags' and 'global' command-line tools.
(use-package ggtags
  :straight t
  ;; add ggtags as a xref backend in emacs-lisp-mode (xref-find-definitions)
  :hook ((emacs-lisp-mode . ggtags-mode))
  ;; set up keybindings to generate and search GTAGS
  :bind (("C-c t c"   . my-ggtags-create)
	 ("C-c t f d" . my-ggtags-find-definition)
	 ("C-c t f r" . my-ggtags-find-reference))
  :config
  ;; interferes with beginning/end of buffer key bindings
  (define-key ggtags-navigation-map (kbd "M->") nil)
  (define-key ggtags-navigation-map (kbd "M-<") nil))


;; when saving a buffer in sh-mode: untabify and delete trailing whitespace
(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.env\\'" . sh-mode))
  :config
  ;; use bash-language-server (installed separately via npm)
  ;; (lsp-deferred)
  (add-hook 'sh-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'sh-mode-hook 'my-strip-on-save-hook))

(use-package lsp-mode
  :straight t
  :defer t
  :commands (lsp lsp-deferred)
  :config
  (message "lsp-mode config ...")
  ;; automatically find detect and configure lsp-ui and company-lsp
  (setq lsp-auto-configure t)
  ;; kill an LSP server when there are no open buffers
  (setq lsp-keep-workspace-alive nil)

  ;; If non-nil, print all messages to/from lang server in *lsp-log*.
  ;; Note: comes at a high performance cost.
  (setq lsp-log-io nil)
  ;; Define whether all of the returned by document/onHover will be displayed.
  ;; If set to nil eldoc will show only the symbol information.
  (setq lsp-eldoc-render-all nil)
  ;; package used to show diagnostics
  (setq lsp-diagnostics-provider :flymake)
  ;; do not autoconfigure company-mode for completions (use capf/corfu)
  (setq lsp-completion-provider :none)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-use-last-result t)
  (setq lsp-completion-no-cache nil)
  ;; Set to t to have eldoc display hover info when present.
  (setq lsp-eldoc-enable-hover nil)
  ;; Seconds to wait for a response from the language server before timing out.
  (setq lsp-response-timeout 5)
  ;; temporary fix for https://github.com/emacs-lsp/lsp-mode/issues/1778
  (setq lsp-gopls-codelens nil)
  ;; do not make references in files clickable. should fix:
  ;; https://github.com/Alexander-Miller/treemacs/issues/626
  (setq lsp-enable-links nil)
  ;; don't show file path breadcrumb at top of window
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; file-watcher ignored directories
  (push "[/\\\\]_build$" lsp-file-watch-ignored-directories)
  (push "[/\\\\].venv$" lsp-file-watch-ignored-directories)

  ;; keybindings for Language Server Protocol features
  (define-key lsp-mode-map (kbd "<M-down>") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "<M-up>")   #'xref-pop-marker-stack)
  (define-key lsp-mode-map (kbd "C-c h")    #'lsp-document-highlight)
  (define-key lsp-mode-map (kbd "C-c f d")  #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c f i")  #'lsp-goto-implementation)
  (define-key lsp-mode-map (kbd "C-c f r")  #'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-c C-r")  #'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c C-d")  #'lsp-describe-thing-at-point))


(use-package lsp-ui
  :straight t
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
  (setq lsp-ui-doc-position 'top)
  ;; disaply doc in a WebKit widget?
  (setq lsp-ui-doc-use-webkit nil)
  ;; enable lsp-ui-peek feature: M-x lsp-ui-peek-find-{references,definitions}
  (setq lsp-ui-peek-enable t)
  ;; show peek view even if there is only one candidate
  (setq lsp-ui-peek-always-show t)
  ;; lsp-ui specific keybindings
  (define-key lsp-mode-map (kbd "C-c p d") #'lsp-ui-peek-find-definitions)
  (define-key lsp-mode-map (kbd "C-c p r") #'lsp-ui-peek-find-references)
  (define-key lsp-mode-map (kbd "C-c d")   #'lsp-ui-doc-show)
  (define-key lsp-mode-map (kbd "C-c e")   #'lsp-ui-doc-hide) ; "end show"
  )

;; treemacs LSP integration. provides a few functions to enable views:
;; - (lsp-treemacs-errors-list): tree-like error list.
;; - (lsp-treemacs-symbols): open a view that shows symbols declared in buffer
(use-package lsp-treemacs
  :straight t
  ;; defer loading of module until any of these functions are called *and* set
  ;; up key bindings to invoke them.
  :bind (("C-c t s" . lsp-treemacs-symbols)
         ("C-c t e" . lsp-treemacs-errors-list))
  :config)

;; Client library for Debug Adapter Protocol (DAP). Similar to LSP, but
;; integrates with debug servers.
;; Note: enable individual language support via `dap-<language>` packages.
(use-package dap-mode
  :straight t
  :commands (dap-debug dap-debug-edit-emplate)
  :hook (
	 ;; Note: if debugging test files: use "Go Launch File Configuration"
	 ;; otherwise: use "Go Launch Unoptimized Debug Package Configuration"
	 (go-mode . (lambda () (require 'dap-go) (dap-go-setup)))
	 (python-mode . (lambda () (require 'dap-python))))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  ;; mouse hover support
  (dap-tooltip-mode 1)
  ;; tooltips on mouse hover
  ;; if it is not enabled `dap-mode` will use the minibuffer.
  (tooltip-mode 1)
  ;; display floating panel with debug buttons
  (dap-ui-controls-mode 1)

  ;; enable/disable output to `*Messages*` buffer
  (setq dap-print-io t)
  ;; trigger hydra when a debugged program hits a breakpoint
  (add-hook 'dap-stopped-hook
	    (lambda (arg) (call-interactively #'dap-hydra)))

  ;; dap-python: seems required in order for dap-mode to find pyenv-provided
  ;; python executable. see:
  ;; https://github.com/emacs-lsp/dap-mode/issues/126#issuecomment-754282754
  (defun dap-python--pyenv-executable-find (command)
    (executable-find command)))


;; Use microsoft's (nodejs-based) `pyright` language server for python.
(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :config
  ;; don't watch files in .venv
  (push "[/\\\\]\\.venv$" lsp-file-watch-ignored))

(use-package python
  :mode (("\\.py\\'" . python-mode))
  ;; note: no :ensure since it is already built into emacs
  :config
  (message "python buffer setup hook ...")

  ;; no tabs for indentation
  (setq indent-tabs-mode nil)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'python-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'python-mode-hook 'my-strip-on-save-hook))

;; Use sphinx-doc when python-mode is activated. Gives a templated docstring
;; when pressing C-c M-d in function head.
(use-package sphinx-doc
  :straight t
  :commands python-mode
  :config
  (sphinx-doc-mode))

(use-package go-mode
  :straight t
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
  (add-hook 'go-mode-hook #'lsp-deferred))

;; golangci-lint support via flymake
(use-package flymake-golangci
  :straight t
  :hook (go-mode . flymake-golangci-load))


;; Major mode for json file editing.
(use-package json-mode
  :straight t
  :defer t
  :mode (("\\.json\\'" . json-mode))
  :config
  (message "json buffer config ...")
  (setq
   indent-tabs-mode nil
   js-indent-level 2) ; use 2 space indentation
  (setq indent-tabs-mode nil) ; no tabs for indentation
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'json-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'json-mode-hook 'my-strip-on-save-hook))

;; Major mode for JavaScript and React/JSX (built-into Emacs).
;; `js-mode` comes with syntax highlighting/indent support for JSX.
(use-package js
  :mode (("\\.js\\'" . (lambda () (js-mode) (lsp-deferred)))
	 ("\\.jsx\\'" . (lambda () (js-mode) (lsp-deferred))))
  :config
  (message "js buffer config ...")
  (setq
   indent-tabs-mode nil
   js-indent-level 2))

;; Enable the Prettier code-formatter's minor mode to format on save whenever we
;; edit JavaSciprt/JSX.  https://prettier.io/.
(use-package prettier
  :straight t
  :hook ((js-mode . prettier-mode)
	 (yaml-mode . prettier-mode)))

;; Major mode for yaml file editing.
(use-package yaml-mode
  :straight t
  :defer t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :config
  (message "yaml buffer config ...")
  (setq indent-tabs-mode nil) ; no tabs for indentation
  (add-hook 'yaml-mode-hook #'my-highlight-todos)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'yaml-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'yaml-mode-hook 'my-strip-on-save-hook))

;; Major mode for markdown (.md) file editing.
(use-package markdown-mode
  :straight t
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
  (add-hook 'markdown-mode-hook 'my-untabify-on-save-hook))

(use-package markdown-preview-mode
  :straight t
  :after markdown-mode
  :config
  (global-set-key (kbd "C-c p m")  #'markdown-preview-mode)
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
  :straight t
  :defer t
  :mode (("\\.vcl\\'" . vcl-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'vcl-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'vcl-mode-hook #'my-strip-on-save-hook))

;; Dockerfile editing
(use-package dockerfile-mode
  :straight t
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'dockerfile-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'dockerfile-mode-hook #'my-strip-on-save-hook))

;; TOML editing
(use-package toml-mode
  :straight t
  :defer t
  :mode (("\\.toml\\'" . toml-mode))
  :config
  (add-hook 'toml-mode-hook #'my-enable-line-numbers-mode)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'toml-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'toml-mode-hook #'my-strip-on-save-hook))


(use-package terraform-mode
  :straight t
  :defer t
  :mode (("\\.tf\\'" . terraform-mode))
  :config
  (message "terraform-mode config ...")
  ;; terraform language server (installed separately).
  (setq lsp-terraform-server '("terraform-ls" "serve"))
  ;; enable terraform-lsp's own logging
  ;; (setq lsp-terraform-enable-logging t)
  (lsp-deferred)

  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'terraform-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'terraform-mode-hook #'my-strip-on-save-hook))


(use-package protobuf-mode
  :straight t
  :defer t
  :mode (("\\.proto\\'" . protobuf-mode))
  :config
  (message "protobuf-mode config ...")
  (add-hook 'protobuf-mode-hook #'my-highlight-todos)
  (add-hook 'protobuf-mode-hook #'my-enable-line-numbers-mode)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'protobuf-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'protobuf-mode-hook #'my-strip-on-save-hook))


;; Rust-mode
(use-package rust-mode
  :straight t
  :defer t
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (message "rust-mode config ...")
  (setq indent-tabs-mode nil)
  ;; automatic formatting
  (setq rust-format-on-save t)
  ;; start rust LSP server.
  (add-hook 'rust-mode-hook #'lsp-deferred))


(defun my-c-mode-common ()
  "Apply common settings for 'c-mode' and 'c++-mode'."
  (setq c-basic-offset   4
        tab-width        4
        indent-tabs-mode nil)
  ;; enable use of clang-format
  (use-package clang-format
    :straight t
    :config
    ;; style to use when calling `clang-format-buffer`
    (setq clang-format-style "WebKit"))

  ;; clangd is the default LSP server for C/C++
  (setq lsp-clients-clangd-executable "clangd")
  ;; extra arguments for clangd
  (setq lsp-clients-clangd-args '("-background-index" "-log=error"))
  ;; Note: clangd needs to know your build flags. Generate a
  ;; compile_commands.json for this purpose using cmake or bear.
  (lsp-deferred)

  ;; add buffer-local save hooks
  (add-hook 'before-save-hook #'clang-format-buffer nil t))

(defun my-c-mode ()
  "Apply settings for 'c-mode'."
  (message "c-mode config ...")
  (my-c-mode-common))

(defun my-c++-mode ()
  "Apply settings for 'c++-mode'."
  (message "c++-mode config ...")
  (my-c-mode-common))

;; C and C++ setup.
(use-package cc-mode
  :hook cc-mode
  :config
  (add-hook 'c-mode-hook   #'my-c-mode)
  (add-hook 'c++-mode-hook #'my-c++-mode))

;; cmake setup.
(use-package cmake-mode
  :disabled
  :straight t
  :mode (("CMakeLists.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  ;; run cmake language server (installed separately with pip)
  (lsp-deferred))


;; Java setup.
(use-package lsp-java
  :straight t
  :defer t
  :hook (java-mode . (lambda () (require 'lsp-java) (lsp-deferred)))
  :bind (:map java-mode-map
	      ;; conflicts with hydra-windows
              ("C-c C-w" . nil)
	      ;; conflicts with describe-symbol/lsp-describe-thing-at-point
	      ("C-c C-d" . nil))
  :config
  ;; disable completion cache
  (setq company-lsp-cache-candidates nil)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'java-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'java-mode-hook #'my-strip-on-save-hook))

;; remove "ElDoc" from modeline
(use-package eldoc
  :diminish eldoc-mode)

;; can be used for working with .groovy and Jenkinsfile
(use-package groovy-mode
  :straight t
  :defer t
  :commands (groovy-mode))

;; emacs mode to edit GraphQL schemas and queries (automtically enabled when
;; opening .graphql and .gql files)
(use-package graphql-mode
  :straight t
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode))
  :hook ((graphql-mode . prettier-mode)))

;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :straight t
  :defer 5 ;; load after 5s
  :diminish
  :config
  ;; show popup at the bottom of the window
  (which-key-setup-side-window-bottom)
  ;; separator between key and bound command. Defaults to ' → '.
  (setq which-key-separator " ")
  ;; delay after which which-key popup appears after starting to type a command
  (setq which-key-idle-delay 1.0)
  (which-key-mode))

;; org-mode
(use-package org
  ;; lazily load when a .org file is opened
  :mode ("\\.org$" . org-mode)
  ;; set up key-bindings and lazily load package whenever either is called
  :bind (("C-c o o" . my-org-open)
         ("C-c o l" . org-store-link)
         ("C-c o c" . org-capture)
         ("C-c o a" . org-agenda)
         ;; key-bindings for org-mode buffers
         :map org-mode-map
         ;; x as in "check as done"
         ("C-c o x" . org-archive-subtree)
         ("C-c o >" . org-clock-in)
         ("C-c o <" . org-clock-out)
         ("C-c C-s" . org-schedule)
         ("C-c C-d" . org-deadline)
	 ;; jump to heading with live preview
	 ("C-c o h" . consult-outline))
  :init
  ;; make org-mode table editor available in text-mode (or derived modes)
  (add-hook 'text-mode-hook #'my-enable-orgtbl-mode)
  ;; extend the org-mode markup by having text surrounded by backticks "`"
  ;; display with verbatim face
  (font-lock-add-keywords 'org-mode
    '(("`[^`\n\r\t]+`" 0 'org-verbatim prepend)) 'append)
  :config
  ;; always run in org-indent-mode (level by indent rather than asterisks)
  (setq org-startup-indented t)

  ;; agenda should start with Monday
  (setq org-agenda-start-on-weekday 1)
  ;; calendar should start with Monday
  (setq calendar-week-start-day 1)
  ;; character(s) to indicate a folded section
  (setq org-ellipsis "...")
  ;; when entering org files: start in folded `OVERVIEW` state?
  ;; can also be configured per file with `#+STARTUP`.
  (setq org-startup-folded nil)
  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
                             ("=" org-verbatim verbatim)
                             ("~" org-code verbatim)
                             ("+" (:strike-through t))))

  ;; default location to look for org files.
  (setq org-directory "~/org")
  ;; files to include when compiling the agenda.
  (setq org-agenda-files '("~/org/work.org" "~/org/archive.org"))
  ;; location to store archived entries/subtrees. Items are placed under
  ;; H1-headlines "From FILE" where `FILE` is the file from where entry was
  ;; archived.
  (setq org-archive-location "~/org/archive.org::* %s archive")
  ;; add archived items first under heading
  (setq org-archive-reversed-order t)

  ;; possible workflow states (use S-{left,right} to move throught states)
  (setq org-todo-keywords '((sequence "TODO" "STARTED" "BLOCKED"
                                      "|" "DONE")))
  ;; capture timestamp when a TODO changes state to DONE.
  (setq org-log-done 'time)

  ;; where to place captured notes.
  (setq org-default-notes-file (concat org-directory "/captured.org"))
  ;; templates to select from on org-capture
  ;; - "%i": initial content (marked region  when capture was called).
  ;; - "%a": annotation, normally the link created with org-store-link.
  ;; - "%l": like %a, but only insert literal link.
  ;; - "%f": file visited by buffer from where org-capture was called.
  ;; - "%F": like %f but full path.
  ;; - "%?": after completing template, position point here.
  (setq org-capture-templates
        '(("w" "work" entry (file+headline "~/org/work.org" "Inbox")
           "* TODO %?")))

  (defun my-org-open ()
    "Interactively open a file in `org-directory`."
    (interactive)
    ;; using temp buffer avoids changing `default-directory` in current buffer
    (with-temp-buffer
      (let ((default-directory (format "%s/" org-directory)))
	(call-interactively #'find-file)))))


(use-package highlight-indent-guides
  :straight t
  :hook ((emacs-lisp-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))


(use-package ruby-mode
  :mode (("\\.rb\\'"  . ruby-mode))
  :config
  ;; use ruby language server (installed as a separate gem)
  (lsp-deferred))

(use-package vterm
  :straight t
  ;; set up key-bindings and lazily load package whenever either is called
  :bind (("C-c v t" . vterm-other-window)
         ;; key-bindings for vterm-mode buffers
         :map vterm-mode-map
         ;; toggle from term input to emacs buffer (search/copy) mode
         ("C-c C-t" . vterm-copy-mode)
         ("C-c C-l" . vterm-clear-scrollback))
  :config
  (setq vterm-kill-buffer-on-exit t))

;; a package for making screencasts within Emacs.
(use-package gif-screencast
  :straight t
  :bind (("C-c C-s s" . gif-screencast-start-or-stop)
	 ("C-c C-s p" . gif-screencast-toggle-pause))
  :config
  (setq gif-screencast-program "scrot")
  (setq gif-screencast-output-directory (expand-file-name "~/.emacs.d/screencasts")))

;; enable through `keycast-mode` or `keycast-log-mode`
(use-package keycast
  :straight t
  :defer t)


;;; Finalization


;; Output the time at which the loading of the init-file itself completed.
(message "Loaded %s after %.3fs." load-file-name (my-elapsed-time))

;; Output the time at which the loading of all init-hooks completed.
(add-hook 'after-init-hook
          (lambda () (message "init-hooks done after %.3fs." (my-elapsed-time))) t)

(provide 'init)

;;; init.el ends here
