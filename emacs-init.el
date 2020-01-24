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

(defvar my-treemacs-min-width 120
  "Minimum frame width when treemacs is enabled (in characters).")
(defvar my-font "DejaVu Sans Mono"
  "Text font to use (for example, `Ubuntu Mono`).")
(defvar my-font-size 10.5 "Font size to use in points (for example, 10.5).")

(defvar my-packages
  '(use-package)
  "A list of packages that will be installed at launch (unless present).
This list is intentionally kept to a bare minimum.  Most packages
are installed via `use-package` and loaded on-demand.")

;; TODO: should skip this and use use-package without :ensure but with
;; :load-path
(defvar my-modules (file-expand-wildcards "~/dotfiles/emacs.modules/*.el")
  "The location of any version-controlled packages to load on init.")

(defvar my-desktops-dir (expand-file-name (concat user-emacs-directory "desktops"))
  "A directory where desktops are to be stored.
A separate directory will be created under this directory for
each saved desktop.  For example,
`<my-desktops-dir>/home/peterg/some/project/dir/.emacs.desktop`.")

(defvar my-desktop-save-file ".emacs.desktop"
  "File name to use for storing desktop state.")

(defvar my-emacs-start-dir default-directory
  "Directory from which emacs was launched.")

;;
;; Tricks to reduce startup time. These need to be set at an early stage.
;;

;; avoid GC performance-penalty on startup by temporarily bumping the memory
;; threshold for GC. This effectively defers garbage collection.
(defvar gc-cons-threshold-custom (* (expt 1 1) gc-cons-threshold)) ;; X^Y * default
(setq gc-cons-threshold 268435456) ;; set high (256 mb) temporarily during init
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

;;
;; Utility functions
;;

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
  (add-hook 'before-save-hook 'my-untabify-buffer nil t))

(defun my-strip-on-save-hook ()
  "Register a buffer-local 'before-save-hook' to run 'delete-trailing-whitespace'."
  ;; note: last argument makes this save-hook local to the buffer
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

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

(defun my-vcs-dir-p (path)
  "Determines if the PATH directory is under version control (e.g. 'Git').
Returns nil for paths not under version control."
  (vc-backend path))

(defun my-project-root-or-cwd ()
  "Return the project root of the directory tree where Emacs was opened.
If Emacs wasn't opened in a version-controlled directory, the
result will be the current working directory."
  (if (my-vcs-dir-p my-emacs-start-dir)
      ;; determine project root
      (let ((vc-backend (vc-responsible-backend my-emacs-start-dir)))
	(vc-call-backend vc-backend 'root my-emacs-start-dir))
    my-emacs-start-dir))

(defun my-desktop-save-dir ()
  "Return the save directory to use for `desktop-save-mode`.
The location is determined from where Emacs was opened."
  (concat my-desktops-dir (expand-file-name (my-project-root-or-cwd))))

(defun my-desktop-save-path ()
  "Return the path where `desktop-save-mode` will store its session.
The location is determined from where Emacs was opened."
  (concat (my-desktop-save-dir) my-desktop-save-file))

(defun my-desktop-delete ()
  "Deletes the desktop save path (if it exists)."
  (interactive)
  (desktop-save-mode 0)
  (delete-file (my-desktop-save-path)))

(defun my-enable-desktop-save-mode ()
  "Enable `desktop-save-mode`.
This will load the saved desktop if one exsists, or create a new
desktop state file if one does not exist.  If the desktop is
already loaded by another Emacs process, a warning is printed."
  (interactive)
  (message "using desktop save directory: %s" (my-desktop-save-dir))
  ;;
  ;; Save settings
  ;;
  (setq desktop-dirname (my-desktop-save-dir)) ;; state directory
  (setq desktop-base-file-name my-desktop-save-file)  ;; state file
  (setq desktop-save t) ;; always save on exit (without prompting)
  (setq desktop-auto-save-timeout 30) ;; in seconds
  ;;
  ;; Desktop load settings.
  ;;
  ;; directory where desktop state is to be loaded from.
  (setq desktop-path (list desktop-dirname))
  ;; max buffers to restore immediately (the rest are lazily loaded)
  (setq desktop-restore-eager 10)
  ;; never load the desktop if locked
  (setq desktop-load-locked-desktop nil)

  ;; create the desktop file if it doesn't already exist
  (when (not (file-exists-p (my-desktop-save-path)))
    (make-directory desktop-dirname t)
    (desktop-save desktop-dirname t)
    ;; appears necessary to create desktop lock after save
    (desktop-read))

  ;; enable desktop-save-mode
  (desktop-save-mode 1)
  (desktop-auto-save-set-timer)

  ;; disable desktop-save-mode if desktop cannot be loaded (e.g. when locked by
  ;; another process)
  (add-hook 'desktop-not-loaded-hook
	    (lambda ()
	      (display-warning :warning "couldn't load desktop (is it locked by a different process?). disabling desktop-save-mode ...")
	      (desktop-save-mode 0))))

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

;; Install any uninstalled "base" packages.
(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
	   when (not (package-installed-p p)) do (cl-return nil)
	   finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new package versions
  (message "%s" "refreshing package database ...")
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Load any local modules from module directory in lexicographical order.
(setq sortedmodules (sort (copy-sequence my-modules) #'string-lessp))
;; Note: messages are logged in *Messages* buffer
(message "Loading local modules: %s" sortedmodules)
(dolist (module sortedmodules)
  (load-file module))

;;
;; General settings
;;
(defun my-general-settings ()
  "Apply appearance and general editor settings."
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8)
  (setq inhibit-startup-screen t)
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
  (global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)
  ;; Hide vertical scrollbar on right
  (scroll-bar-mode -1)
  ;; Hide tool-bar (icons, such as open file, cut, paste, etc)
  (tool-bar-mode -1)
  ;; Display line numbers (toggle with M-x display-line-numbers-mode)
  (global-display-line-numbers-mode -1)

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
      (setq initial-frame-alist '((width . normal-width) )))
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

  ;;
  ;; generic key bindings
  ;;

  ;; unbind unneeded/disturbing keybindings
  (global-set-key (kbd "C-t") nil) ;; transpose characters
  (global-set-key (kbd "M-t") nil) ;; transpose words

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
  (windmove-default-keybindings)
  ;; enlarge/shrink current window vertically (on vertical split)
  (global-set-key (kbd "C-S-<up>")    'enlarge-window)
  (global-set-key (kbd "C-S-<down>")  'shrink-window)
  ;; enlarge/shrink current window horizontally (on horizontal split)
  (global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-S-<left>")  'shrink-window-horizontally))

(my-general-settings)

;;
;; Start of custom package installation/configuration.
;;

(require 'use-package)
(setq use-package-verbose nil) ;; set to t to see when packages are loaded


;; F6 enables desktop-save-mode (the desktop state directory becomes the VCS
;; root, or the current working directory if not in a version-controlled file
;; tree. In this mode the desktop is saved periodically and on exit.
;;
;; If, on load of this init file, an existing desktop state directory is found
;; it gets loaded and desktop-save-mode is enabled. If the desktop is already in
;; use by another emacs process (locked), a warning is printed and
;; desktop-save-mode stays disabled.
(use-package desktop
  ;; defers loading of package until command is invoked
  :commands my-enable-desktop-save-mode
  :defer t
  :init
  (when (file-exists-p (my-desktop-save-path))
    (message "discovered saved desktop, enabling desktop-save-mode ...")
    (my-enable-desktop-save-mode))
  (global-set-key (kbd "<f6>") 'my-enable-desktop-save-mode))

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
  ;; increment delta (in tenths of points), so needs to be divisible by 10.
  (setq default-text-scale-amount 10)
  (global-set-key (kbd "C-x C-+") 'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") 'default-text-scale-decrease)
  (global-set-key (kbd "C-x C-0") 'my-set-default-font-height))

(use-package immaterial-theme
  :ensure t
  :config
  ;; default: https://material.io/resources/color/#!/?view.left=0&view.right=0&primary.color=4DB6AC&secondary.color=AED581
  (setq immaterial-color-override-alist
	'(;; solarized dark background colors
	  ;; ("background-primary"   . "#002b36")
	  ;; ("background-off"       . "#00212b")
	  ;; ("background-on"        . "#003644")

	  ;; blue-grey
	  ;; ("background-primary" . "#1b292d")
	  ;; ("background-off"     . "#121212")
	  ;; ("background-on"      . "#23363d")

	  ;; emacs purple dark
	  ;; ("background-primary" . "#252342")
	  ;; ("background-off"     . "#151426")
	  ;; ("background-on"      . "#2d2b51")

	  ;; petrol
	  ;; ("background-primary" . "#002525")
	  ;; ("background-off"     . "#002426")
	  ;; ("background-on"      . "#003435")
	))
  (load-theme 'immaterial t))

;; A theme that runs on top of the existing theme to extend/highlight the
;; modeline buffer id with the name of the host. Can be customized via
;; `tramp-theme-face-remapping-alist`.
;; (use-package tramp-theme
;;   :ensure t
;;   :config
;;   (load-theme 'tramp t))

;; beacon shows a brief light flash at the cursor to help keep track
(use-package beacon
  :disabled
  :ensure t
  :config
  (setq beacon-blink-when-buffer-changes t)   ;; on buffer switch
  (setq beacon-blink-when-window-scrolls nil) ;; on scrolling
  (setq beacon-blink-when-window-changes t)   ;; change window (on splits)
  (setq beacon-blink-when-focused nil)        ;; when emacs gains focus
  (setq beacon-blink-duration 0.3)
  (setq beacon-blink-delay 0.3)
  (beacon-mode t))

(use-package powerline
  :ensure t
  :if window-system
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'contour))

;; ido is a completion engine that is activated on calls to `switch-to-buffer`
;; (C-x b) and `find-file` (C-x C-f). It can be used as an alternative to ivy
;; (and counsel), which offers a superset of ido's functionality.
(use-package ido
  :disabled
  :config
  ;; any item that contains all entered characters will match
  (setq ido-enable-flex-matching t)
  (ido-mode t))

;; ivy ensures that any Emacs command using `completing-read-function` uses ivy
;; for completion. Can be complemented with (1) swiper for ivy-enhanced isearch
;; and (2) counsel for versions of common Emacs commands customised to make use
;; of ivy.
(use-package ivy
  :ensure t
  :pin melpa-stable
  :diminish ivy-mode
  :config
  ;; add recent files and bookmarks to ivy-switch-buffer?
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy)
  (setq enable-recursive-minibuffers t)
  ;; find-file-in-project will use ivy by default
  (setq projectile-completion-system 'ivy)
  (ivy-mode 1)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; during search, make ivy complete to the greatest common denominator on TAB.
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial))

;; ivy-enhanced version of isearch
(use-package swiper
  :ensure t
  :pin melpa-stable
  :bind (("C-s" . swiper)))

;; a collection of ivy-enhanced versions of common Emacs commands.
(use-package counsel
  :ensure t
  :pin melpa-stable
  :bind (("M-x"     . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ;; list faces
	 ("C-c l f" . counsel-faces)
	 ("C-h v"   . counsel-describe-variable)
	 ("C-h f"   . counsel-describe-function)))

;; uses ivy to improve projectile. For example, freetext search in project via
;; `counsel-projectile-ag`.
(use-package counsel-projectile
  :ensure t
  :pin melpa-stable
  :after projectile
  :config
  ;; free-text "search-in-project"
  (global-set-key (kbd "C-c s p") 'counsel-projectile-ag))

;; annotates ivy completion candidates with additional descriptions. supports
;; ivy-switch-buffer, counsel-M-x, counsel-describe-function and
;; counsel-describe-variable
(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  ;; change appearance of `ivy-switch-buffer` colums
  (setq ivy-rich--display-transformers-list
	(plist-put ivy-rich--display-transformers-list 'ivy-switch-buffer
		   '(:columns
		     ;; candidate itself
		     ((ivy-rich-candidate (:width 30))
		      ;; path relative to project root (or default-directory)
		      (ivy-rich-switch-buffer-path (:width 50 :face font-lock-doc-face)))
		     :predicate
		     (lambda (cand) (get-buffer cand)))))

  (ivy-rich-mode 1))

;; display ivy searches elsewhere than in the minibuffer
(use-package ivy-posframe
  :ensure t
  :diminish
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-window-center)))
  (ivy-posframe-mode 1))

;; highlights occurences of colors (in text) with a background of that
;; color. For example, "#aaaaaa" will be displayed with a gray background.
;; Activate via M-x rainbow-mode
(use-package rainbow-mode
  :ensure t
  :defer 5
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

;; Transparent Remote Access, Multiple Protocols -- edit remote files
(use-package tramp
  :defer 5 ;; wait 5 seconds before loading
  :config
  ;; default method for transferring files (scp, ssh)
  (customize-set-variable 'tramp-default-method "ssh"))

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

;; built-in on-the-fly spell checking for text or code comments.
(use-package flyspell
  :disabled
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode)))

;; A language template system for emacs. lsp-mode auto-configures yasnippet for
;; use with a given language server.  Write a snippet key and press the key
;; associated with yas-expand (TAB by default) to have the snippet expanded. To
;; see available snippets: M-x yas-describe-tables
(use-package yasnippet
  :ensure t
  :defer 2
  :diminish yas-minor-mode ; don't display on modeline
  :config
  ;; use yasnippet as a global minor mode
  ;; note: it can also be activated per language/major-mode
  ;;    see https://github.com/joaotavora/yasnippet
  (yas-global-mode 1))

;; A collection of snippets for many languages.
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


;; A Git porcelain inside Emacs.
(use-package magit
  :ensure t
  ;; defer loading of module until any of these functions are called *and* set
  ;; up key bindings to invoke them.
  :bind (("C-x g" . magit-status)))

;; Highlight diffs (in the fringe) for version-controlled buffers.
(use-package diff-hl
  :ensure t
  :defer 5
  :diminish
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :config
  ;; refresh if magit does an update
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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
	("<f8>" . my-toggle-treemacs)))

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

;; transparently open compressed files
(use-package auto-compression-mode
  :defer 5
  :config
  (auto-compression-mode t))

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
  :disabled
  :ensure t
  :defer 2
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
  :defer 2
  :config
  ;; Replacement for ggtags-find-definition that always prompts (default
  ;; behavior is to just search for symbol at point if there is one)
  (defun ggtags-find-definition-interactive  ()
    (interactive)
    ;; can read 'definition, 'symbol, 'reference, 'id, 'path
    (let ((tag (ggtags-read-tag 'definition t "Find definition")))
      (ggtags-find-definition tag)))
  ;; "find-tag", "find-type"
  (global-set-key (kbd "C-c f t") 'ggtags-find-definition-interactive))

;; Enable display-line-numbers-mode whenever we are in prog-mode
(use-package linum
  :ensure t
  :defer t
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

;; when saving a buffer in sh-mode: untabify and delete trailing whitespace
(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
	 ("\\.env\\'" . sh-mode))
  :config
  (add-hook 'sh-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'sh-mode-hook 'my-strip-on-save-hook))

(use-package lsp-mode
  :ensure t
  ;;:pin melpa-stable
  :defer t
  :commands lsp
  :config
  (message "lsp-mode config ...")
  ;; If non-nil, print all messages to/from lang server in *lsp-log*.
  (setq lsp-log-io nil)
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
  (setq lsp-ui-doc-position 'top)
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
  :mode "\\.py$"
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
  (add-hook 'json-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'json-mode-hook 'my-strip-on-save-hook))

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
  (add-hook 'yaml-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'yaml-mode-hook 'my-strip-on-save-hook))

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
  (add-hook 'markdown-mode-hook 'my-untabify-on-save-hook))

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
  (add-hook 'vcl-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'vcl-mode-hook 'my-strip-on-save-hook))

;; Dockerfile editing
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-mode))
  :config
  (add-hook 'dockerfile-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'dockerfile-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'dockerfile-mode-hook 'my-strip-on-save-hook))

;; TOML editing
(use-package toml-mode
  :ensure t
  :defer t
  :mode (("\\.toml\\'" . toml-mode))
  :config
  (add-hook 'toml-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'toml-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'toml-mode-hook 'my-strip-on-save-hook))

(use-package terraform-mode
  :ensure t
  :defer t
  :mode (("\\.tf\\'" . terraform-mode))
  :config
  (message "terraform-mode config ...")
  (add-hook 'terraform-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'terraform-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'terraform-mode-hook 'my-strip-on-save-hook))

(use-package protobuf-mode
  :ensure t
  :defer t
  :mode (("\\.proto\\'" . protobuf-mode))
  :config
  (message "protobuf-mode config ...")
  (add-hook 'protobuf-mode-hook 'display-line-numbers-mode) ; show line numbers
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'protobuf-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'protobuf-mode-hook 'my-strip-on-save-hook))

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
  :disabled
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook (lambda () (require 'lsp-java) (lsp)))
  :config
  (message "lsp-java config ...")
  ;; disable completion cache
  (setq company-lsp-cache-candidates nil)
  ;; disable keymap bindings that would override lsp ones.
  (define-key java-mode-map (kbd "C-c C-d") nil)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'java-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'java-mode-hook 'my-strip-on-save-hook))

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

;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :ensure t
  :pin melpa-stable
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


;;; Finalization


;; Output the time at which the loading of the init-file itself completed.
(message "Loaded %s after %.3fs." load-file-name (my-elapsed-time))

;; Output the time at which the loading of all init-hooks completed.
(add-hook 'after-init-hook
	  (lambda () (message "init-hooks done after %.3fs." (my-elapsed-time))) t)
