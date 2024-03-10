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
(defvar my-font "Roboto Mono"
  "Text font to use.
For example, `Source Code Pro`, `Ubuntu Mono`,`Cousine`, `JetBrains Mono`).")
(defvar my-font-size 10.5 "Font size to use in points (for example, 10.5).")

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
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (message "loading straight.el bootstrapper ...")
    (load bootstrap-file nil 'nomessage)))

(defun my-normalized-path (path)
  "Return a normalized PATH that is expanded and trimmed of trailing slash."
  (string-trim-right (expand-file-name path) "/"))

(defun my-first-existing-ancestor-dir (path)
  "Return the first ancestor directory of PATH that exists on the file system."
  (if (and (file-directory-p path) (file-exists-p path))
      (abbreviate-file-name path)
    (my-first-existing-ancestor-dir (file-name-directory (my-normalized-path path)))))

(defun my-find-project-root (path)
  "Return the project root directory for PATH.
If PATH is not in a version-controlled directory, nil is returned."
  (when-let* ((closest-dir (my-first-existing-ancestor-dir path))
              (proj (project-current nil closest-dir)))
    (project-root proj)))

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


(defun my-add-eglot-format-on-save-hook ()
  "Register a buffer-local `before-save-hook' to run `eglot-format-buffer' and `eglot-code-action-organize-imports'."
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  ;; See https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook (lambda () (call-interactively #'eglot-code-action-organize-imports)) -9 t))


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


(defun my-color-lighten (hex-color percent)
  "Determine a brighter/darker shade of a hex color.
For a HEX-COLOR (such as `#3cb878`) return the hex color that is
PERCENT percent brighter (or darker if percent value is
negative)."
  (interactive "sHex color: \nnPercent brighter/darker (-): ")
  (if (fboundp 'immaterial-color-lighten)
      (message "%s" (immaterial-color-lighten hex-color percent))))


(defun my-enable-line-numbers-mode ()
  "Enable `display-line-numbers-mode' in buffer."
  (display-line-numbers-mode 1))

(defun my-disable-line-numbers-mode ()
  "Disable `display-line-numbers-mode' in buffer."
  (display-line-numbers-mode -1))


(defun my-enable-orgtbl-mode ()
  "Enable `orgtbl-mode'.
`orgtbl-mode' is a minor mode that makes Org-modes table editor
commands available."
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
;; For a more stable alternative go with branch "master".
(setq straight-repository-branch "develop")
(my-bootstrap-straight-el)

;; The use-package macro is included in Emacs as of version 29. No need to
;; install.
(setq use-package-verbose nil) ;; set to t to see when packages are loaded

;;
;; General settings
;;
(defun my-general-settings ()
  "Apply appearance and general editor settings."
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8)
  ;; System locale for formatting time values -- ensures that weekdays in the
  ;; org-mode timestamps appear in English.
  (setq system-time-locale "C")
  ;; Show column in mode-line.
  (setq column-number-mode t)
  ;; Wrap long lines.
  (set-default 'truncate-lines nil)
  ;; Sets the fill column (where to break paragraphs on M-q)
  (setq-default fill-column 80)
  ;; Entering a character replaces the selected (active) region.
  (delete-selection-mode 1)
  ;; Set the default font to use on all frames (see
  ;; https://www.freedesktop.org/software/fontconfig/fontconfig-user.html)
  (add-to-list 'default-frame-alist `(font . ,(format "%s-%f" my-font my-font-size)))
  ;; Allow copy/paste to/from system clipboard.
  (setq select-enable-clipboard t)
  ;; Middle mouse button inserts the clipboard (rather than emacs primary).
  (global-set-key (kbd "<mouse-2>") #'x-clipboard-yank)
 ;; Use spaces for indentation.
  (setq-default indent-tabs-mode nil)
  ;; No sudden jumps when cursor moves off top/bottom of screen. If the value is
  ;; greater than 100, redisplay will never recenter point, but will always
  ;; scroll just enough text to bring point into view
  ;; (setq scroll-conservatively 101)
  ;; Enable line numbers in all text-mode/prog-mode buffers.
  (add-hook 'text-mode-hook    #'my-enable-line-numbers-mode)
  (add-hook 'prog-mode-hook    #'my-enable-line-numbers-mode)
  ;; disable line numbers in special mode buffers (magit)
  (add-hook 'special-mode-hook #'my-disable-line-numbers-mode)
  ;; Highlight todo markers in code.
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
  ;; Allow minibuffer commands while in the minibuffer.
  (setq enable-recursive-minibuffers t)
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
  (global-set-key (kbd "C-c C-d")  #'eldoc)
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
  :bind (("C-x C-+" . default-text-scale-increase)
	 ("C-x C--" . default-text-scale-decrease)
	 ("C-x C-0" . my-set-default-font-height))
  :config
  ;; increment delta (in tenths of points), so needs to be divisible by 10.
  (setq default-text-scale-amount 10))


(use-package immaterial-theme
  ;; :load-path "~/dev/git/emacs-immaterial-theme"
  :straight (immaterial-theme
             :type git :host github
             :repo "petergardfjall/emacs-immaterial-theme"
             :branch "master")
  :config
  (load-theme 'immaterial-dark t))


;;
;; Configure some basic settings for `completing-read' (minibuffer completion=
;; and, to some extent also `complete-at-point' (buffer completion).
;;
;; Also see:
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
;;
(use-package minibuffer
  :straight (:type built-in)
  :config
  ;; Determine how to match minibuffer input text against completion candidates.
  (setq completion-styles '(substring basic))

  ;; Ignore case on various forms of `completing-read' (minibuffer completion).
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t))


;;
;; Icomplete is a built-in ui for `completing-read' (a.k.a. "minibuffer
;; completion"). We configure it to use vertical candidate display in the
;; minibuffer.
;;
;; Also see:
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
;;
(use-package icomplete
  :straight (:type built-in)
  :config
  ;; Display candidates in a vertical list.
  (icomplete-vertical-mode 1)
  ;; Move point through list rather than rotate first entry.
  (setq icomplete-scroll t)
  (setq icomplete-show-matches-on-no-input t)
  ;; Make more responsive.
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-compute-delay 0.0)
  ;; Don't bring up help dialog on failure to complete.
  (setq completion-auto-help nil)
  ;; Control how matches are ordered in *Completions* buffer on calls to
  ;; `minibuffer-completion-help'.
  (setq completions-format 'one-column)
  ;; Truncate long completion candidate lines in the minibuffer.
  (add-hook 'icomplete-minibuffer-setup-hook
            (lambda () (setq-local truncate-lines t)))

  ;; Implement page-wise scrolling since that's not yet provided.
  (defun my-icomplete-page-up ()
    (interactive)
    (let* ((shown-candidates (- (window-total-height) 1))
           (scroll (/ shown-candidates 2)))
      (dotimes (_ scroll) (icomplete-backward-completions))))
  (defun my-icomplete-page-down ()
    (interactive)
    (let* ((shown-candidates (- (window-total-height) 1))
           (scroll (/ shown-candidates 2)))
      (dotimes (_ scroll) (icomplete-forward-completions))))

  ;;
  ;; Key bindings.
  ;;
  (let ((m icomplete-minibuffer-map))
    (define-key m (kbd "<return>") #'icomplete-force-complete-and-exit)
    ;; For `find-file' this allows forcing creation of a file whose name matches
    ;; one of the completion candidates.
    (define-key m (kbd "C-<return>") #'icomplete-ret)
    (define-key m (kbd "<SPC>") nil))
  (let ((m minibuffer-local-completion-map))
    ;; Open *Completions* buffer. Can be useful when there are a lot of
    ;; candidates and `icomplete' won't allow paging through results.
    (define-key m (kbd "?")       #'minibuffer-completion-help)
    (define-key m (kbd "<prior>") #'my-icomplete-page-up)
    (define-key m (kbd "<next>")  #'my-icomplete-page-down)))


;;
;; Company is a UI for `completion-at-point' (a.k.a. "in-buffer completion"). It
;; shows completion candidates in a popup overlay. Completions candidates are
;; provided by `completion-at-point-functions' such as
;; `eglot-completion-at-point'.
;;
(use-package company
  :straight t
  :diminish
  :init
  (global-company-mode)
  :config
  ;; Minimum prefix length before triggering auto-completion. Note that
  ;; completion can be summoned at any time with 'C-<tab>'
  ;; (`completion-at-point').
  (setq company-minimum-prefix-length 3)
  ;; Decrease delay until completion popup shows.
  (setq company-idle-delay 0.1)
  ;; Only use company with `complation-at-point-functions'.
  (setq company-backends '(company-capf))
  ;; Maximum number of candidates to display.
  (setq company-tooltip-limit 15)
  ;; miniumum popup width (in characters)
  (setq company-tooltip-minimum-width 20)
  ; Start autocompletion only after typing.
  (setq company-begin-commands '(self-insert-command))
  ;; Align annotations (e.g. function signatures) to the right tooltip border.
  (setq company-tooltip-align-annotations t)
  ;; Summon the completion popup.
  (define-key company-mode-map (kbd "C-<tab>") #'company-complete))


;;
;; Orderless provides a "completion style" for `completing-read' where
;; space-separated words can be input as search terms.
;;
(use-package orderless
  :straight t
  :init
  ;; Note: we do not just add `orderless' to the default
  ;; `completion-styles'. Instead we apply it very selectively to certain
  ;; completion categories. In particular, we avoid `orderless' with eglot
  ;; completions, where it appears to not always play well.
  (setq completion-category-overrides
        '(;; Use orderless for `switch-to-buffer'.
          (buffer (styles orderless))
          ;; Use orderless for `project-find-file'.
          (project-file (styles orderless))
          ;; Use orderless for `describe-variable' (C-h v).
          (variable (styles orderless))
          ;; Use orderless for `describe-function' (C-h f).
          (function (styles orderless))
          ;; Use orderless for `execute-extended-command' (M-x).
          (command (styles orderless))
          ;; Use orderless for different variants of `consult-grep'.
          (consult-grep (styles orderless))
          ;; Use orderless for different variants of `consult-line'.
          (consult-location (styles orderless)))))


;; Incremental buffer search configured to support navigation with up/down key.
(use-package isearch
  :straight (:type built-in)
  ;; Lazily load when called for.
  :bind (("C-S-s" . isearch-forward)
	 ("C-r"   . isearch-backward))
  :config
  (let ((m isearch-mode-map))
    (define-key m (kbd "<up>") #'isearch-repeat-backward)
    (define-key m (kbd "<up>") #'isearch-repeat-forward)))


(defun my-buffer-search ()
  "Search the current buffer for occurences of a string.
It works by executing `consult-line-multi' only on the current
buffer (controlled through a buffer predicate).  `consult-line'
offers similar functionality (`completing-read' search with live
preview) but only highlights a single occurence of the search
term in the buffer preview.  `consult-line-multi' shows all
occurences (like for example the regular `isearch-forward') but
executes `grep' behind the scenes.  For a single buffer the
performance impact should be unnoticable though."
  (interactive)
  ;; For this call only, make the `grep' search more responsive.
  (let ((consult-async-input-debounce 0.0)
	(consult-async-min-input 1)
	(consult-async-refresh-delay 0.0)
	(consult-async-input-throttle 0.0))
    (consult-line-multi '(:predicate (lambda (buf) (eq buf (current-buffer)))))))


;; Consult provides practical commands based on completing-read.
(use-package consult
  :straight t
  :bind (("M-g g"   . consult-goto-line) ;; goto-line
	 ;; search buffer with live preview.
	 ("C-s"     . consult-line)
	 ;; "search buffer". runs grep in current buffer. Shows all findings in
	 ;; live preview, will start at first occurence.
	 ("C-c s b" . my-buffer-search)
	 ;; "search git": free-text search in version-controlled files
	 ("C-c s g" . consult-git-grep)
	 ;; "search project": free-text search in all project files
	 ("C-c s p" . consult-ripgrep))
  :init
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Needed to make consult project-aware (for example for `consult-grep').
  (setq consult-project-root-function #'my-project-root)
  ;; Delay before starting a new async search (for example for `consult-grep').
  (setq consult-async-input-debounce 0.2))


;;
;; Adds richer annotations for minibuffer completions for any completing-read
;; compatible framework (such as selectrum or vertico). Similar to ivy-rich.
;;
(use-package marginalia
  :straight t
  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode)
  (defun my-project-buffer-annotator (cand)
    (let* ((buffer (get-buffer cand)))
      (when-let* ((buffer-file (buffer-file-name buffer))
	          (project-dir (my-find-project-root buffer-file))
	          (project-short (file-name-base (directory-file-name project-dir))))
        (let ((project-rel-dir (file-name-directory (file-relative-name buffer-file project-dir))))
	  (marginalia--fields
	   (project-short :truncate 0.4 :face 'marginalia-value)
	   (project-rel-dir :truncate 0.4 :face 'marginalia-documentation))))))
  ;; update annotator-registry to use my custom annotator for buffers
  (add-to-list 'marginalia-annotator-registry
               '(buffer my-project-buffer-annotator none))
  :config
  (let ((m minibuffer-local-map))
    (define-key m (kbd "M-A") #'marginalia-cycle)))


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


(use-package vundo
  :straight t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  ;; Display visual undo tree.
  (global-set-key (kbd "C-c u t") #'vundo)
  (let ((m vundo-mode-map))
    (define-key m (kbd "d") #'vundo-diff)
    (define-key m (kbd "C-x u") #'undo)))


;; built-in project.el
(use-package project
  :straight (:type built-in)
  :config
  (global-set-key (kbd "C-c f f") #'project-find-file))


(use-package wsp
  :straight (emacs-wsp
             :type git :host github :repo "petergardfjall/emacs-wsp")
  ;; Lazily load when called for.
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


(use-package projtree
  ;; :load-path "~/dev/git/emacs-projtree"
  :straight (emacs-projtree
             :type git :host github :repo "petergardfjall/emacs-projtree" :branch "main")
  :commands (projtree-mode)
  :bind (("<f8>" . projtree-mode))
  :config
  (setq projtree-profiling-enabled nil))


(use-package postrace
  ;; :load-path "~/dev/git/emacs-postrace"
  :straight (postrace :type git :host github
                      :repo "petergardfjall/emacs-postrace"
                      :branch "main")
  ;; Lazily load when called for.
  :bind (("C-c p p" . postrace-push)
	 ("C-c p b" . postrace-browse)))


;; allows definition of hydras - families of commands with a common prefix
(use-package hydra
  :straight t
  :config
  ;; Window navigation/resizing hydra.
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




;; built-in on-the-fly syntax checking, which highlights erroneous lines.
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


;; built-in on-the-fly spell checking for text buffers.
(use-package flyspell
  :straight (:type built-in)
  :diminish
  :hook ((text-mode . flyspell-mode)))

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
  ;; Lazily load when called for.
  :bind (("C-x g" . magit-status)))


;; Highlight diffs (in the fringe) for version-controlled buffers.
(use-package diff-hl
  :straight t
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :diminish
  :config
  ;; refresh if magit does an update
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


;; transparently open compressed files
(use-package auto-compression-mode
  :straight (:type built-in)
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
  ;; Lazily load when called for.
  :bind (("C-c t c"   . my-ggtags-create)
	 ("C-c t f d" . my-ggtags-find-definition)
	 ("C-c t f r" . my-ggtags-find-reference))
  :config
  ;; interferes with beginning/end of buffer key bindings
  (define-key ggtags-navigation-map (kbd "M->") nil)
  (define-key ggtags-navigation-map (kbd "M-<") nil))


;; when saving a buffer in sh-mode: untabify and delete trailing whitespace
(use-package sh-script
  :mode (("\\.sh$" . sh-mode)
         ("\\.env$" . sh-mode))
  :config
  ;; use bash-language-server (installed separately via npm)
  (add-hook 'sh-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'sh-mode-hook 'my-strip-on-save-hook))


;; Integrates the tree-sitter incremental language parsing library. It supports
;; syntax highlighting and comes with replacement major-modes for many languages
;; `<language>-ts-mode'. In the future a lot of interesting functionality might
;; come from it.
(use-package treesit
  :straight (:type built-in)
  :defer t
  :init
  ;; Use treesit-based major-modes where grammars are available.
  (add-to-list 'major-mode-remap-alist '(bash-mode   . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode      . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode    . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode    . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mode     . go-ts-mode))
  (add-to-list 'major-mode-remap-alist '(html-mode   . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(json-mode   . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode     . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode   . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(rust-mode   . rust-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sql-mode    . sql-ts-mode))
  (add-to-list 'major-mode-remap-alist '(toml-mode   . toml-ts-mode))
  ;; Specify which tree-sitter language grammar defintions to use.
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
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

(use-package eglot
  :straight (:type built-in)
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
	 (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure))
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
  ;; See https://rust-analyzer.github.io/manual.html#rustup.
  (add-to-list 'eglot-server-programs '((rust-mode) . ("rustup" "run" "stable" "rust-analyzer")))

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

  (defun my-find-workspace-symbol ()
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
    (define-key m (kbd "C-c f s")  #'my-find-workspace-symbol)
    (define-key m (kbd "C-c f i")  #'eglot-find-implementation)
    (define-key m (kbd "C-c f r")  #'xref-find-references)
    (define-key m (kbd "C-c C-r")  #'eglot-rename)
    (define-key m (kbd "C-c d")    #'eldoc)))


(use-package python
  :straight (:type built-in)
  :mode (("\\.py$" . python-mode))
  ;; note: no :ensure since it is already built into emacs
  :config
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


(use-package go-ts-mode
  :straight (:type built-in)
  :mode (("\\.go$"  . go-ts-mode)
	 ("^go.mod$" . go-ts-mode))
  :config
  (message "go-ts-mode config ...")
  ;; Set up on-save hooks to have eglot format and organize imports.
  (add-hook 'go-ts-mode-hook 'my-add-eglot-format-on-save-hook)
  ;; Sets the fill column (where to break paragraphs on M-q)
  (add-hook 'go-ts-mode-hook (lambda () (setq fill-column 100))))


;; Debug programs using `delve'. This debugger relies on Emacs' GUD framework.
(use-package go-dlv
  :straight t
  :commands (dlv dlv-current-function)
  :config)

;; golangci-lint support via flymake
(use-package flymake-golangci
  :straight t
  :hook (go-mode . flymake-golangci-load))


;; Major mode for json file editing.
(use-package json-ts-mode
  :straight t
  :mode (("\\.json$" . json-ts-mode))
  :config
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
  :straight (:type built-in)
  :mode (("\\.js$" . js-ts-mode)
	 ("\\.jsx$" . js-ts-mode))
  :config
  (setq
   indent-tabs-mode nil
   js-indent-level 2))


;; Enable the Prettier code-formatter's minor mode to format on save whenever we
;; edit JavaSciprt/JSX.  https://prettier.io/.
(use-package prettier
  :straight t
  :hook ((json-ts-mode . prettier-mode)
	 (yaml-mode . prettier-mode)
	 (gfm-mode . prettier-mode)
	 (markdown-mode . prettier-mode)))


;; Major mode for yaml file editing.
(use-package yaml-mode
  :straight t
  :mode (("\\.yaml\\(.gotmpl\\)?$" . yaml-mode)
         ("\\.yml\\(.gotmpl\\)?$" . yaml-mode))
  :config
  (setq indent-tabs-mode nil) ; no tabs for indentation
  (add-hook 'yaml-mode-hook #'my-highlight-todos)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'yaml-mode-hook 'my-untabify-on-save-hook)
  (add-hook 'yaml-mode-hook 'my-strip-on-save-hook))


;; Major mode for markdown (.md) file editing.
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md$" . gfm-mode)
         ("\\.markdown$" . gfm-mode)
         ;; cheat sheets under ~/dotfiles/cheat/sheets
         ("\\.cheat$" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  ;; no tabs for indentation
  (setq indent-tabs-mode nil)
  (setq markdown-list-indent-width 2)

  ;; Define key-bindings.
  (let ((m markdown-mode-map))
    ;; Subtree, list, and table editing
    (define-key m (kbd "M-<up>")    #'markdown-move-up)
    (define-key m (kbd "M-<down>")  #'markdown-move-down)
    (define-key m (kbd "M-<left>")  #'markdown-promote)
    (define-key m (kbd "M-<right>") #'markdown-demote))

  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'markdown-mode-hook 'my-untabify-on-save-hook))


(use-package markdown-preview-mode
  :straight t
  ;; Lazily load when called for.
  :bind ("C-c p m" . markdown-preview-mode)
  :config
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
  :disabled t
  :mode (("\\.vcl$" . vcl-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'vcl-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'vcl-mode-hook #'my-strip-on-save-hook))


;; Dockerfile editing
(use-package dockerfile-mode
  :straight t
  :mode (("^.*Dockerfile$" . dockerfile-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'dockerfile-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'dockerfile-mode-hook #'my-strip-on-save-hook))


;; TOML editing
(use-package toml-mode
  :straight t
  :mode (("\\.toml$" . toml-mode))
  :config
  (add-hook 'toml-mode-hook #'my-enable-line-numbers-mode)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'toml-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'toml-mode-hook #'my-strip-on-save-hook))


(use-package terraform-mode
  :straight t
  :disabled t
  :mode (("\\.tf$" . terraform-mode))
  :config
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'terraform-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'terraform-mode-hook #'my-strip-on-save-hook))


(use-package protobuf-mode
  :straight t
  :mode (("\\.proto$" . protobuf-mode))
  :config
  (add-hook 'protobuf-mode-hook #'my-highlight-todos)
  (add-hook 'protobuf-mode-hook #'my-enable-line-numbers-mode)
  ;; add buffer-local save hook only for buffers in this mode
  (add-hook 'protobuf-mode-hook #'my-untabify-on-save-hook)
  (add-hook 'protobuf-mode-hook #'my-strip-on-save-hook))


;; Rust-mode
(use-package rust-mode
  :straight t
  :mode (("\\.rs$" . rust-mode))
  :config
  (setq indent-tabs-mode nil)
  ;; automatic formatting
  (setq rust-format-on-save t))


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
  :straight (:type built-in)
  :hook cc-mode
  :config
  (add-hook 'c-mode-hook   #'my-c-mode)
  (add-hook 'c++-mode-hook #'my-c++-mode))


;; cmake setup.
(use-package cmake-mode
  :straight t
  :mode (("CMakeLists.txt$" . cmake-mode)
         ("\\.cmake$" . cmake-mode))
  :config
)


;; remove "ElDoc" from modeline
(use-package eldoc
  :straight (:type built-in)
  :diminish eldoc-mode)


;; can be used for working with .groovy and Jenkinsfile
(use-package groovy-mode
  :straight t
  :disabled t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gvy$" . groovy-mode)
	 ("\\.gy$" . groovy-mode)
	 ("^Jenkinsfile$" . groovy-mode)))


;; emacs mode to edit GraphQL schemas and queries (automtically enabled when
;; opening .graphql and .gql files)
(use-package graphql-mode
  :straight t
  :mode (("\\.gql$" . graphql-mode)
         ("\\.graphql$" . graphql-mode))
  :hook ((graphql-mode . prettier-mode)))


;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :straight t
  :defer 5
  :diminish
  :config
  ;; show popup at the bottom of the window
  (which-key-setup-side-window-bottom)
  ;; separator between key and bound command. Defaults to ' → '.
  (setq which-key-separator " ")
  ;; delay after which which-key popup appears after starting to type a command
  (setq which-key-idle-delay 1.0)
  (which-key-mode))


;; Built-in browse-url.el package.
(use-package browse-url
  :straight (:type built-in)
  ;; Lazily load when called for.
  :bind (("C-c u o" . browse-url-xdg-open))) ;; "URL open"


;; org-mode
(use-package org
  :straight (:type built-in)
  ;; Lazily load when a .org file is opened.
  :mode ("\\.org$" . org-mode)
  ;; Lazily load when called for.
  :bind (("C-c o o" . my-org-open)
         ("C-c o l" . org-store-link)
         ("C-c o c" . org-capture)
         ("C-c o a" . org-agenda))
  :init
  ;; make org-mode table editor available in text-mode (or derived modes)
  (add-hook 'text-mode-hook #'my-enable-orgtbl-mode)
  :config
  ;; Key-bindings.
  (let ((m org-mode-map))
    ;; x as in "check as done".
    (define-key m (kbd "C-c o x") #'org-archive-subtree)
    (define-key m (kbd "C-c o >") #'org-clock-in)
    (define-key m (kbd "C-c o <") #'org-clock-out)
    (define-key m (kbd "C-c C-s") #'org-schedule)
    (define-key m (kbd "C-c C-d") #'org-deadline)
    ;; jump to heading with live preview
    (define-key m (kbd "C-c o h") #'consult-outline))
  ;; Always run in org-indent-mode (level by indent rather than asterisks).
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
  ;; Extend the org-mode markup to be fontified like markdown:
  ;; - surround with "`": verbatim/code face
  ;; - surround with "**": bold
  ;; - surround with "*": italic
  ;; - surround with "_": underline
  ;; - surround with "~": strike-through
  ;; This should work both for single-line sentences and (fill-column) folded
  ;; sentences.
  (font-lock-add-keywords 'org-mode
			  '(("`[^`]+`" 0 'org-verbatim prepend)
			    ("\\*\\*[^\\*]*\\*\\*" . 'bold)
			    ("\\*[^\\*]*\\*" . 'italic))
			  'append)
  (setq org-emphasis-alist '(("_" underline)
                             ("~" (:strike-through t))))
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
  :straight (:type built-in)
  :mode (("\\.rb$"  . ruby-mode))
  :config)


(use-package vterm
  :straight t
  ;; Lazily load when called for.
  :bind (("C-c v t" . vterm-other-window))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (let ((m vterm-mode-map))
    ;; Toggle from term input to emacs buffer (search/copy) mode.
    (define-key m (kbd "C-c C-t") #'vterm-copy-mode)
    (define-key m (kbd "C-c C-l") #'vterm-clear-scrollback)))


;; a package for making screencasts within Emacs.
(use-package gif-screencast
  :straight t
  ;; Lazily load when called for.
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
