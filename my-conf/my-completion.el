;;; package --- Completion-related configuration.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

;;
;; Minibuffer completion.
;;

(defun my-completions-buffer-truncate-lines ()
  "Don't wrap *Completions* buffer lines."
  (with-current-buffer "*Completions*"
    (setq truncate-lines t)))

;; Set up to use the built-in *Completions* buffer for both types of completion:
;;
;; - Minibuffer completion (`completing-read') like M-x, C-x C-f, C-x b, etc.
;; - In-buffer completion (`completion-at-point'), normally summoned by M-TAB or such.
;;
(use-package minibuffer
  :straight (:type built-in)
  :hook ((minibuffer-setup . cursor-intangible-mode)
	 (completion-setup . my-completions-buffer-truncate-lines))
  :config
  ;; Generic minibuffer properties.
  (setq
   enable-recursive-minibuffers t
   minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt)
   minibuffer-depth-indicate-mode t
   minibuffer-electric-default-mode t)

  ;;
  ;; Generic config for *Completions* buffer.
  ;;
  (setq
   ;; Update candidates as I type.
   completion-eager-update t
   ;; Bring up Completions buffer immedidately on `completing-read'.
   completion-eager-display t
   ;; Do not inform about default keybindings.
   completion-show-help nil
   ;; Do not show messages in echo area pertaining to completion.
   ;; (setq completion-show-inline-help nil)
   ;; Show useful annotations in minibuffer prompts. A bit like `marginalia'.
   completions-detailed t
   completions-group nil
   completions-format 'one-column
   completions-max-height 10
   ;; Surface previous inputs towards the top of the list.
   completions-sort 'historical)
   ;; Unbind `minibuffer-complete-word'.
  (keymap-unset minibuffer-local-completion-map "SPC")

  ;;
  ;; In-buffer completion.
  ;;

  ;; Summon in-buffer completion at any time with "C-<tab>".
  (define-key global-map (kbd "C-<tab>") #'completion-at-point)
  (setq
   ;; Hitting TAB first tries to indent the current line, and if the line was
   ;; already indented, it tries `completion-at-point'.
   tab-always-indent 'complete
   ;; Show the Completions buffer if I hit TAB but there is no unique match yet.
   completion-auto-help t)

  ;;
  ;; Minibuffer completion.
  ;;
  (setq
   ;; Determine how to match minibuffer input text against completion candidates.
   ;; Can be extended with the `orderless' package.
   completion-styles '(basic flex)

   ;; Open *Completions* buffer on first TAB, move point to it on second TAB.
   completion-auto-select 'second-tab
   ;; Allow up/down navigation of Completions buffer candidates while typing in
   ;; minibuffer.
   minibuffer-visible-completions 'up-down

   ;; Ignore case on various forms of `completing-read' (minibuffer completion).
   completion-ignore-case t
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t))


;; A built-in UI for `completing-read' ("minibuffer completion"). We configure
;; it to use vertical candidate display in the minibuffer. Also see:
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
(use-package icomplete
  :straight (:type built-in)
  :disabled t
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


;; Adds richer annotations for minibuffer completions for any `completing-read'
;; compatible framework (such as icomplete, selectrum and vertico).
(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  ;; (setq marginalia-field-width 40)
  ;; (defun my-project-buffer-annotator (cand)
  ;;   (let* ((buffer (get-buffer cand)))
  ;;     (when-let* ((buffer-file (buffer-file-name buffer))
  ;;                 (buffer-proj (project-current nil buffer-file))
  ;;                 (project-dir (project-root buffer-proj))
  ;;                 (project-short (project-name buffer-proj)))
  ;;       (let ((project-rel-dir (file-name-directory (file-relative-name buffer-file project-dir))))
  ;;         (marginalia--fields
  ;;          (project-short :truncate 0.4 :face 'marginalia-value)
  ;;          (project-rel-dir :truncate 0.4 :face 'marginalia-documentation))))))
  ;; update annotator-registry to use my custom annotator for buffers
  ;; (add-to-list 'marginalia-annotators
  ;;              '(buffer my-project-buffer-annotator none))
  :config
  (let ((m minibuffer-local-map))
    (define-key m (kbd "M-A") #'marginalia-cycle)))


;; Add nerd icons to minibuffer completion.
(use-package nerd-icons-completion
  :straight t
  :disabled t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-to-list 'nerd-icons/devicon-alist '("nf-dev-go" . "") nil)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;; Orderless provides a different type of `completion-styles' for minibuffer
;; completion, where space-separated words can be input as search terms.
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
          ;; Use basic completion and then orderless for `find-file' (C-x C-f).
          (file (styles basic orderless))
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


;;
;; In-buffer (`completion-at-point') completion.
;;

;; A UI for `completion-at-point' ("in-buffer completion"). It shows completion
;; candidates in a popup overlay. Completions candidates are provided by
;; `completion-at-point-functions' such as `eglot-completion-at-point'.
(use-package company
  :straight t
  :disabled t
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


;; A UI for `completion-at-point' ("in-buffer completion"). It shows completion
;; candidates in a popup overlay. Completions candidates are provided by
;; `completion-at-point-functions' such as `eglot-completion-at-point'.
(use-package corfu
  :straight t
  :disabled t
  :init
  (global-corfu-mode)
  :config
  ;; Summon completion at any time with "C-<tab>".
  (define-key global-map (kbd "C-<tab>") #'completion-at-point)
  (setq
   corfu-auto t               ;; Enable (unsummoned) auto-completion.
   corfu-auto-prefix 3        ;; Min chars before auto-completion is triggered.
   corfu-auto-delay 0.2       ;; Delay (s) after typing until popup appears.
   corfu-echo-documentation t ;; Show candidate documentation in echo area.
   corfu-preview-current nil  ;; Disable current candidate preview.
   corfu-quit-no-match t      ;; Quit when no candidates remain.
   corfu-count 15             ;; Maximum number of candidates to display.
   corfu-min-width 20))       ;; Miniumum popup width (in characters).


;; Extensions for `completion-at-point'.
(use-package cape
  :straight t
  :after corfu
  :init
  ;; Complete word from current buffers.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Avoid having `corfu' (`completion-at-point' ui) base all completion on the
  ;; first candidate list retrieved from the LSP server. With this "cache
  ;; buster" Eglot's completion at point gets to re-fetch the candidate list
  ;; from the LSP server on every keystroke. This behavior is identical to
  ;; `company'. For a lengthy discussion see
  ;; https://github.com/joaotavora/eglot/discussions/1127#discussioncomment-7593169
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


(provide 'my-completion)
;;; my-completion.el ends here.
