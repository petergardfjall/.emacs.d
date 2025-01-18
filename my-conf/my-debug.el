;;; package --- Configuration of debugging tools.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Debug Adapter Protocol (DAP) client for Emacs.
;;
;; Some quick
;; Use `dape' to start a debugging session.
;; Use `dape-kill' to stop a debugging session.
;; Use `dape-info' to show dape windows.
;; Use `dape-breakpoint-toggle' to set/unset breakpoints.
;; Use `dape-breakpoint-save' to write breakpoints to a file.
;; Use `dape-breakpoint-load' to load them.
(use-package dape
  :straight t
  :commands (dape dape-info dape-breakpoint-load  dape-breakpoint-toggle)
  :config
  (setq dape-buffer-window-arrangement 'right)
  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  ;; Customize selection of window for displaying the source code when clicking
  ;; a stack trace entry. The default seems to select the *projtree* window
  ;; despite it being a dedicated window.
  ;; Note: this is used as a `display-buffer' ACTION argument.
  (setq dape-display-source-buffer-action
        '((display-buffer-use-some-window display-buffer-pop-up-window)))
  ;; Save unsaved buffers when starting a `dape' session.
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  ;; Set up additional `dape-configs'.
  ;;
  ;; Add a "dlv-attach" config for attaching to a running debugger.
  ;;   dlv exec --listen=localhost:33284 --headless <binary> [argv]
  (add-to-list 'dape-configs
	       `(dlv-attach ;; Name of configuration.
	         modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
	         command "dlv"
                 command-cwd default-directory
                 host "127.0.0.1"
                 port 33284
	         :type "go"
	         :request "attach" ;; Will run "dlv attach".
	         :mode "remote"    ;; Connect to a running debugger.
                 :showLog "true")))


(use-package hydra
  :straight t
  :commands (defhydra)
  :config

  (defhydra my-debug-hydra (:hint nil)
    "
  Breakpoint: _b_: toggle    _s_: save  _l_: load
  Session:    _d_: start     _i_: info  _k_: kill
  Step:       _c_: continue  _→_: next  _↓_: step-in  _↑_: step-out
  "
    ("d"       dape)              ;; Start debug session.
    ("b"       dape-breakpoint-toggle :exit t)
    ("s"       dape-breakpoint-save)
    ("l"       dape-breakpoint-load)
    ("<right>" dape-next)         ;; Step one line (skip functions).
    ("<down>"  dape-step-in)      ;; Step into function.
    ("<up>"    dape-step-out)     ;; Step out of function.
    ("c"       dape-continue)
    ("i"       dape-info)         ;; Create/display dape info buffers.
    ("k"       dape-quit :exit t) ;; Kill debug session.
    ("q"       nil))
  (define-key global-map (kbd "C-x C-d") #'my-debug-hydra/body))


(provide 'my-debug)
;;; my-debug.el ends here.
