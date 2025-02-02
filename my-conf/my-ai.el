;;; package --- Configuration of AI tools.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)


;; Emacs large language model (LLM) client.
;; Use `gptel' to open a chat buffer.
;; Use `gptel-send' (C-c RET) to send a query.
(use-package gptel
  :straight t
  :commands (gptel gptel-send)
  :config
  (setq
   ;; Gemini 1.5 Flash is free of charge with lower rate limits
   ;; (15 requests/min and 1500 requests/day).
   gptel-model 'gemini-1.5-flash
   gptel-backend (gptel-make-gemini
                  "Gemini"
                  :key (getenv "GEMINI_API_KEY")
                  :stream t)))


(provide 'my-ai)
;;; my-ai.el ends here.
