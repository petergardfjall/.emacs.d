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
   ;; Set Gemini model to use. Can change in-session with `gptel-menu' followed by `-m'.
   ;; Gemini models are described here:
   ;;   https://ai.google.dev/gemini-api/docs/models.
   ;; Can also be listed with:
   ;;   curl "https://generativelanguage.googleapis.com/v1beta/models?key=${GEMINI_API_KEY}"

   ;; gptel-model 'gemini-2.5-pro   ;; Powerful model for agentic coding.
   gptel-model 'gemini-2.5-flash ;; Well-rounded, low-latency, high-volume tasks.

   gptel-backend (gptel-make-gemini
                  "Gemini"
                  :key (getenv "GEMINI_API_KEY")
                  :stream t)))


(provide 'my-ai)
;;; my-ai.el ends here.
