;;; package --- Text-to-speech functions.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(defcustom my-tts-voice 'us
  "Voice to use for `my-text-to-speech'.
Must be a key in 'my-tts-voice-model-alist'."
  :options '(us swe))

(defconst my-tts-voice-model-alist
  '((us . "en_US-ryan-high.onnx")
    (swe . "sv_SE-nst-medium.onnx"))
  "Maps a voice short-name to a voice model file.")

(defvar my-piper-binary-path "/opt/bin/piper"
  "File system location of piper executable.")
(defvar my-piper-voice-model-dir "/opt/piper-v1.2.0/voices/"
  "File system folder where voice models (.onnx files) are stored.")
(defvar my-piper-aplay-parameters "-r 22050 -f S16_LE -t raw -"
  "Command-line parameters to use to play sounds with aplay.")


(defun my-text-to-speech (&optional text)
  "Translate TEXT to speech using piper.
If a region is active, the marked text is spoken.
If called interactively without active region it prompts for TEXT input."
  (interactive
   (list
    (cond
     ((region-active-p) (buffer-substring-no-properties
                         (region-beginning) (region-end)))
     (t (read-string "Text to be spoken: ")))))
  (my--run-piper text))


(defun my--run-piper (text)
  "Run piper to translate TEXT to speech."
  (let ((voice-model (my--voice-model)))
    (start-process "piper" "*piper*" "sh" "-c"
                   (format "echo '%s' | %s --model %s  --output-raw | aplay %s 2>/dev/null" text my-piper-binary-path voice-model my-piper-aplay-parameters))))


(defun my--voice-model ()
  "Locate the currently selected voice model file."
  (let* ((voice-file (cdr (assoc my-tts-voice my-tts-voice-model-alist))))
    (concat my-piper-voice-model-dir voice-file)))


(provide 'my-text-to-speech-functions)
;;; my-text-to-speech-functions.el ends here.
