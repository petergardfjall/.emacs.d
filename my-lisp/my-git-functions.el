;;; package --- Assorted git-related functions.  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(message "loading %s ..." load-file-name)

(defun my--github-https-repo-url (remote-url)
  "Return a normalized https GitHub repo URL for a REMOTE-URL.
For example both of the REMOTE-URLs `git@github.com:/owner/repo' and
`https://github.com/owner/repo.git' would be turned to
`https://gthub.com/owner/repo'."
  (let* ((remote-url (string-remove-suffix ".git" remote-url)))
    (if (string-prefix-p "git@github.com" remote-url)
        ;; Translate ssh-style URL: git@github.com:/owner/repo
        (let* ((repo-path (string-remove-prefix "git@github.com:" remote-url)))
          (file-name-concat "https://github.com" (string-trim-left repo-path "/+")))
      (if (not (string-match-p "github.com" remote-url))
          (error "Not a GitHub repository: %s" remote-url)
        remote-url))))

(defun my-github-browse-url-at-point ()
  "Open a web browser that visits the GitHub file at point.
An error is output if the buffer file is not from a GitHub repository."
  (interactive)
  (with-current-buffer (current-buffer)
    (unless (vc-git-root (buffer-file-name))
      (error "Not visiting a git file"))
    (let* ((git-remote (vc-git-repository-url (buffer-file-name)))
           (https-url (my--github-https-repo-url git-remote))
           (current-branch (car (vc-git-branches)))
           (git-root (vc-git-root (buffer-file-name)))
           (file  (file-relative-name (buffer-file-name) git-root))
           (lineno (if (region-active-p)
                       (format "L%d-L%d" (line-number-at-pos (region-beginning)) (line-number-at-pos (region-end)))
                     (format "L%d" (line-number-at-pos))))
           (github-url (format "%s/blob/%s/%s?plain=1#%s" https-url current-branch file lineno)))
      (message "Browse URL: %s" github-url)
      (browse-url github-url))))

(provide 'my-git-functions)
;;; my-git-functions.el ends here.
