;;; wsp.el --- Functions for a workspace-centric workflow.  -*- lexical-binding: t -*-
;;
;; Copyright © 2020 Peter Gardfjäll <peter.gardfjall@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall@gmail.com>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: workspace, project
;; Package-Requires: ((emacs "25.2") (cl-lib "0.5") (treemacs "2.8") (projectile "2.2.0"))
;; Version: 0.0.1
;; Homepage: https://github.com/petergardfjall/wsp
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; A collection of functions and interactive commands that coordinate the use of
;; `treemacs`, `projectile` and `desktop-save-mode` to support a
;; workspace-centric workflow.
;;
;; The user can manage and switch between a collection of workspaces, each of
;; which tracks a set of projects (directory roots).  The user can manage and
;; switch between projects within the workspace.

;;; Code:

(require 'projectile)
(require 'cl-lib)
(require 'desktop)
(require 'treemacs)

(defvar wsp-workspaces-dir (concat (file-name-as-directory user-emacs-directory) "wsp")
  "Minimum frame width when treemacs is enabled (in characters).")

(defvar wsp-desktop-filename  "desktop"
  "The file name to use for `desktop-save-mode` state.")

(defvar wsp--default-treemacs-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
  "The default value for treemacs state file.")

(defvar wsp--current-workspace nil
  "Tracks the name of the workspace currently open.")


;;
;; workspaces
;;

(defun wsp-workspace-open (name &optional first-project-dir)
  "Open (or switch to) workspace NAME.
When creating a new workspace, the first project directory to
include in workspace may optionally be passed as
FIRST-PROJECT-DIR.  If it's not passed, the user will be
prompted."
  (interactive
   (list (completing-read "Select workspace: " (wsp-workspace-list) nil nil)))
  ;; if a workspace is already open, close it
  (when (wsp-workspace-current)
    (wsp-workspace-close))

  (unless (wsp-workspace-exists name)
    (wsp--workspace-create name first-project-dir))
  (wsp--workspace-init name)
  (wsp--workspace-load name))


(defun wsp--workspace-create (name &optional first-project-dir)
  "Create a workspace named NAME.
The first project directory to include in workspace can
optionally be passed as FIRST-PROJECT-DIR.  If it's not passed,
the user will be prompted."
  (unless (string-match "^[_.0-9A-Za-z\\-]+$" name)
    (error "A workspace name may only contain a-z, A-Z, 0-9, underscore (_), dash (-), and dot (.)"))

  (wsp--workspace-init name)
  (message "creating new workspace '%s' ..." name)
  (make-directory (wsp--workspace-dir name) t)

  ;; read initial project directory
  (let* ((proj-path (or first-project-dir
			 (read-directory-name "Add first workspace project directory:")))
	 (dir-name (wsp--basename proj-path))
	 (p1 (treemacs-project->create! :name dir-name :path proj-path :path-status 'local-readable))
	 (ws (treemacs-workspace->create! :name name :projects (list p1))))
    ;; add first project to projectile (will also save state)
    (projectile-add-known-project proj-path)
    ;; create treemacs state file
    (setq treemacs--workspaces (list ws))
    (setf (treemacs-current-workspace) ws)
    (put 'treemacs :state-is-restored t)
    (treemacs--persist)))


(defun wsp--workspace-init (name)
  "Reset and prime libraries for being used with workspace named NAME."
  (wsp--desktop-init name)
  (wsp--projectile-init name)
  (wsp--treemacs-init name))


(defun wsp-workspace-close ()
  "Close the current workspace (if one is open)."
  (interactive)
  (wsp--ensure-workspace-open)

  ;; save current desktop
  (desktop-save desktop-dirname t)
  ;; disable desktop-save mode
  (desktop-save-mode 0)

  ;; close all open project buffers
  (dolist (project-name (wsp-project-list))
    (wsp-project-close project-name))

  ;; save treemacs state
  (treemacs--persist)
  ;; disable/close treemacs
  (delete-window (treemacs-get-local-window))
  (kill-buffer (treemacs-get-local-buffer))
  ;; reset the treemacs workspace state
  (setq treemacs-persist-file wsp--default-treemacs-file)
  (setf treemacs--workspaces (list (treemacs-workspace->create! :name "Default"))
        (treemacs-current-workspace) (car treemacs--workspaces))

  ;; disable projectile
  (projectile-mode 0)

  ;; set no current workspace
  (setq wsp--current-workspace nil))



(defun wsp-workspace-current ()
  "Return the name of the currently open workspace or nil if none is open."
  (interactive)
  wsp--current-workspace)


(defun wsp-workspace-delete (name)
  "Delete the workspace named NAME.
If this happens to be the current workspace, it is first closed."
  (interactive
   (list (completing-read "Select workspace to delete: "
			  (wsp-workspace-list) nil t)))
  ;; if deleting the current workspace, first close it.
  (when (string= (wsp-workspace-current) name)
    (wsp-workspace-close))
  ;; ... then perform delete
  (when (wsp-workspace-exists name)
    (message "deleting workspace %s ..." name)
    (delete-directory (wsp--workspace-dir name) t)))


(defun wsp-workspace-exists (name)
  "Determine if a saved workspace named NAME exists."
  (member name (wsp-workspace-list)))


(defun wsp-workspace-list ()
  "List the collection of known (saved) workspaces."
  (wsp--ensure-workspaces-dir)
  (mapcar #'wsp--basename (wsp--list-dirs wsp-workspaces-dir)))


(defun wsp--ensure-workspace-open ()
  "Fails with an error if no workspace is currently open."
  (if (not (wsp-workspace-current))
      (error "No workspace opened")
    t))


(defun wsp--workspace-load (name)
  "Load workspace named NAME."
  (message "loading workspace %s ..." name)
  (wsp--projectile-activate name)
  (wsp--desktop-activate name)
  (wsp--treemacs-activate name)

  (setq wsp--current-workspace name)
  (message "workspace '%s' loaded." name))


(defun wsp--desktop-init (name)
  "Disable and prepare `desktop-save-mode` for use in workspace NAME."
  (desktop-save-mode 0)
  ;; save settings
  (let ((save-path (wsp--desktop-save-path name)))
    ;; state dir
    (setq desktop-dirname (file-name-directory save-path))
    ;; state file
    (setq desktop-base-file-name (file-name-nondirectory save-path))
    ;; directory where desktop state is to be loaded from.
    (setq desktop-path (list desktop-dirname)))

  ;; load the desktop if locked?
  (setq desktop-load-locked-desktop 'ask)
  ;; always save on exit (without prompting)
  (setq desktop-save t))


(defun wsp--treemacs-init (name)
  "Close any existing treemacs session and prepare `treemacs` for use in workspace NAME."
  ;; close any existing open treemacs
  (when (equal (treemacs-current-visibility) 'visible)
    (delete-window (treemacs-get-local-window))
    (kill-buffer (treemacs-get-local-buffer))
    (treemacs--invalidate-buffer-project-cache))
  ;; set state file
  (setq treemacs-persist-file (wsp--treemacs-statefile name))
  ;; mark that treemacs state has not yet been loaded
  (put 'treemacs :state-is-restored nil))


(defun wsp--projectile-init (name)
  "Disable and prepare `projectile` for use in workspace NAME."
  (projectile-mode 0)
  ;; projectile shouldn't automatically register known projects
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-known-projects-file
	(wsp--projectile-projects-file name)))


(defun wsp--desktop-load-hook ()
  "To be called when a desktop is successsfully loaded."
  (message "desktop loaded: %s" desktop-path))

(defun wsp--desktop-activate (name)
  "Activate/load `desktop-save-mode` in workspace NAME."
  (desktop-save-mode +1)

  ;; on successsful desktop-read
  (add-hook 'desktop-after-read-hook #'wsp--desktop-load-hook)
  ;; on failed desktop-read: disable desktop-save-mode, since we want to
  ;; disable automatic saving of desktop on exit in this case.
  (add-hook 'desktop-not-loaded-hook
	    (lambda ()
	      (display-warning :warning "couldn't load desktop. disabling desktop-save-mode ...")
	      (desktop-save-mode 0)))

  ;; try to load saved desktop if one exists
  (when (file-exists-p (wsp--desktop-save-path name))
    (desktop-read)))


(defun wsp--projectile-activate (name)
  "Activate/load `projectile` in workspace NAME."
  (projectile-mode +1))


(defun wsp--treemacs-activate (name)
  "Activate/load `treemacs` in workspace NAME."

  ;; move point back to current buffer after displaying treemacs
  (save-selected-window
    ;; restore treemacs state from saved workspace file
    (setf (treemacs-current-workspace) (car (treemacs--restore)))
    ;; tells treemacs that state has already been restored
    (put 'treemacs :state-is-restored t)
    (treemacs)))


(defun wsp--desktop-save-path (name)
  "State file destination for `desktop-save-mode` in workspace NAME."
  (concat (wsp--workspace-dir name) wsp-desktop-filename))

(defun wsp--projectile-projects-file (name)
  "State file destination for `projectile` in workspace NAME."
  (concat (wsp--workspace-dir name) "projectile-bookmarks.eld"))

(defun wsp--treemacs-statefile (name)
  "State file destination for `treemacs` in workspace NAME."
  (concat (wsp--workspace-dir name) "treemacs-persist"))

(defun wsp--workspace-dir (name)
  "Workspace state directory for workspace NAME."
  (file-name-as-directory (concat (file-name-as-directory wsp-workspaces-dir) name)))



;;
;; projects
;;

(defun wsp-project-list ()
  "Return the name of each project added to the current workspace."
  (interactive)

  (if (not (wsp-workspace-current))
      nil ;; no workspace set => empty list
    (if (bound-and-true-p projectile-mode)
	;; note: may not be unique if we only care about the dirname
	(let ((projects (mapcar 'file-name-nondirectory
				(mapcar 'directory-file-name projectile-known-projects))))
	  (sort projects 'string-lessp))
      (error "Cannot list projects when not in projectile-mode"))))


(defun wsp-project-current ()
  "Return the wsp project open in the current buffer or nil."
  (interactive)
  (wsp--ensure-workspace-open)

  (let ((proj-path (cl-find-if (lambda (proj)
				 (string-prefix-p proj (buffer-file-name (window-buffer))))
			       (wsp--projectile-known-projects-expanded))))
    (if proj-path
	(message "%s" (wsp--basename proj-path))
      nil)))

(defun wsp-project-add (dir-path)
  ""
  (interactive (list (and (wsp--ensure-workspace-open) (read-directory-name "Project directory to add:"))))

  (let (project-name (wsp--basename dir-path))
    (projectile-add-known-project dir-path)
    (treemacs-add-project-to-workspace dir-path project-name)
    ;;(put 'treemacs :state-is-restored t)
    (treemacs--persist))
  (message "project added: %s" dir-path))


(defun wsp-project-close (project-name)
  "Close all project buffers for PROJECT-NAME."
  (interactive
   (list (completing-read "Select project: " (wsp-project-list) nil t)))

  (dolist (buffer (wsp--project-buffers project-name))
    (kill-buffer buffer)))


(defun wsp-project-close-current ()
  "Close the currently open project."
  (interactive)
  (if (wsp-project-current)
      (wsp-project-close (wsp-project-current))
    (error "Not visiting a wsp project buffer")))


(defun wsp-project-close-other ()
  "Close all open projects except for the currently open project."
  (interactive)
  (if (wsp-project-current)
      (dolist (other-proj (cl-remove-if (lambda (proj-name) (string= proj-name (wsp-project-current)))
					(wsp-project-list)))
	(wsp-project-close other-proj))
    (error "Not visiting a wsp project buffer")))


(defun wsp-project-delete (project-name)
  "Delete PROJECT-NAME from current workspace."
  (interactive
   (list (completing-read "Select project: " (wsp-project-list)  nil t)))

  ;; close all project buffers
  (wsp-project-close project-name)
  (let* ((project-path (wsp--projectile-project project-name))
	 (abs-path (expand-file-name project-path)))
    (projectile-remove-known-project project-path)
    (treemacs--remove-project-from-current-workspace (treemacs--find-project-for-path abs-path))
    (treemacs--persist)
    (treemacs--rerender-after-workspace-change)
    (message "project removed: %s (%s)" project-name project-path)))


(defun wsp-project-switch (project-name)
  ""
  (interactive
   (list (completing-read "Switch to project: " (wsp-project-list)  nil t)))
  (let ((proj (wsp--projectile-project project-name))
	(proj-buffers (wsp--project-buffers project-name)))
    (if proj-buffers
	;; if a project buffer is already opened, switch to that
	(switch-to-buffer (car proj-buffers))
      ;; otherwise: select a buffer to open via projectile
      (projectile-switch-project-by-name proj))))

(defun wsp--basename (path)
  "Return the base name of a PATH.
For a PATH of either `/a/b/c/` or `/a/b/c`, the result is `c`."
  (file-name-nondirectory (directory-file-name path)))

(defun wsp--list-dirs (path)
  ""
  (cl-remove-if 'wsp--isdot-p
	     (cl-remove-if-not 'wsp--isdir-p (directory-files path t))))

(defun wsp--isdot-p (path)
  "Determine if PATH is a `.` or `..` file entry."
  ;; TODO
  (or (string= (file-name-base path) ".") (string= (file-name-base path) "..")))

(defun wsp--isdir-p (path)
  ""
  (file-attribute-type (file-attributes path)))


(defun wsp--projectile-project (project-name)
  "Return the projectile name/path corresponding to the wsp PROJECT-NAME."
  (cl-find-if (lambda (proj) (string= (wsp--basename proj) project-name))
	      projectile-known-projects))

(defun wsp--projectile-known-projects-expanded ()
  (mapcar #'expand-file-name  projectile-known-projects))


(defun wsp--project-buffers (project-name)
  ""
  ;; get all project buffers except special buffers
  (cl-remove-if
   (lambda (buffer) (member (buffer-name buffer) '("*scratch*" "*Messages*" "*Backtrace*" )))
   ;; note: projectile-project-buffers seems to require full path (not
   ;; `~/some/proj` as returned by `projectile-known-projects`).
   (let ((project-path (expand-file-name (wsp--projectile-project project-name))))
     (projectile-project-buffers project-path))))


(defun wsp--ensure-workspaces-dir ()
  "Ensures that the workspaces directory has been created."
  (make-directory wsp-workspaces-dir t))


(provide 'wsp)

;;; wsp.el ends here
