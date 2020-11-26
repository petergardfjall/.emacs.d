;;; wsp-test.el -- tests for wsp.el -*- lexical-binding: t -*-
;;
;; Copyright © 2020 Peter Gardfjäll <peter.gardfjall@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall@gmail.com>
;;
;; Package-Requires: ((cl-lib "0.5") (treemacs "2.8") (projectile "2.2.0"))
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
;;; Commentary:
;;
;; This file is part of wsp.el
;;
;;; Code:

(require 'cl-lib)
(require 'wsp)

(defvar wsp-test-dir
  (file-name-as-directory (concat default-directory "tests")))

(defvar wsp-test-assets-dir
  (file-name-as-directory (concat wsp-test-dir "assets")))

(defvar wsp-test-state-dir
  (file-name-as-directory (concat default-directory "sandbox/wsp")))

(defun wsp-test-project-dir (name)
  (file-name-as-directory (concat wsp-test-assets-dir name)))

(defun wsp-test-project-file (project-name file-name)
   (concat (wsp-test-project-dir project-name) file-name))

(defun wsp-test-workspace-state-dir (workspace-name)
  (file-name-as-directory (concat wsp-test-state-dir workspace-name)))

(defun wsp-test-workspace-state-file (workspace-name file-name)
   (concat (wsp-test-workspace-state-dir workspace-name) file-name))


(defun wsp-test-init ()
  "Any code to run before a test function starts."
  )

(defun wsp-test-end ()
  "Any code to run when a test function ends."
  (print "test done" #'external-debugging-output)
  (save-buffers-kill-emacs))


(setq wsp-workspaces-dir wsp-test-state-dir)


(defun wsp-test-buffers-open-p (buffer-names)
  (cl-every #'get-buffer buffer-names))

(defun wsp-test-buffers-not-open-p (buffer-names)
  (cl-notany #'get-buffer buffer-names))

(defun wsp-treemacs-open-project-names ()
  (cl-loop for p in (treemacs-workspace->projects (treemacs-current-workspace))
	 collect (treemacs-project->name p)))

(defun wsp-test-workspace-create ()
  (wsp-test-init)

  ;;
  ;; check preconditions
  ;;
  (cl-assert (not (wsp-workspace-list))
	     "shouldn't be any known workspaces at this point")
  (cl-assert (not (wsp-workspace-current))
	     "expected `(wsp-workspace-current)` to be nil")
  (cl-assert (not (wsp-workspace-exists "workspace1"))
	     "expected workspace1 to not exist")

  ;;
  ;; open workspace
  ;;
  (wsp-workspace-open "workspace1" (wsp-test-project-dir "proj1"))

  (cl-assert (wsp-workspace-exists "workspace1")
	     "expected workspace1 to exist")
  (cl-assert (file-exists-p (wsp-test-workspace-state-file "workspace1" "projectile-bookmarks.eld"))
	     "workspace1: no projectile-bookmarks.eld at expected path")
  (cl-assert (file-exists-p (wsp-test-workspace-state-file "workspace1" "treemacs-persist"))
	     "workspace1: no treemacs-persist at expected path")

  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-workspace-list) '("workspace1"))
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1"))
	     "expected workspace1 to contain proj1")

  ;; verify treemacs visible
  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  ;; verify treemacs projects
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj1"))
	     "expected treemacs to display proj1")
  ;; verify projectile-known-projects
  (let ((proj1-dir (abbreviate-file-name (wsp-test-project-dir "proj1"))))
    (cl-assert (equal projectile-known-projects (list proj1-dir))
	       "expected projectile to be aware of proj1"))


  ;;
  ;; prepare for restore test by
  ;; - opening proj1 buffers
  ;; - adding another project to workspace
  ;; - creating workspace2
  ;;
  (find-file (wsp-test-project-file "proj1" "1a.txt"))
  (cl-assert (string= (wsp-project-current) "proj1")
	     "expected proj1 to be current project")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt"))
	     "expected proj1 buffer 1a.txt to be open")

  ;; create workspace2 and add a project
  (wsp-workspace-open "workspace2" (wsp-test-project-dir "proj2"))

  (cl-assert (wsp-workspace-exists "workspace2")
	     "expected workspace2 to exist")
  (cl-assert (equal (wsp-workspace-list) '("workspace1" "workspace2"))
	     "expected workspace1 to be current workspace")

  ;; verify workspace switch
  (cl-assert (equal (wsp-project-list) '("proj2"))
	     "expected workspace2 to contain proj2")
  (cl-assert (wsp-test-buffers-not-open-p '("1a.txt"))
	     "expected workspace1 buffers to be closed on workspace switch")
  (cl-assert (string= (wsp-workspace-current) "workspace2")
	     "expected workspace1 to be current workspace")

  ;; assert workspace2 state file creation
  (cl-assert (file-exists-p (wsp-test-workspace-state-file "workspace2" "projectile-bookmarks.eld"))
	     "workspace2: no projectile-bookmarks.eld at expected path")
  (cl-assert (file-exists-p (wsp-test-workspace-state-file "workspace2" "treemacs-persist"))
	     "workspace2: no treemacs-persist at expected path")

  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj2"))
	     "expected treemacs to display proj2")
  (let ((proj2-dir (abbreviate-file-name (wsp-test-project-dir "proj2"))))
    (cl-assert (equal projectile-known-projects (list proj2-dir))
	       "expected projectile to be aware of proj2"))


  ;; open workspace2 file 2b.txt

  (find-file (wsp-test-project-file "proj2" "2b.txt"))
  (cl-assert (string= (wsp-project-current) "proj2")
	     "expected proj2 to be current project")
  (cl-assert (wsp-test-buffers-open-p '("2b.txt"))
	     "expected proj2 buffer 2b.txt to be open")

  (redisplay)
  (wsp-test-end))


(defun wsp-test-workspace-restore1 ()
  "Verify that first-time created workspaces are properly loaded."
  (wsp-test-init)

  ;;
  ;; check preconditions: nothing should be loaded at this point
  ;;
  (cl-assert (not (wsp-workspace-current))
	     "expected `(wsp-workspace-current)` to be nil")
  ;; make sure the saved workspaces are detected
  (cl-assert (equal (wsp-workspace-list) '("workspace1" "workspace2"))
	     "expected workspace1 and workspace2 to exist")

  (cl-assert (equal (treemacs-current-visibility) 'none)
	     "expected treemacs to be invisible")
  (cl-assert (equal projectile-known-projects nil)
	     "expected projectile to be empty")


  ;;
  ;; restore workspace1
  ;;
  (wsp-workspace-open "workspace1")
  (redisplay)

  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1"))
	     "expected workspace1 to contain proj1")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt"))
	     "expected proj1 buffer 1a.txt to be open")
  ;; should not have opened workspace2 buffers
  (cl-assert (wsp-test-buffers-not-open-p '("2b.txt"))
	     "expected proj2 buffer 2b.txt to NOT be open")
  ;; last open workspace buffer should be restored
  (cl-assert (string= (buffer-name (window-buffer)) "1a.txt"))
  ;; verify treemacs/projectile restored
  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj1"))
	     "expected treemacs to display proj1")
  (let ((proj1-dir (abbreviate-file-name (wsp-test-project-dir "proj1"))))
    (cl-assert (equal projectile-known-projects (list proj1-dir))
	       "expected projectile to be aware of proj1"))

  ;;
  ;; open one more file from proj1 to ensure that saving after loading a
  ;; workspace works (this was bug-prone earlier)
  ;;
  (find-file (wsp-test-project-file "proj1" "1b.txt"))
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt"))
	     "expected proj1 buffers 1a.txt, 1b.txt to be open")

  ;; also add another project (proj3) to workspace1
  (wsp-project-add (wsp-test-project-dir "proj3"))
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1 and proj3")
  ;; open project file
  (find-file (wsp-test-project-file "proj3" "3a.txt"))
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")

  ;;
  ;; restore workspace2
  ;;
  (wsp-workspace-open "workspace2")
  (cl-assert (string= (wsp-workspace-current) "workspace2")
	     "expected workspace2 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj2"))
	     "expected workspace2 to contain proj2")
  (cl-assert (wsp-test-buffers-open-p '("2b.txt"))
	     "expected proj2 buffer 2b.txt to be open")
  ;; should have closed workspace1 buffers
  (cl-assert (wsp-test-buffers-not-open-p '("1a.txt" "3a.txt"))
	     "expected proj1 buffers 1a.txt, 3a.txt to NOT be open")
  ;; last open workspace buffer should be restored
  (cl-assert (string= (buffer-name (window-buffer)) "2b.txt"))
  ;; verify treemacs/projectile restored
  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj2"))
	     "expected treemacs to display proj2")
  (let ((proj2-dir (abbreviate-file-name (wsp-test-project-dir "proj2"))))
    (cl-assert (equal projectile-known-projects (list proj2-dir))
	       "expected projectile to be aware of proj2"))

  (redisplay)
  (wsp-test-end))


(defun wsp-test-workspace-restore2 ()
  "Verify that workspace changes after a restore were properly
saved and can be restored."
  (wsp-test-init)

  ;;
  ;; check preconditions: nothing should be loaded at this point
  ;;
  (cl-assert (not (wsp-workspace-current))
	     "expected `(wsp-workspace-current)` to be nil")
  ;; make sure the saved workspaces are detected
  (cl-assert (equal (wsp-workspace-list) '("workspace1" "workspace2"))
	     "expected workspace1 and workspace2 to exist")

  ;;
  ;; restore workspace1
  ;;
  (wsp-workspace-open "workspace1")
  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1, proj3")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")
  ;; focus should be on last open buffer
  (cl-assert (string= (buffer-name (window-buffer)) "3a.txt"))
  ;; verify treemacs/projectile restored
  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj1" "proj3"))
	     "expected treemacs to display proj1 and proj3")
  (let ((proj1-dir (abbreviate-file-name (wsp-test-project-dir "proj1")))
	(proj3-dir (abbreviate-file-name (wsp-test-project-dir "proj3"))))
    (cl-assert (equal projectile-known-projects (list proj3-dir proj1-dir ))
	       "expected projectile to be aware of proj3 and proj1"))
  (redisplay)
  (wsp-test-end))


(defun wsp-test-workspace-close ()
  ""
  (wsp-test-init)
  ;;
  ;; preconditions
  ;;
  (cl-assert (not (wsp-workspace-current))
	     "expected `(wsp-workspace-current)` to be nil")
  ;; make sure the saved workspaces are detected
  (cl-assert (equal (wsp-workspace-list) '("workspace1" "workspace2"))
	     "expected workspace1 and workspace2 to exist")
  ;;
  ;; load a workspace
  ;;
  (wsp-workspace-open "workspace1")
  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1, proj3")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")
  ;; verify treemacs/projectile restored
  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj1" "proj3"))
	     "expected treemacs to display proj1 and proj3")
  (let ((proj1-dir (abbreviate-file-name (wsp-test-project-dir "proj1")))
	(proj3-dir (abbreviate-file-name (wsp-test-project-dir "proj3"))))
    (cl-assert (equal projectile-known-projects (list proj3-dir proj1-dir ))
	       "expected projectile to be aware of proj3 and proj1"))
  ;;
  ;; close workspace: should close all open workspace buffers
  ;;
  (wsp-workspace-close)
  (cl-assert (not (wsp-workspace-current))
	     "expected no current workspace to be set after close")
  (cl-assert (wsp-test-buffers-not-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be closed after workspace close")

  ;; verify treemacs/projectile restored
  (cl-assert (equal (treemacs-current-visibility) 'none)
	     "expected treemacs to be invisible")
  (cl-assert (equal (wsp-treemacs-open-project-names) nil)
	     "expected treemacs to display no projects")
  (cl-assert (equal projectile-mode nil)
	     "expected projectile to be deactivated")
  (redisplay)
  (wsp-test-end))


(defun wsp-test-workspace-switch ()
  "Switching workspace should restore the new workspace buffers
and close all buffers in the current workspace."
  (wsp-test-init)
  ;;
  ;; load a workspace
  ;;
  (wsp-workspace-open "workspace1")
  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1, proj3")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")
  ;; verify treemacs/projectile restored
  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj1" "proj3"))
	     "expected treemacs to display proj1 and proj3")
  (let ((proj1-dir (abbreviate-file-name (wsp-test-project-dir "proj1")))
	(proj3-dir (abbreviate-file-name (wsp-test-project-dir "proj3"))))
    (cl-assert (equal projectile-known-projects (list proj3-dir proj1-dir ))
	       "expected projectile to be aware of proj3 and proj1"))

  ;; switch (open another), should close all workspace1 buffers
  (wsp-workspace-open "workspace2")
  (cl-assert (string= (wsp-workspace-current) "workspace2")
	     "expected workspace2 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj2"))
	     "expected workspace2 to contain proj2")
  (cl-assert (wsp-test-buffers-open-p '("2b.txt"))
	     "expected buffers proj2/2b.txt to be open")
  (cl-assert (wsp-test-buffers-not-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected workspace1 buffers to be closed after switch")
  ;; focus should be on last open workspace buffer
  (cl-assert (string= (buffer-name (window-buffer)) "2b.txt"))
  ;; verify treemacs/projectile restored
  (cl-assert (equal (treemacs-current-visibility) 'visible)
	     "expected treemacs to be visible")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj2"))
	     "expected treemacs to display proj2")
  (let ((proj2-dir (abbreviate-file-name (wsp-test-project-dir "proj2"))))
    (cl-assert (equal projectile-known-projects (list proj2-dir))
	       "expected projectile to be aware of proj2"))
  (redisplay)
  (wsp-test-end))


(defun wsp-test-project-switch ()
  "Switching projects should set the current buffer to one in the
switched-to project."
  (wsp-test-init)
  ;;
  ;; load a workspace
  ;;
  (wsp-workspace-open "workspace1")
  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1, proj3")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")
  (cl-assert (string= (buffer-name (window-buffer)) "3a.txt")
	     "expected current buffer to be 3a.txt")
  (cl-assert (string= (wsp-project-current) "proj3")
	     "expected current workspace1 project to be proj3")

  ;; switch project
  (wsp-project-switch "proj1")
  (cl-assert (string= (wsp-project-current) "proj1")
	     "expected current workspace1 project to be proj1 after switch")
  (cl-assert (string= (buffer-name (window-buffer)) "1a.txt")
	     "expected current buffer to be 1a.txt")
  ;; should not have closed any buffers
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")
  (redisplay)
  (wsp-test-end))


(defun wsp-test-project-close ()
  "Closing a project should close all open project buffers."
  (wsp-test-init)
  ;;
  ;; load a workspace
  ;;
  (wsp-workspace-open "workspace1")
  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1, proj3")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")
  (cl-assert (string= (buffer-name (window-buffer)) "3a.txt")
	     "expected current buffer to be 3a.txt")
  (cl-assert (string= (wsp-project-current) "proj3")
	     "expected current workspace1 project to be proj3")

  ;; close the current project (proj3) and all its buffers
  (wsp-project-close-current)
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1, proj3")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt"))
	     "expected only proj1 buffers to be open")
  (cl-assert (string= (buffer-name (window-buffer)) "1b.txt")
	     "expected current buffer to be 1b.txt")
  (cl-assert (string= (wsp-project-current) "proj1")
	     "expected current workspace1 project to be proj1")
  (redisplay)
  (wsp-test-end))

;; TODO: defun wsp-test-project-close-other

(defun wsp-test-workspace-delete ()
  ""
  (wsp-test-init)
  ;; open workspace
  (wsp-workspace-open "workspace1")
  (cl-assert (string= (wsp-workspace-current) "workspace1")
	     "expected workspace1 to be current workspace")
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace1 to contain proj1, proj3")
  (cl-assert (wsp-test-buffers-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected buffers proj1/{1a.txt,1b.txt}, proj3/3a.txt to be open")
  (cl-assert (string= (buffer-name (window-buffer)) "3a.txt")
	     "expected current buffer to be 3a.txt")
  (cl-assert (string= (wsp-project-current) "proj3")
	     "expected current workspace1 project to be proj3")
  ;; delete the current workspace
  (wsp-workspace-delete "workspace1")
  ;; should close workspace and all projects
  (cl-assert (equal (wsp-workspace-current) nil)
	     "expected no workspace to be open")
  (cl-assert (equal (wsp-project-list) nil)
	     "expected empty project list")
  ;; should close all project buffers
  (cl-assert (wsp-test-buffers-not-open-p '("1a.txt" "1b.txt" "3a.txt"))
	     "expected workspace buffers to no longer be open")
  (cl-assert (string= (buffer-name (window-buffer)) "*scratch*")
	     "expected current buffer to be *scratch*")
  (redisplay)
  (wsp-test-end))


(defun wsp-test-new-workspace-with-multiple-projects ()
  "Tests an issue where treemacs did not persist additional projects added to a newly created workspace."
  (wsp-workspace-open "workspace3" (wsp-test-project-dir "proj1"))
  (wsp-project-add (wsp-test-project-dir "proj3"))
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace3 to contain proj1 and proj3")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj1" "proj3"))
	     "expected treemacs to display proj1 and proj3")
  (redisplay)
  (wsp-test-end))

(defun wsp-test-new-workspace-with-multiple-projects-restore ()
  "Tests an issue where treemacs did not persist additional projects added to a newly created workspace."
  (wsp-workspace-open "workspace3")
  (cl-assert (equal (wsp-project-list) '("proj1" "proj3"))
	     "expected workspace3 to contain proj1 and proj3")
  (cl-assert (equal (wsp-treemacs-open-project-names) '("proj1" "proj3"))
	     "expected treemacs to display proj1 and proj3")
  (redisplay)
  (wsp-test-end))



(provide 'wsp-test)
;;; wsp-test.el ends here
