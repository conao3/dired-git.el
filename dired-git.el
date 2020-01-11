;;; dired-git.el --- Git integration for dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26.1") (async-await "1.0"))
;; URL: https://github.com/conao3/dired-git.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Git integration for dired.


;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'dired)
(require 'async-await)

(defgroup dired-git nil
  "Git integration for dired."
  :prefix "dired-git-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/dired-git.el"))

(defface dired-git-branch-master
  '((t (:foreground "green" :weight bold)))
  "Face of showing branch master.")

(defface dired-git-branch-else
  '((t (:foreground "cyan" :weight bold)))
  "Face of showing branch else.")

(cl-defstruct (dired-git-status
               (:constructor nil)
               (:constructor dired-git-status-new)
               (:copier nil))
  "Structure reporesenting an git status object.
Slots:

`path'
    Path of directory or file as string.

`branch'
    Git branch as string

`modified'
    If non-nil, work tree has some changes.

`merge-ff'
    If non-nil, work tree can merge via fast-forward."
  path branch modified merge-ff)


;;; Manage Overlays

(defun dired-git--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'dired-git-overlay t)
    (overlay-put ov 'after-string string)))

(defun dired-git--overlays-in (beg end)
  "Get all dired-git overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'dired-git-overlay))
   (overlays-in beg end)))

(defun dired-git--overlays-at (pos)
  "Get dired-git overlays at POS."
  (apply #'dired-git--overlays-in `(,pos ,pos)))

(defun dired-git--remove-all-overlays ()
  "Remove all `dired-git' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay (dired-git--overlays-in (point-min) (point-max)))))


;;; Function

(defun dired-git--promise-get-branch (dir)
  "Return promise to get branch name for DIR."
  (promise-then
   (promise:make-process
    shell-file-name
    shell-command-switch
    (format "cd %s; git rev-parse --abbrev-ref HEAD" dir))
   (lambda (res)
     (seq-let (stdin _stderr) res
       (promise-resolve (string-trim stdin))))
   (lambda (reason)
     (promise-reject `(fail-get-branch ,reason)))))

(defun dired-git--promise-has-modified (dir)
  "Return promise to get work tree modified for DIR."
  (promise-then
   (promise:make-process
    shell-file-name
    shell-command-switch
    (format "cd %s; git status --short" dir))
   (lambda (res)
     (seq-let (stdin _stderr) res
       (promise-resolve (not (string-empty-p (string-trim stdin))))))
   (lambda (reason)
     (promise-reject `(fail-has-modified ,reason)))))

(async-defun dired-git--add-git-annotation (&optional rootonly)
  "Add git annotation for current-point in dired buffer.
If ROOTONLY is non-nil, do nothing when DIR doesn't git root directory."
  (let (status)
    (when-let* ((path (dired-get-filename nil t))
                (git-dir
                 (if rootonly
                     (let ((path (expand-file-name ".git" path)))
                       (when (file-directory-p path)
                         (shell-quote-argument path)))
                   (shell-quote-argument
                    (expand-file-name
                     (locate-dominating-file path ".git"))))))
      (when (string-match-p "/\\.\\.?\\'" path)
        (setq status (await
                      (promise-all
                       (vector
                        (dired-git--promise-get-branch git-dir)))))
        (dired-git--add-overlay (pos) "  ")))))

(defun dired-git--add-status ()
  "Add git status for `current-buffer'."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (dired-git--add-git-annotation (point))
      (dired-next-line 1))))


;;; Main

;;;###autoload
(defun dired-git-setup (&optional buf)
  "Setup dired-git for BUF or `current-buffer'."
  (with-current-buffer (or buf (current-buffer))
    (save-restriction
      (widen)
      (dired-git--add-status))))

(provide 'dired-git)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dired-git.el ends here
