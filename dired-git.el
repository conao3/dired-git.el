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

`modify'
    If non-nil, work tree has some changes.

`ff'
    If non-nil, work tree can merge via fast-forward."
  path branch modifiy ff)


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

(defun dired-git--promise-git-info (dir)
  "Return promise to get branch name for DIR."
  (promise-then
   (let ((default-directory dir))
     (promise:make-process
      shell-file-name
      shell-command-switch
      "find . -mindepth 1 -maxdepth 1 -type d | sort | tr \\\\n \\\\0 | \
xargs -0 -I^ sh -c \"
cd ^
function gitinfo() {
if [ \\$PWD != \\$(git rev-parse --show-toplevel) ]; then exit 1; fi
branch=\\$(git symbolic-ref --short HEAD)
remote=\\$(git config --get branch.\\${branch}.remote)
ff=\\$(git rev-parse \\${remote}/\\${branch} >/dev/null 2>&1;
  if [ 0 -ne \\$? ]; then
    echo missing
  else
    if [ 0 -eq \\$(git rev-list --count \\${remote}/\\${branch}..\\${branch}) ]; then echo true; else echo false; fi
  fi
)
echo \\\"(\
 :file \\\\\\\"\\$PWD\\\\\\\"\
 :branch \\\\\\\"\\${branch}\\\\\\\"\
 :remote \\\\\\\"\\${remote}\\\\\\\"\
 :ff \\\\\\\"\\${ff}\\\\\\\"\
)\\\"
}
gitinfo\"
"))
   (lambda (res)
     (seq-let (stdin stderr) res
       (if (not (string-empty-p stderr))
           (promise-reject `(fail-git-info-invalid-output ,stdin ,stderr))
         (condition-case err
             (let ((info (read (format "(%s)" stdin))))
               (promise-resolve info))
           (error
            (promise-reject `(fail-git-info-read ,stdin ,err)))))))
   (lambda (reason)
     (promise-reject `(fail-git-info-command ,reason)))))

(defun dired-git--promise-add-annotation (buf info)
  "Add git annotation for BUF.
INFO is return value by `dired-git--promise-git-info'.")

(async-defun dired-git--add-status (&optional buf rootonly)
  "Add git status for BUF or `current-buffer'.
If ROOTONLY is non-nil, do nothing when DIR doesn't git root directory."
  (condition-case err
      (with-current-buffer (or buf (current-buffer))
        (let ((res (await (dired-git--promise-git-info dired-directory))))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (dired-git--add-overlay)
              (dired-next-line 1)))))
    (error
     (pcase err
       (`(error (fail-git-command ,reason))
        (warn "Fail invoke git command
  buffer: %s\n  rootonly: %s\n  reason:%s"
              (prin1-to-string buf) rootonly reason))
       (`(error (fail-git-info-read ,stdin ,orig-err))
        (warn "Fail read git output
  buffer: %s\n  rootonly: %s\n  stdin: %s\n  orig-err: %s"
              (prin1-to-string buf) rootonly stdin orig-err))
       (`(error (fail-git-info-invalid-output ,stdin ,stderr))
        (warn "Fail invoke git command.  Include stderr output
  buffer: %s\n  rootonly: %s\n  stdin: %s\n  stderr: %s"
              (prin1-to-string buf) rootonly stdin stderr))
       (_
        (warn "Fail dired-git--promise-add-annotation
  buffer: %s\n  rootonly: %s\n"
              (prin1-to-string buf) rootonly))))))


;;; Main

;;;###autoload
(defun dired-git-setup (&optional buf)
  "Setup dired-git for BUF or `current-buffer'."
  (interactive)
  (let ((buf* (or buf (current-buffer))))
    (dired-git--add-status buf*)))

(provide 'dired-git)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dired-git.el ends here
