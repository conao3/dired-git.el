;;; dired-git.el --- Git integration for dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26.1") (async-await "1.0") (async "1.9.4") (all-the-icons "2.2.0"))
;; URL: https://github.com/conao3/dired-git.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Git integration for dired.

;; To use this package, simply add this to your init.el:
;;   (add-hook 'dired-mode-hook 'dired-git-mode)


;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'dired)
(require 'async-await)
(require 'all-the-icons)

(defgroup dired-git nil
  "Git integration for dired."
  :prefix "dired-git-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/dired-git.el"))

(defvar dired-git-mode)
(defvar dired-git-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap of interactive commands.")

(defface dired-git-branch-master
  '((t (:foreground "SpringGreen" :weight bold)))
  "Face of showing branch master.")

(defface dired-git-branch-else
  '((t (:foreground "cyan" :weight bold)))
  "Face of showing branch else.")


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

(defvar-local dired-git-working nil
  "If non-nil, now working dired-git process.")

(defvar-local dired-git-hashtable nil
  "Hashtable stored git information.
Key is file absolute path, value is alist of information.")

(defun dired-git--create-overlay-string (file width table)
  "Create overlay string from data for FILE from TABLE.
WIDTH stored maxlength to align column."
  (let ((w-branch  (alist-get :branch width))
        (w-remote  (alist-get :remote width))
        (w-ff      2)
        (w-forward 2))
    (if-let ((data (gethash file table)))
        (let ((branch  (alist-get :branch data))
              (_remote (alist-get :remote data))
              (ff      (alist-get :ff data))
              (forward (alist-get :forward data)))
          (concat
           (format "%s " (all-the-icons-octicon "git-branch"))
           (format (format "%%-%ds\t" w-branch)
                   (propertize branch 'face
                               (if (string= "master" branch)
                                   'dired-git-branch-master
                                 'dired-git-branch-else)))
           (format "%s\t"
                   (cond
                    ((string= "true" ff)
                     (all-the-icons-octicon "rocket"))
                    ((string= "false" ff)
                     (all-the-icons-octicon "x"))
                    ((string= "missing" ff)
                     (all-the-icons-octicon "stop" :v-adjust -0.2))))
           (format "%s\t"
                   (cond
                    ((string= "missing" ff)
                     (all-the-icons-octicon "stop" :v-adjust -0.2))
                    (t
                     (all-the-icons-octicon "diff-added"))))
           (format "%s\t"
                   (if (string= "missing" forward) "?" forward))))
      (format (format "%%%ds %%%ds %%%ds" (+ 2 w-branch) w-ff w-forward)
              "" "" "" ""))))

(defun dired-git--promise-git-info (buf)
  "Return promise to get branch name for dired BUF."
  (promise-then
   (let ((default-directory (with-current-buffer buf
                              dired-directory)))
     (promise:make-process
      shell-file-name
      shell-command-switch
      "find . -mindepth 1 -maxdepth 1 -type d | sort | tr \\\\n \\\\0 | \
xargs -0 -I^ sh -c \"
cd ^
git rev-parse --is-inside-work-dir >/dev/null 2>&1 || exit 0
if [ \\\"\\$PWD\\\" != \\\"\\$(git rev-parse --show-toplevel)\\\" ]; then exit 0; fi
branch=\\\"\\$(git symbolic-ref --short HEAD)\\\"
remote=\\\"\\$(git config --get branch.\\${branch}.remote)\\\"

git rev-parse \\${remote}/\\${branch} >/dev/null 2>&1
if [ 0 -ne \\$? ]; then
  ff=\\\"missing\\\"
  forward=\\\"missing\\\"
else
  ff=\\\"\\$(if [ 0 -eq \\$(git rev-list --count \\${remote}/\\${branch}..\\${branch}) ]; then
    echo true
  else
    echo false
  fi)\\\"
  forward=\\\"\\$(git log \\${remote}/\\${branch}..\\${branch} --oneline | wc -l)\\\"
fi

echo \\\"(\
 :file \\\\\\\"\\$PWD\\\\\\\"\
 :branch \\\\\\\"\\${branch}\\\\\\\"\
 :remote \\\\\\\"\\${remote}\\\\\\\"\
 :ff \\\\\\\"\\${ff}\\\\\\\"\
 :forward \\\\\\\"\\${forward}\\\\\\\"\
)\\\"
\"
"))
   (lambda (res)
     (seq-let (stdout stderr) res
       (if (not (string-empty-p stderr))
           (promise-reject `(fail-git-info-invalid-output ,stdout ,stderr))
         (promise-resolve stdout))))
   (lambda (reason)
     (promise-reject `(fail-git-info-command ,reason)))))

(defun dired-git--promise-create-hash-table (_buf stdout)
  "Return promise to create hash table from STDOUT.
STDOUT is return value form `dired-git--promise-git-info'."
  (promise-then
   (promise:async-start
    `(lambda ()
       (require 'subr-x)
       (let ((info (read (format "(%s)" ,stdout)))
             (table (make-hash-table :test 'equal))
             width-alist)
         (dolist (elm info)
           (puthash (plist-get elm :file)
                    `((:branch  . ,(plist-get elm :branch))
                      (:remote  . ,(plist-get elm :remote))
                      (:ff      . ,(plist-get elm :ff))
                      (:forward . ,(plist-get elm :forward)))
                    table)
           (dolist (key '(:branch :remote :ff :forward))
             (when-let ((width (string-width (plist-get elm key))))
               (when (< (or (alist-get key width-alist) 0) width)
                 (setf (alist-get key width-alist) width)))))
         (puthash "**dired-git/width**" width-alist table)
         (prin1-to-string table))))
   (lambda (res)
     (promise-resolve (read res)))
   (lambda (reason)
     (promise-reject `(fail-create-hash-table ,stdout ,reason)))))

(defun dired-git--promise-add-annotation (buf table)
  "Add git annotation for BUF.
TABLE is hash table returned value by `dired-git--promise-git-info'."
  (promise-new
   (lambda (resolve reject)
     (condition-case err
         (with-current-buffer buf
           (when-let* ((width (gethash "**dired-git/width**" table)))
             (save-restriction
               (widen)
               (save-excursion
                 (goto-char (point-min))
                 (while (not (eobp))
                   (when-let ((file (dired-get-filename nil 'noerror)))
                     (dired-git--add-overlay
                      (point)
                      (dired-git--create-overlay-string file width table)))
                   (dired-next-line 1))
                 (funcall resolve t)))))
       (error
        (funcall reject `(fail-add-annotation ,table ,err)))))))


;;; Main

(async-defun dired-git--refresh (&optional buf)
  "Refresh git overlays for BUF or `current-buffer'."
  (let ((buf* (or buf (current-buffer))))
    (condition-case err
        (unless dired-git-working
          (with-current-buffer buf*
            (setq-local tab-width 1)
            (setq-local dired-git-working t)
            (setq-local dired-git-hashtable nil)
            (dired-git--remove-all-overlays))
          (let* ((buf* (or buf (current-buffer)))
                 (res  (await (dired-git--promise-git-info buf*)))
                 (hash (await (dired-git--promise-create-hash-table buf* res)))
                 (res  (await (dired-git--promise-add-annotation buf* hash))))
            (with-current-buffer buf*
              (setq-local dired-git-working nil)
              (setq-local dired-git-hashtable hash))))
      (error
       (pcase err
         (`(error (fail-git-info-command ,reason))
          (warn "Fail invoke git command
  buffer: %s\n  reason:%s"
                (prin1-to-string buf*) reason))
         (`(error (fail-git-info-invalid-output ,stdout ,stderr))
          (warn "Fail invoke git command.  Include stderr output
  buffer: %s\n  stdout: %s\n  stderr: %s"
                (prin1-to-string buf*) stdout stderr))
         (`(error (fail-create-hash-table ,stdout ,reason))
          (warn "Fail create hash table
  buffer: %s\n  stdout: %s\n  reason: %s"
                (prin1-to-string buf*) stdout reason))
         (`(error (fail-add-annotation ,table ,reason))
          (warn "Fail add annotation
  buffer: %s\n  table: %s\n  reason: %s"
                (prin1-to-string buf*) table  reason))
         (_
          (warn "Fail dired-git--refresh
  buffer: %s\n  reason: %s"
                (prin1-to-string buf*) err)))))))

(defun dired-git--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (when dired-git-mode
    (dired-git--refresh)))

(defvar dired-git-advice-alist
  '((dired-readin . dired-git--refresh-advice)
    (dired-revert . dired-git--refresh-advice)
    (dired-internal-do-deletions . dired-git--refresh-advice)
    (dired-narrow--internal . dired-git--refresh-advice))
  "Alist defined advice functions.")

(defun dired-git--setup ()
  "Setup dired-git minor-mode."
  (pcase-dolist (`(,sym . ,fn) dired-git-advice-alist)
    (advice-add sym :around fn))
  (dired-git--refresh (current-buffer)))

(defun dired-git--teardown ()
  "Teardown all overlays added by dired-git."
  (setq-local dired-git-hashtable nil)
  (setq-local dired-git-working nil)
  (dired-git--remove-all-overlays))

;;;###autoload
(define-minor-mode dired-git-mode
  "Minor mode to add git information for dired."
  :keymap dired-git-mode-map
  :lighter " Dired-git"
  :group 'dired-git
  (if dired-git-mode
      (dired-git--setup)
    (dired-git--teardown)))

(provide 'dired-git)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dired-git.el ends here
