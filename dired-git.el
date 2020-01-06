;;; dired-git.el --- Git integration for dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26"))
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

(defgroup dired-git nil
  "Git integration for dired."
  :prefix "dired-git-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/dired-git.el"))

(provide 'dired-git)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dired-git.el ends here
