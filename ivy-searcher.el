;;; ivy-searcher.el --- Ivy interface to use searcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-06-19 22:30:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Ivy interface to use searcher.
;; Keyword: ivy interface use searcher search
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (ivy "0.8.0"))
;; URL: https://github.com/jcs090218/ivy-searcher

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Ivy interface to use searcher.
;;

;;; Code:

(require 'ivy)

(defgroup ivy-searcher nil
  "Ivy interface to use searcher."
  :prefix "ivy-searcher-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-searcher"))

(defconst ivy-searcher--candidate-format "%s:%s:%s"
  "Format to display candidate.")

(defun ivy-searcher-test ()
  (interactive)
  (let ((lst (searcher-search-in-project "searcher"))
        (candidates '())
        (file nil) (pos nil) (ln-str nil)
        (candidate ""))
    (message "%s" lst)
    (dolist (item lst)
      (message "item: %s" item)
      (setq file (plist-get item :file))
      (setq pos (plist-get item :position))
      (setq ln-str (plist-get item :string))
      (message "file: %s" file)
      (setq candidate (format "%s:%s:%s" file pos ln-str))
      (push candidate candidates))
    (ivy-read "searcher: " candidates)))

;;;###autoload
(defun ivy-searcher-project ()
  ""
  (interactive)
  (ivy-read "searcher: " lst
            :dynamic-collection t
            :action (lambda (cand)
                      (message "data: %s" cand)))
  )


(provide 'ivy-searcher)
;;; ivy-searcher.el ends here
