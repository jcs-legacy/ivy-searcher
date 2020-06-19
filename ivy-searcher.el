;;; ivy-searcher.el --- Ivy interface to use searcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-06-19 22:30:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Ivy interface to use searcher.
;; Keyword: ivy interface use searcher search
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (ivy "0.8.0") (s "1.12.0"))
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
(require 's)

(defgroup ivy-searcher nil
  "Ivy interface to use searcher."
  :prefix "ivy-searcher-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-searcher"))

(defconst ivy-searcher--candidate-format "%s:%s:%s"
  "Format to display candidate.")

(defconst ivy-searcher--prompt "searcher: "
  "Prompt string when using `ivy-searcher'.")

(defvar ivy-searcher--target-buffer nil
  "Record down the current target buffer.")

(defun ivy-searcher--do-complete-action (cand)
  "Do action with CAND."
  (let* ((str-lst (split-string cand ":"))
         (file (nth 0 str-lst))
         (pt (string-to-number (nth 1 str-lst))))
    (if (file-exists-p file) (find-file file) (switch-to-buffer file))
    (goto-char (1+ pt))))

(defun ivy-searcher--do-search-action (cands dir)
  "Do the search action by CANDS and DIR."
  (let ((candidates '())
        (file nil) (pos nil) (ln-str nil)
        (candidate ""))
    (dolist (item cands)
      (setq file (plist-get item :file))
      (setq file (s-replace dir "" file))
      (setq pos (plist-get item :position))
      (setq ln-str (plist-get item :string))
      (setq candidate (format ivy-searcher--candidate-format file pos ln-str))
      (push candidate candidates))
    candidates))

(defun ivy-searcher--do-project (str)
  "Search for STR in project."
  (let ((project-dir (cdr (project-current)))
        (cands (searcher-search-in-project str)))
    (ivy-searcher--do-search-action cands project-dir)))

(defun ivy-searcher--do-file (str)
  "Search for STR in file."
  (let ((dir (concat (f-dirname ivy-searcher--target-buffer) "/"))
        (cands (searcher-search-in-file ivy-searcher--target-buffer str)))
    (ivy-searcher--do-search-action cands dir)))

;;;###autoload
(defun ivy-searcher-project ()
  "Search through the project."
  (interactive)
  (ivy-read ivy-searcher--prompt
            #'ivy-searcher--do-project
            :dynamic-collection t
            :require-match t
            :action #'ivy-searcher--do-complete-action))

;;;###autoload
(defun ivy-searcher-file ()
  "Search through current file."
  (interactive)
  (let ((ivy-searcher--target-buffer (or (buffer-file-name) (buffer-name))))
    (ivy-read ivy-searcher--prompt
              #'ivy-searcher--do-file
              :dynamic-collection t
              :require-match t
              :action #'ivy-searcher--do-complete-action)))

(provide 'ivy-searcher)
;;; ivy-searcher.el ends here
