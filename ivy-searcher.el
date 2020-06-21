;;; ivy-searcher.el --- Ivy interface to use searcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-06-19 22:30:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Ivy interface to use searcher.
;; Keyword: ivy interface use searcher search
;; Version: 0.1.2
;; Package-Requires: ((emacs "25.1") (ivy "0.8.0") (searcher "0.1.2") (s "1.12.0") (f "0.20.0"))
;; URL: https://github.com/jcs-elpa/ivy-searcher

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

(require 'cl-lib)
(require 'ivy)
(require 'searcher)
(require 's)

(defgroup ivy-searcher nil
  "Ivy interface to use searcher."
  :prefix "ivy-searcher-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-searcher"))

(defcustom ivy-searcher-display-info 'position
  "Display option for file information."
  :type '(choice (const :tag "position" position)
                 (const :tag "line/column" line/column))
  :group 'ivy-searcher)

(defcustom ivy-searcher-separator ":"
  "Separator string for display."
  :type 'string
  :group 'ivy-searcher)

(defconst ivy-searcher--prompt "searcher: "
  "Prompt string when using `ivy-searcher'.")

(defvar ivy-searcher--target-buffer nil
  "Record down the current target buffer.")

(defun ivy-searcher--separator-string ()
  "Return the separator string with text properties."
  (propertize ivy-searcher-separator 'face 'default))

(defun ivy-searcher--propertize-line-string (ln-str input col)
  "Propertize the LN-STR with INPUT and column (COL)."
  (let ((key-pt (1+ col)) (str-len (length ln-str)))
    (when (> key-pt str-len) (setq key-pt str-len))
    (concat
     (substring ln-str 0 key-pt)
     (propertize input 'face 'ivy-highlight-face)
     (substring ln-str (+ key-pt (length input)) str-len))))

(defun ivy-searcher--do-complete-action (cand)
  "Do action with CAND."
  (let* ((str-lst (split-string cand ivy-searcher-separator))
         (file (nth 0 str-lst))
         (pt (string-to-number (nth 1 str-lst))))
    (if (file-exists-p file) (find-file file) (switch-to-buffer file))
    (goto-char (1+ pt))))

(defun ivy-searcher--do-search-action (input cands dir)
  "Do the search action by INPUT, CANDS and DIR."
  (let ((candidates '())
        (file nil) (ln-str nil) (pos nil) (ln nil) (col nil)
        (candidate ""))
    (dolist (item cands)
      (setq file (plist-get item :file)) (setq file (s-replace dir "" file))
      (progn  ; Resolve line string.
        (setq ln-str (plist-get item :string))
        (setq col (plist-get item :column))
        (setq ln-str (ivy-searcher--propertize-line-string ln-str input col)))
      (progn  ; Resolve information.
        (setq pos (plist-get item :position)) (setq pos (number-to-string pos))
        (setq ln (plist-get item :line-number)) (setq ln (number-to-string ln))
        (setq col (number-to-string col)))
      (setq candidate
            (cl-case ivy-searcher-display-info
              ('position
               (concat (propertize file 'face 'ivy-grep-info)
                       (ivy-searcher--separator-string)
                       (propertize pos 'face 'ivy-grep-line-number)
                       (ivy-searcher--separator-string)
                       ln-str))
              ('line/column
               (concat (propertize file 'face 'ivy-grep-info)
                       (ivy-searcher--separator-string)
                       (propertize ln 'face 'ivy-grep-line-number)
                       (ivy-searcher--separator-string)
                       (propertize col 'face 'ivy-grep-line-number)
                       (ivy-searcher--separator-string)
                       ln-str))))
      (push candidate candidates))
    candidates))

(defun ivy-searcher--do-project (str)
  "Search for STR in project."
  (let ((project-dir (cdr (project-current)))
        (cands (searcher-search-in-project str)))
    (ivy-searcher--do-search-action str cands project-dir)))

(defun ivy-searcher--do-file (str)
  "Search for STR in file."
  (let ((dir (concat (f-dirname ivy-searcher--target-buffer) "/"))
        (cands (searcher-search-in-file ivy-searcher--target-buffer str)))
    (ivy-searcher--do-search-action str cands dir)))

;;;###autoload
(defun ivy-searcher-project ()
  "Search through the project."
  (interactive)
  (ivy-read ivy-searcher--prompt
            #'ivy-searcher--do-project
            :dynamic-collection t
            :action #'ivy-searcher--do-complete-action))

;;;###autoload
(defun ivy-searcher-file ()
  "Search through current file."
  (interactive)
  (let ((ivy-searcher--target-buffer (or (buffer-file-name) (buffer-name))))
    (ivy-read ivy-searcher--prompt
              #'ivy-searcher--do-file
              :dynamic-collection t
              :action #'ivy-searcher--do-complete-action)))

(provide 'ivy-searcher)
;;; ivy-searcher.el ends here
