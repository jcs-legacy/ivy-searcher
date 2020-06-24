;;; ivy-searcher.el --- Ivy interface to use searcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-06-19 22:30:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Ivy interface to use searcher.
;; Keyword: ivy interface searcher search replace grep ag rg
;; Version: 0.2.3
;; Package-Requires: ((emacs "25.1") (ivy "0.8.0") (searcher "0.1.4") (s "1.12.0") (f "0.20.0"))
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

(defcustom ivy-searcher-default-input ""
  "Default initial input for searcher."
  :type 'string
  :group 'ivy-searcher)

(defconst ivy-searcher--prompt-format "[Searcher] %s: "
  "Prompt string when using `ivy-searcher'.")

(defvar ivy-searcher--initial-input nil
  "Current initial input for searcher.")

(defvar ivy-searcher--target-buffer nil
  "Record down the current target buffer.")

(defvar ivy-searcher--search-string ""
  "Record down the current search string.")

(defvar ivy-searcher--replace-string ""
  "Record down the current replace string.")

(defvar ivy-searcher--replace-candidates '()
  "Record down all the candidates for searching.")

(defun ivy-searcher--is-contain-list-string (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun ivy-searcher--goto-line (ln)
  "Goto LN line number."
  (goto-char (point-min))
  (forward-line (1- ln)))

(defun ivy-searcher--get-string-from-file (path)
  "Return PATH file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun ivy-searcher--separator-string ()
  "Return the separator string with text properties."
  (propertize ivy-searcher-separator 'face 'default))

(defun ivy-searcher--propertize-line-string (ln-str input col)
  "Propertize the LN-STR with INPUT and column (COL)."
  (let ((key-pt (1+ col)))
    (concat
     (substring ln-str 0 key-pt)
     (propertize input 'face 'ivy-highlight-face)
     (substring ln-str (+ key-pt (length input)) (length ln-str)))))

(defun ivy-searcher--candidate-to-plist (cand)
  "Convert CAND string to a plist data."
  (let* ((data (split-string cand ivy-searcher-separator))
         (file (nth 0 data)) (ln-str nil)
         (pos nil) (ln nil) (col nil) )
    (cl-case ivy-searcher-display-info
      ('position
       (setq pos (nth 1 data))
       (setq ln-str (nth 2 data)))
      ('line/column
       (setq ln (nth 1 data))
       (setq col (nth 2 data))
       (setq ln-str (nth 3 data))))
    (list :file file :string ln-str :position pos :line-number ln :column col)))

(defun ivy-searcher--initial-input-or-region ()
  "Return the default initiali input depend if region is active or not."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ivy-searcher-default-input))

;;; Search

(defun ivy-searcher--do-search-complete-action (cand)
  "Do action with CAND."
  (let* ((data (ivy-searcher--candidate-to-plist cand))
         (file (plist-get data :file))
         (pos (plist-get data :position))
         (ln (plist-get data :line-number))
         (col (plist-get data :column)))
    (if (file-exists-p file) (find-file file) (switch-to-buffer file))
    (cl-case ivy-searcher-display-info
      ('position
       (setq pos (string-to-number pos))
       (goto-char (1+ pos)))
      ('line/column
       (setq ln (string-to-number ln))
       (setq col (string-to-number col))
       (ivy-searcher--goto-line ln)
       (move-to-column (1+ col))))))

(defun ivy-searcher--do-search-input-action (input cands dir)
  "Do the search action by INPUT, CANDS and DIR."
  (let ((candidates '())
        (file nil) (ln-str nil) (pos nil) (ln nil) (col nil)
        (candidate ""))
    (setq ivy-searcher--replace-candidates '())  ; Clean up.
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
      (push candidate candidates)
      ;; Record down all the candidates.
      (push (cons candidate item) ivy-searcher--replace-candidates))
    candidates))

(defun ivy-searcher--do-search-project (input)
  "Search for INPUT in project."
  (let ((project-dir (cdr (project-current)))
        (cands (searcher-search-in-project input)))
    (setq ivy-searcher--search-string input)
    (ivy-searcher--do-search-input-action input cands project-dir)))

(defun ivy-searcher--do-search-file (input)
  "Search for INPUT in file."
  (let ((dir (concat (f-dirname ivy-searcher--target-buffer) "/"))
        (cands (searcher-search-in-file ivy-searcher--target-buffer input)))
    (setq ivy-searcher--search-string input)
    (ivy-searcher--do-search-input-action input cands dir)))

;;;###autoload
(defun ivy-searcher-search-project ()
  "Search through the project."
  (interactive)
  (let ((ivy-searcher--initial-input (ivy-searcher--initial-input-or-region)))
    (ivy-read (format ivy-searcher--prompt-format "Search")
              #'ivy-searcher--do-search-project
              :initial-input ivy-searcher--initial-input
              :dynamic-collection t
              :require-match t
              :action #'ivy-searcher--do-search-complete-action)))

;;;###autoload
(defun ivy-searcher-search-file ()
  "Search through current file."
  (interactive)
  (let ((ivy-searcher--initial-input (ivy-searcher--initial-input-or-region))
        (ivy-searcher--target-buffer (or (buffer-file-name) (buffer-name))))
    (ivy-read (format ivy-searcher--prompt-format "Search")
              #'ivy-searcher--do-search-file
              :initial-input ivy-searcher--initial-input
              :dynamic-collection t
              :require-match t
              :action #'ivy-searcher--do-search-complete-action)))

;;; Replace

(defun ivy-searcher--do-replace-complete-action (_cand)
  "Replace all recorded candidates."
  (let ((output-files '()))
    (dolist (cand ivy-searcher--replace-candidates)
      (let* ((cand-plist (cdr cand))
             (file (plist-get cand-plist :file))
             (new-content nil))
        (unless (ivy-searcher--is-contain-list-string output-files file)
          (push file output-files)
          (setq new-content (s-replace-regexp ivy-searcher--search-string
                                              ivy-searcher--replace-string
                                              (ivy-searcher--get-string-from-file file)))
          (write-region new-content nil file))))))

(defun ivy-searcher--do-replace (input)
  "Update the candidates with INPUT in ivy so the user can look at it."
  (setq ivy-searcher--replace-string input)
  (let ((candidates '()))
    (dolist (cand ivy-searcher--replace-candidates)
      (let* ((cand-str (car cand)) (cand-plist (cdr cand))
             (ln-str (plist-get cand-plist :string)))
        (setq cand
              (concat
               (substring cand-str 0 (- (length cand-str) (length ln-str)))
               (s-replace-regexp ivy-searcher--search-string input ln-str)))
        (push cand candidates)))
    (reverse candidates)))

(defun ivy-searcher--do-replace-matched-action (_cand)
  "Get the new string input and replace all candidates."
  (ivy-read (format ivy-searcher--prompt-format
                    (format "Replace %s with" ivy-searcher--search-string))
            #'ivy-searcher--do-replace
            :dynamic-collection t
            :require-match t
            :action #'ivy-searcher--do-replace-complete-action))

;;;###autoload
(defun ivy-searcher-replace-project ()
  "Search and replace string in project."
  (interactive)
  (let ((ivy-searcher--initial-input (ivy-searcher--initial-input-or-region)))
    (ivy-read (format ivy-searcher--prompt-format "Replace")
              #'ivy-searcher--do-search-project
              :initial-input ivy-searcher--initial-input
              :dynamic-collection t
              :require-match t
              :action #'ivy-searcher--do-replace-matched-action)))

;;;###autoload
(defun ivy-searcher-replace-file ()
  "Search and replace string in file."
  (interactive)
  (let ((ivy-searcher--initial-input (ivy-searcher--initial-input-or-region))
        (ivy-searcher--target-buffer (or (buffer-file-name) (buffer-name))))
    (ivy-read (format ivy-searcher--prompt-format "Replace")
              #'ivy-searcher--do-search-file
              :initial-input ivy-searcher--initial-input
              :dynamic-collection t
              :require-match t
              :action #'ivy-searcher--do-replace-matched-action)))

(provide 'ivy-searcher)
;;; ivy-searcher.el ends here
