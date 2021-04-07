;;; xsv.el --- front end for xsv - A fast CSV command line toolkit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 emacsbliss
;;
;; Author: emacsbliss <http://github/emacsbliss>
;; Maintainer: emacsbliss <emacsbliss@gmail.com>
;; Version: 0.0.1
;; Keywords: csv xsv
;; Homepage: https://github.com/emacsbliss/xsv.el
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5") (s "1.12.0") (dash "2.18.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;  Front end for xsv, enhance the experience for working with csv in Emacs.
;;
;;; Code:

(require 's)
(require 'dash)

(defun xsv/split-filters (filters)
  "Split multiple criteria from `FILTERS' separated by `&'."
  (s-split "&" filters))

(defun xsv/parse-filter (filter)
  "Parse the given `FILTER' which specifies a criteria for xsv search into
internal format."
  (let* ((pattern "\\('?[^!]*'?\\) *\\(!?=\\) *\\('.*'\\)")
         (matching (s-match pattern filter)))

    (if matching
      (-drop 1 matching)
      (error "Wrong input, should be column-name ='regex' or column-name != 'regex'"))
))

(defun xsv/build-search-command (filter)
  "`FILTER' is a list of three elements in the order of column, operator, regex.
Return the xsv command for the given `FILTER'."
    (let ((column (nth 0 filter))
          (operator (nth 1 filter))
          (regex (nth 2 filter))
          (invert ""))

        (when (string= "!=" operator)
          (setq invert "-v"))

        (format "xsv search %s -s %s %s" invert column regex)))

(defun xsv/run (input cmd-builder)
  "Invoke xsv to run the `COMMAND' then collect output and show it in buffer."
  (let* ((csv-file (buffer-file-name))
         (content)
         (buf)
         (command)
         (result))

   ;; if cur-file is nil, ie we may visit a buffer which is not a file
   ;; then we need to save buffer content to a temp file first
   (unless csv-file
     (setq content (buffer-substring (point-min) (point-max)))
     (setq csv-file (make-temp-file "xsv-"))
     (with-temp-file csv-file
       (insert content)))

   (setq command (funcall cmd-builder input csv-file))
   (setq buf (get-buffer-create (generate-new-buffer-name "*xsv*")))
   (set-buffer buf)
   (setq result (shell-command-to-string command))
   (insert result)
   (select-window (split-window-below))
   (switch-to-buffer buf)
))

(defun xsv/search-cmd-builder (input csv-file)
  "Build the actual xsv command from given `INPUT' and `CSV-FILE'."
  (let* ((filters (xsv/split-filters input))
         (parts (mapcar #'xsv/parse-filter filters))
         (commands (mapcar #'xsv/build-search-command parts))
         (cmd))

   (setq cmd
         (s-join " | " (cons (s-concat (car commands) " " csv-file)
                     (-drop 1 commands))))

   cmd
))

(defun xsv/select-cmd-builder (input csv-file)
  "Build the actual xsv select command from given `INPUT' and `CSV-FILE'."
  (format "xsv select %s %s" input csv-file))

(defun xsv/filter (input)
  "For the given `INPUT', run the xsv search on the current buffer.
Save the search result into a new buffer with prefix `xsv-'.
`INPUT' can contain multiple criteria and each criteria should be in one of
these following syntax:
- columnA = 'regexA'
- columnB != 'regexB'
First one means search columnA where its value match regexA.
Second form means search columnB where its value NOT match regexB.

Criteria can be joined by using `&' like this:
columnA = 'regexA' & columnB = 'regexB' & columnC != 'regexC'

It means return rows where columnA matches regexA AND columnB matches regexB AND
columnC NOT matches regexC."
  (interactive
   (list (read-string "input: ")))

  (xsv/run input #'xsv/search-cmd-builder))

(defun xsv/select (columns)
  "For the given `COLUMNS', run the xsv select on the current buffer.
Save the search result into a new buffer with prefix `xsv-'.
`COLUMNS' should contain columns separated with comma and no space in between.
For eg: 'Transaction Type',Date

Column name must be single quoted when there is space in the name."

  (interactive
   (list (read-string "select columns(separated by ,): ")))

  (xsv/run columns #'xsv/select-cmd-builder))

(provide 'xsv)
;;; xsv.el ends here
