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
  (let* ((pattern "\\(\\w+\\) *\\(!?=\\) *\\('.*'\\)")
        (matching (s-match pattern filter)))

    (if matching
      (-drop 1 matching)
      (error "Wrong input, should be column-name ='regex' or column-name != 'regex'"))
))

(defun xsv/build-command (filter)
  "`FILTER' is a list of three elements in the order of column, operator, regex.
Return the xsv command for the given `FILTER'."
    (let ((column (nth 0 filter))
          (operator (nth 1 filter))
          (regex (nth 2 filter))
          (invert ""))

        (when (string= "!=" operator)
          (setq invert "-v"))

        (format "xsv search %s -s %s %s" invert column regex)))

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

  (let* ((filters (xsv/split-filters input))
         (parts (mapcar #'xsv/parse-filter filters))
         (commands (mapcar #'xsv/build-command parts))
         (cur-file (buffer-file-name))
         (content)
         (final-cmd)
         (buf)
         (result))

   ;; if cur-file is nil, ie we may visit a buffer which is not a file
   ;; then we need to save buffer content to a temp file first
   (unless cur-file
     (setq content (buffer-substring (point-min) (point-max)))
     (setq cur-file (make-temp-file "xsv-"))
     (with-temp-file cur-file
       (insert content)))

   (setq final-cmd
         (s-join " | " (cons (s-concat (car commands) " " cur-file)
                     (-drop 1 commands))))

   (setq buf (get-buffer-create (generate-new-buffer-name "*xsv*")))
   (set-buffer buf)
   (setq result (shell-command-to-string final-cmd))
   (insert result)
   (select-window (split-window-below))
   (switch-to-buffer buf)
))

(provide 'xsv)
;;; xsv.el ends here
