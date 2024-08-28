;;; ob-cobol.el --- Org-babel functions for COBOL  -*- lexical-binding: t; -*-

;; Copyright © 2024 Tekki

;; Author: Tekki (Rolf Stöckli)
;; Maintainer: Tekki
;; Created: August 27, 2024
;; Keywords: cobol, languages, org, babel
;; Homepage: https://github.com/
;; Version: 0.0.1

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating COBOL code.

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)

;; optionally define a file extension for this language
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("cobol" . "cbl"))

(defvar org-babel-default-header-args:cobol '())

(defvar ob-cobol-source-format "free"
  "Default format for source code, either \"free\" or \"fixed\".")

(defvar ob-cobol-compiler "cobc"
  "Path to the COBOL compiler.")

(defun org-babel-execute:cobol (body params)
  "Execute a block of Template code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing COBOL source code block")
  (let* ((tmp-src-file (org-babel-temp-file "cobol-src-" ".cbl"))
         (tmp-binary (org-babel-temp-file "cobol-bin-"))
         (processed-params (org-babel-process-params params))
         (source-format (alist-get :source-format processed-params ob-cobol-source-format))
         (source-indent (if (string-equal source-format "fixed") "       " ""))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (wrapped-body (concat body "\n")))

    ;; optionally add data and procedure divisions
    (unless (string-match-p "division\." wrapped-body)
      (setq wrapped-body
            (if (string-match-p "…" wrapped-body)

                ;; split source code between data and procedure division
                (apply 'format
                       "%1$sDATA DIVISION.
%1$sWORKING-STORAGE SECTION.
%2$s
%1$sPROCEDURE DIVISION.
%3$s"
                       source-indent
                       (string-split wrapped-body "\n[ ]*…[ ]*\n"))

              ;; add everything to procedure division
              (format "%1$sPROCEDURE DIVISION.\n%2$s" source-indent wrapped-body))))

    ;; optionally add identification division
    (unless (string-match-p "identification division\." wrapped-body)
      (setq wrapped-body
            (format "%1$sIDENTIFICATION DIVISION.
%1$s    PROGRAM-ID. ob-cobol.
%2$s" source-indent wrapped-body)))

    ;; write to temporary file
    (with-temp-file tmp-src-file (insert wrapped-body))

    ;; compile file and return result
    (let ((results
           (org-babel-eval
            (format "%s -%s -x -o %s -j %s" ob-cobol-compiler source-format tmp-binary tmp-src-file)
            "")))
      (when results
        (org-babel-reassemble-table
         (if (or (member "table" (cdr (assoc :result-params processed-params)))
           (member "vector" (cdr (assoc :result-params processed-params))))
       (let ((tmp-file (org-babel-temp-file "cobol-")))
         (with-temp-file tmp-file (insert (org-babel-trim results)))
         (org-babel-import-elisp-from-file tmp-file))
       (org-babel-read (org-babel-trim results) t))
         (org-babel-pick-name
          (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
         (org-babel-pick-name
          (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:cobol (_session _params)
  "This function does nothing as COBOL is a compiled language with no
support for sessions."
  (error "COBOL is a compiled language -- no support for sessions"))

(provide 'ob-cobol)
;;; ob-cobol.el ends here
