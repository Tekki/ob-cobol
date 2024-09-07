;;; ob-cobol.el --- Org-babel functions for COBOL  -*- lexical-binding: t; -*-

;; Copyright © 2024 Tekki

;; Author: Tekki (Rolf Stöckli)
;; Maintainer: Tekki
;; Created: 2024-08-27
;; Updated: 2024-09-07
;; Keywords: cobol languages org babel
;; Homepage: https://github.com/Tekki/ob-cobol
;; Version: 0.0.5
;; Package-Requires: ((emacs "29.1"))

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

(defcustom ob-cobol-source-format "free"
  "Default format for source code, either \"free\" or \"fixed\".

Overwrite in code block with parameter `:source-format'."
  :group 'org-babel
  :type '(choice (const "free")
                 (const "fixed")))

(defcustom ob-cobol-dialect "default"
  "The COBOL dialect used by the compiler.

One of \"default\", \"acu\", \"acu-strict\", \"bs2000\",
\"bs2000-strict\", \"cobol2002\", \"cobol2014\", \"cobol85\",
\"ibm\", \"ibm-strict\", \"mf\", \"mf-strict\", \"mvs\",
\"mvs-strict\", \"rm\", \"rm-strict\", \"xopen\".

Overwrite in code block with parameter `:dialect'."
  :group 'org-babel
  :type '(choice (const "default")
                 (const "acu")
                 (const "acu-strict")
                 (const "bs2000")
                 (const "bs2000-strict")
                 (const "cobol2002")
                 (const "cobol2014")
                 (const "cobol85")
                 (const "ibm")
                 (const "ibm-strict")
                 (const "mf")
                 (const "mf-strict")
                 (const "mvs")
                 (const "mvs-strict")
                 (const "rm")
                 (const "rm-strict")
                 (const "xopen")))

(defvar ob-cobol-compiler "cobc"
  "Path to the COBOL compiler.")

(defvar ob-cobol-last-src-file nil
  "The last compiled source file.")

(defun ob-cobol-last-src-file ()
  "Open the last compiled source file."
  (interactive)

  (if ob-cobol-last-src-file
      (progn
        (if (eq 1 (length (window-list)))
            (split-window-sensibly)
          (other-window 1))
        (find-file ob-cobol-last-src-file))
    (message "No file available!")))

(defun ob-cobol--wrap-code (code source-format)
  "Wrap incomplete CODE formatted as SOURCE-FORMAT according to the rules of this package."
  (let ((wrapped code)
        (source-indent (if (string-equal source-format "fixed") "       " "")))

    ;; optionally add data and procedure divisions
    (unless (string-match-p "division\." wrapped)
      (setq wrapped
            (if (string-match-p "…" wrapped)

                ;; split source code between data and procedure division
                (apply 'format
                       "%1$sDATA DIVISION.
%1$sWORKING-STORAGE SECTION.
%2$s

%1$sPROCEDURE DIVISION.
%3$s"
                       source-indent
                       (string-split wrapped "\n[ ]*…[ ]*\n"))

              ;; add everything to procedure division
              (format "%1$sPROCEDURE DIVISION.\n%2$s" source-indent wrapped))))

    ;; optionally add identification division
    (unless (string-match-p "identification division\." wrapped)
      (setq wrapped
            (format "%1$sIDENTIFICATION DIVISION.
%1$s    PROGRAM-ID. ob-cobol.

%2$s" source-indent wrapped)))

    ;; optionally add trailing newline
    (unless (string-equal (substring wrapped -1) "\n")
      (setq wrapped (concat wrapped "\n")))

    wrapped))

(defun org-babel-execute:cobol (body params)
  "Execute BODY with COBOL code with org-babel.
Accepted PARAMS:
  `:source-format', optionally set format to \"fixed\" or \"free\"
  `:dialect', optionally set the dialect of the code

This function is called by `org-babel-execute-src-block'."

  (setq ob-cobol-last-src-file (org-babel-temp-file "cobol-src-" ".cbl"))

  (let* ((tmp-binary (org-babel-temp-file "cobol-bin-"))
         (processed-params (org-babel-process-params params))
         (dialect (alist-get :dialect processed-params ob-cobol-dialect))
         (source-format (alist-get :source-format processed-params ob-cobol-source-format))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (wrapped-body (ob-cobol--wrap-code body source-format)))

    ;; write to temporary file
    (with-temp-file ob-cobol-last-src-file (insert wrapped-body))

    ;; compile file and return result
    (when-let ((results
                (org-babel-eval
                 (format "%s -%s -std=%s -x -o %s -j %s"
                         ob-cobol-compiler
                         source-format
                         dialect
                         tmp-binary
                         ob-cobol-last-src-file)
                 "")))
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
        (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:cobol (_session _params)
  "COBOL is a compiled language with no support for sessions."
  (error "COBOL is a compiled language -- no support for sessions"))

(provide 'ob-cobol)
;;; ob-cobol.el ends here
