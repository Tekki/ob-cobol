;;; test-ob-cobol.el --- tests for ob-cobol.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'ob-cobol)

(defconst ob-cobol-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (org-babel-temp-file "test-org-id-locations-"))

(defun ob-cobol-test-update-id-locations ()
  (let* ((files (directory-files
                ob-cobol-test-dir 'full
                "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))
    (org-id-update-id-locations files t)))

(defmacro org-test-at-id (id &rest body)
  "Run body after placing the point in the headline identified by ID."
  (declare (indent 1))
  `(let* ((id-location (org-id-find ,id))
	  (id-file (car id-location))
	  (visited-p (get-file-buffer id-file))
	  to-be-removed)
     (unwind-protect
	 (save-window-excursion
	   (save-match-data
	     (org-id-goto ,id)
	     (setq to-be-removed (current-buffer))
	     (condition-case nil
		 (progn
		   (org-show-subtree)
		   (org-show-block-all))
	       (error nil))
	     (save-restriction ,@body)))
       (unless (or visited-p (not to-be-removed))
	 (kill-buffer to-be-removed)))))
(def-edebug-spec org-test-at-id (form body))

(ert-deftest ob-cobol/00-assert ()
  (should t))

(ert-deftest ob-cobol/01-functions ()
  "Check if all functions are available."
  (dolist (fn '(ob-cobol-last-src-file
                org-babel-execute:cobol
                org-babel-prep-session:cobol
                ob-cobol--wrap-code))
    (should (functionp fn))))

(ert-deftest ob-cobol/02-errors ()
  "Expected errors."
  (should-error (org-babel-prep-session:cobol 1 2)))

(ert-deftest ob-cobol/10-wrap-free ()
  "Wrap code blocks in free format."
  (let* ((expected "IDENTIFICATION DIVISION.
    PROGRAM-ID. ob-cobol.

PROCEDURE DIVISION.
    DISPLAY \"Hello COBOL\".
")
        (sources `(,expected
                   "IDENTIFICATION DIVISION.
    PROGRAM-ID. ob-cobol.

PROCEDURE DIVISION.
    DISPLAY \"Hello COBOL\"."
                   "PROCEDURE DIVISION.
    DISPLAY \"Hello COBOL\"."
                   "    DISPLAY \"Hello COBOL\".")))

    (dolist (source sources)
      (should (string-equal (ob-cobol--wrap-code source "free") expected)))))

(ert-deftest ob-cobol/11-wrap-variables-free ()
  "Wrap code blocks with variables in free format."
  (let* ((expected "IDENTIFICATION DIVISION.
    PROGRAM-ID. ob-cobol.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 username PIC(30) VALUE \"Mainframe\".

PROCEDURE DIVISION.
    DISPLAY username.
")
        (sources `(,expected
                   "01 username PIC(30) VALUE \"Mainframe\".
…
    DISPLAY username.")))

    (dolist (source sources)
      (should (string-equal (ob-cobol--wrap-code source "free") expected)))))

(ert-deftest ob-cobol/12-wrap-fixed ()
  "Wrap code blocks in fixed format."
  (let* ((expected "       IDENTIFICATION DIVISION.
           PROGRAM-ID. ob-cobol.

       PROCEDURE DIVISION.
000100     DISPLAY \"Hello COBOL\".
")
         (sources `(,expected
                    "       PROCEDURE DIVISION.
000100     DISPLAY \"Hello COBOL\"."
                    "000100     DISPLAY \"Hello COBOL\".")))

    (dolist (source sources)
      (should (string-equal (ob-cobol--wrap-code source "fixed") expected)))))

(ert-deftest ob-cobol/13-wrap-variables-fixed ()
  "Wrap code blocks with variables in fixed format."
  (let* ((expected "       IDENTIFICATION DIVISION.
           PROGRAM-ID. ob-cobol.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
000100 01 username PIC(30) VALUE \"Mainframe\".

       PROCEDURE DIVISION.
000200     DISPLAY username.
")
        (sources `(,expected
                   "000100 01 username PIC(30) VALUE \"Mainframe\".
   …
000200     DISPLAY username.")))

    (dolist (source sources)
      (should (string-equal (ob-cobol--wrap-code source "fixed") expected)))))

(ert-deftest ob-cobol/20-hello1 ()
  "Complete code block."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "79d9792d-d100-4b69-9925-16088e1e8eff"
      (org-babel-next-src-block)
      (should (string-equal "Hello COBOL 1" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/21-hello2 ()
  "Code without identification division."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "5e245d78-e831-4d73-9eee-4269bbc970c3"
      (org-babel-next-src-block)
      (should (string-equal "Hello COBOL 2" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/22-hello3 ()
  "Code without any divisions."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "ca5ee15f-7247-4d70-9de2-c433d527e0f7"
      (org-babel-next-src-block)
      (should (string-equal "Hello COBOL 3" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/23-variables ()
  "Code with variables."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "b670f46a-e6c1-42b8-b1b2-47c44f0372c2"
      (org-babel-next-src-block)
      (should (string-equal "Your name is Mainframe" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/24-variables-ibm ()
  "Code with variables, IBM dialect."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-babel-eval-wipe-error-buffer)
    (org-test-at-id "8b8d8ce7-7198-4cd0-a974-7ce4a92287b3"
      (org-babel-next-src-block)
      (should (string-equal "IBM" (org-babel-execute-src-block)))
      (when (get-buffer org-babel-error-buffer-name)
        (with-current-buffer org-babel-error-buffer-name
          (should-not (string-match-p "OCCURS DEPENDING ON"
                                      (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest ob-cobol/25-numbers-ibm ()
  "Code with negative numbers, IBM dialect."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "4aaa24ff-1e49-498f-a68d-46460962e10f"
      (org-babel-next-src-block)
      (should (string-equal "01234567-" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/30-fixed-hello1 ()
  "Code with fixed format."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "4a3eeb0a-6dff-40a2-b560-69197b31c5cd"
      (org-babel-next-src-block)
      (should (string-equal "Hello COBOL 1b" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/31-fixed-hello2 ()
  "Fixed format without identification division."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "201d4878-ef70-48b9-ba2b-d4347bce1a62"
      (org-babel-next-src-block)
      (should (string-equal "Hello COBOL 2b" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/32-fixed-hello3 ()
  "Fixed format without any divisions."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "7ac5d279-cc51-47f3-8344-77657c53d74e"
      (org-babel-next-src-block)
      (should (string-equal "Hello COBOL 3b" (org-babel-execute-src-block))))))

(ert-deftest ob-cobol/33-fixed-variables ()
  "Fixed format with variables."
  (let (org-confirm-babel-evaluate)
    (ob-cobol-test-update-id-locations)
    (org-test-at-id "d0b4e2cf-f4df-4a01-9431-0731aaf74a25"
      (org-babel-next-src-block)
      (should (string-equal "Your name is Mainframe B" (org-babel-execute-src-block))))))

(provide 'ob-cobol-test)
