;;; tests/walt.el --- Some tests for js2-mode with flow syntax.

;; Copyright (C) 2009, 2011-2017  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'js2-mode)
(require 'cl-lib)

(defmacro js2-deftest (name buffer-contents &rest body)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (with-temp-buffer
       (save-excursion
         (insert ,buffer-contents))
       (unwind-protect
           (progn
             ,@body)
         (fundamental-mode)))))

(defun js2-mode--and-parse ()
  (js2-mode)
  (js2-reparse))

(defun js2-test-string-to-ast (s)
  (insert s)
  (js2-mode--and-parse)
  (should (null js2-mode-buffer-dirty-p))
  js2-mode-ast)

(cl-defun js2-test-parse-string (code-string &key syntax-error errors-count
                                             reference warnings-count)
  (ert-with-test-buffer (:name 'origin)
    (let ((ast (js2-test-string-to-ast code-string)))
      (if syntax-error
          (let ((errors (js2-ast-root-errors ast)))
            (should (= (or errors-count 1) (length errors)))
            (cl-destructuring-bind (_ pos len) (car (last errors))
              (should (string= syntax-error (substring code-string
                                                       (1- pos) (+ pos len -1))))))
        (should (= 0 (length (js2-ast-root-errors ast))))
        (ert-with-test-buffer (:name 'copy)
          (js2-print-tree ast)
          (skip-chars-backward " \t\n")
          (should (string= (or reference code-string)
                           (buffer-substring-no-properties
                            (point-min) (point)))))
        (when warnings-count
          (should (= warnings-count
                     (length (js2-ast-root-warnings ast)))))))))

(cl-defmacro js2-deftest-parse (name code-string &key bind syntax-error errors-count
                                     reference warnings-count)
  "Parse CODE-STRING.  If SYNTAX-ERROR is nil, print syntax tree
with `js2-print-tree' and assert the result to be equal to
REFERENCE, if present, or the original string.  If SYNTAX-ERROR
is passed, expect syntax error highlighting substring equal to
SYNTAX-ERROR value.  BIND defines bindings to apply them around
the test."
  (declare (indent defun))
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append bind '((js2-basic-offset 2)))
       (js2-test-parse-string ,code-string
                              :syntax-error ,syntax-error
                              :errors-count ,errors-count
                              :warnings-count ,warnings-count
                              :reference ,reference))))

(defun js2-find-node (node predicate)
  "Find the first descendant of NODE meeting PREDICATE."
  (let (target)
    (js2-visit-ast node (lambda (n end-p)
                          (unless end-p
                            (if (funcall predicate n)
                              (progn (setq target n) nil)
                              t))))
    target))

(defun js2-node-text (node)
  "Return the part of the buffer corresponding to NODE as a string."
  (let ((beg (js2-node-abs-pos node)))
    (buffer-substring-no-properties beg (+ beg (js2-node-len node)))))

(defun js2-test-indent (content keep-indent)
  (let ((s (replace-regexp-in-string "^ *|" "" content)))
    (with-temp-buffer
      (insert
       (if keep-indent
           s
         (replace-regexp-in-string "^ *" "" s)))
      (js2-jsx-mode)
      (js2-reparse) ; solely for js2-jsx-self-closing, for some reason
      (indent-region (point-min) (point-max))
      (should (string= s (buffer-substring-no-properties
                          (point-min) (point)))))))

(cl-defmacro js2-deftest-indent (name content &key bind keep-indent)
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append '(indent-tabs-mode
                     (js2-basic-offset 2)
                     (js2-pretty-multiline-declarations t)
                     (inhibit-point-motion-hooks t))
                   bind)
       (js2-test-indent ,content ,keep-indent))))

;;; Type annotations

;; Primitive type

;; (js2-deftest-parse flow-primitive-number-type
;;   "var a: number;")

;;; Exprs Stmts Decls

;; Modules

(js2-deftest-parse walt-import
  "import {a: b} from 'm';")
