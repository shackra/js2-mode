;;; tests/flow.el --- Some tests for js2-mode with flow syntax.

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

;;; Type annotations

;; Primitive type

(js2-deftest-parse flow-primitive-number-type
  "var a: number;")

(js2-deftest-parse flow-primitive-string-type
  "var a: string;")

(js2-deftest-parse flow-primitive-boolean-type
  "var a: boolean;")

(js2-deftest-parse flow-primitive-bool-type
  "var a: bool;")

(js2-deftest-parse flow-primitive-any-type
  "var a: any;")

(js2-deftest-parse flow-primitive-mixed-type
  "var a: mixed;")

(js2-deftest-parse flow-primitive-empty-type
  "var a: empty;")

;; Literal type

(js2-deftest-parse flow-literal-number-type
  "var a: 42;")

(js2-deftest-parse flow-literal-number-negative-type
  "var a: -42;")

(js2-deftest-parse flow-literal-number-dot-type
  "var a: .42;")

(js2-deftest-parse flow-literal-number-e-type
  "var a: 1e42;")

;; (js2-deftest-parse flow-literal-number-hex-type
;;   "var a: 0x42;")

;; (js2-deftest-parse flow-literal-string-type
;;   "var a: 'foo';")

;; (js2-deftest-parse flow-literal-string-double-quote-type
;;   "var a: \"foo\";")

(js2-deftest-parse flow-literal-true-type
  "var a: true;")

(js2-deftest-parse flow-literal-false-type
  "var a: false;")

(js2-deftest-parse flow-literal-null-type
  "var a: null;")

(js2-deftest-parse flow-literal-undefined-type
  "var a: undefined;")

(js2-deftest-parse flow-literal-this-type
  "var a: this;")

(js2-deftest-parse flow-literal-void-type
  "var a: void;")

(js2-deftest-parse flow-literal-star-type
  "var a: *;")

;; Generic Type

(js2-deftest-parse flow-generic-type
  "var a: a;")

(js2-deftest-parse flow-generic-member-type
  "var a: a.b;")

(js2-deftest-parse flow-generic-member-type-no-closed
  "var a: a.b.;"
  :syntax-error ".")

(js2-deftest-parse flow-generic-param-type
  "var a: a<b>;")

(js2-deftest-parse flow-generic-param-type-1
  "var a: a<>;")

(js2-deftest-parse flow-generic-params-type
  "var a: a<b, c>;")

;; (js2-deftest-parse flow-generic-params-nested-type
;;   "var a: a<b, c<d>>;")

;; (js2-deftest-parse flow-generic-params-nested-type-2
;;   "var a: a<b, c<d>, e>;")

;; (js2-deftest-parse flow-generic-params-nested-type-3
;;   "var a: a<b, c<d, e<f>>>;")

;; (js2-deftest-parse flow-generic-params-nested-type-4
;;   "var a: a<b, c,>;")

;; (js2-deftest-parse flow-generic-member-params-type
;;   "var a: a.b<c, d>;")

;; (js2-deftest-parse flow-generic-params-decl-type
;;   "var a: <b>(b)=>b;")

;; (js2-deftest-parse flow-generic-params-decl-type-type
;;   "var a: <b: c>(b) => b;")

;; (js2-deftest-parse flow-generic-params-decl-init-type
;;   "var a: <b = 42>(b) => b;")

;; (js2-deftest-parse flow-generic-params-decl-type-init-type
;;   "var a: <b: c = 42>(b) => b;")

;; Maybe type

(js2-deftest-parse flow-maybe-type
  "var a: ?number;")

(js2-deftest-parse flow-maybe-maybe-type
  "var a: ??number;")

;; Array type

(js2-deftest-parse flow-array-type
  "var a: number[];")

(js2-deftest-parse flow-array-array-type
  "var a: number[][];")

(js2-deftest-parse flow-array-type-no-closed
  "var a: number[;"
  :syntax-error "[")

;; Tuple type
;; Object type
;; Function type
;; Intersection type
;; Union type
