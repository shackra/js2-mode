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

(js2-deftest-parse flow-generic-params-type-1
  "var a: a<b, c,>;")

(js2-deftest-parse flow-generic-params-type-2
  "var a: a<;"
  :syntax-error "<")

(js2-deftest-parse flow-generic-params-type-3
  "var a: a<,;"
  :syntax-error ",")

(js2-deftest-parse flow-generic-params-type-4
  "var a: a<b;"
  :syntax-error "b")

(js2-deftest-parse flow-generic-params-type-5
  "var a: a<b,;"
  :syntax-error ",")

(js2-deftest-parse flow-generic-params-type-6
  "var a: a<,>;"
  :syntax-error ","
  :errors-count 3)

(js2-deftest-parse flow-generic-params-nested-type
  "var a: a<b, c<d>>;")

(js2-deftest-parse flow-generic-params-nested-type-2
  "var a: a<b, c<d>, e>;")

(js2-deftest-parse flow-generic-params-nested-type-3
  "var a: a<b, c<d, e<f>>>;")

(js2-deftest-parse flow-generic-member-params-type
  "var a: a.b<c, d>;")

(js2-deftest-parse flow-generic-params-decl-type
  "var a: <b>(c) => d;")

(js2-deftest-parse flow-generic-params-decl-type-2
  "var a: <b: c>(d) => e;")

(js2-deftest-parse flow-generic-params-decl-type-3
  "var a: <b = 42>(c) => d;")

(js2-deftest-parse flow-generic-params-decl-type-4
  "var a: <b: c = 42>(d) => e;")

(js2-deftest-parse flow-generic-params-decl-type-5
  "var a: <b: c = d, e>(f) => g;")

(js2-deftest-parse flow-generic-params-decl-type-6
  "var a: <b: c = d, e: f>(g) => h;")

(js2-deftest-parse flow-generic-params-decl-type-7
  "var a: <b, c: d<e>>(f) => g;")

(js2-deftest-parse flow-generic-params-decl-type-8
  "var a: <b<c>>(d) => e;")

(js2-deftest-parse flow-generic-params-decl-type-no-closed
  "var a: <b(c) => d;"
  :syntax-error "b")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-2
  "var a: <,(b) => c;"
  :syntax-error ",")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-3
  "var a: <,b(c) => d;"
  :syntax-error ","
  :errors-count 4)

(js2-deftest-parse flow-generic-params-decl-type-no-closed-4
  "var a: <,>(b) => c;"
  :syntax-error ","
  :errors-count 6)

(js2-deftest-parse flow-generic-params-decl-type-no-closed-5
  "var a: <b:>(c) => d;"
  :syntax-error ":")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-6
  "var a: <b = >(c) => d;"
  :syntax-error "=")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-7
  "var a: <b: = >(c) => d;"
  :syntax-error ":"
  :errors-count 2)

(js2-deftest-parse flow-generic-params-decl-type-no-closed-8
  "var a: <b: c = >(d) => e;"
  :syntax-error "=")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-9
  "var a: <b: c = d<>(c) => d;"
  :syntax-error ">")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-10
  "var a: <b: c = d<e>(f) => g;"
  :syntax-error ">")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-11
  "var a: <b: c = d<e(f) => g;"
  :syntax-error "e"
  :errors-count 2)

;; (js2-deftest-parse flow-generic-params-decl-type-not-decl
;;   "var a: <b: c = d<e: f>>(g) => h;")

;; Typeof type

(js2-deftest-parse flow-typeof-type
  "var a: typeof a;")

(js2-deftest-parse flow-typeof-type-nested
  "var a: typeof typeof a;")

(js2-deftest-parse flow-typeof-type-no-argument
  "var a: typeof;"
  :syntax-error "typeof")

(js2-deftest-parse flow-typeof-type-no-argument-2
  "var a: typeof typeof;"
  :syntax-error "typeof"
  :errors-count 1)

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

(js2-deftest-parse flow-tuple-type
  "var a: [a];")

(js2-deftest-parse flow-tuple-multi-type
  "var a: [a, b];")

(js2-deftest-parse flow-tuple-empty-type
  "var a: [];")

(js2-deftest-parse flow-tuple-array-type
  "var a: [][];")

(js2-deftest-parse flow-tuple-tuple-type
  "var a: [[]];")

(js2-deftest-parse flow-tuple-type-no-closed
  "var a: [;"
  :syntax-error "[")

(js2-deftest-parse flow-tuple-type-no-closed-2
  "var a: [b;"
  :syntax-error "b")

(js2-deftest-parse flow-tuple-type-no-closed-3
  "var a: [b,;"
  :syntax-error ",")

(js2-deftest-parse flow-tuple-type-trailing-comma
  "var a: [,];"
  :syntax-error ","
  :errors-count 3)

(js2-deftest-parse flow-tuple-type-trailing-comma-2
  "var a: [b,];")

;; Function type

(js2-deftest-parse flow-function-type
  "var a: b => c;")

(js2-deftest-parse flow-function-type-maybe
  "var a: ?b => c;")

(js2-deftest-parse flow-function-type-chain
  "var a: b => c => d;")

(js2-deftest-parse flow-function-type-param
  "var a: (b) => c;")

(js2-deftest-parse flow-function-type-param-1
  "var a: (b);")

(js2-deftest-parse flow-function-type-param-tuple-type
  "var a: [] => c;")

;; (js2-deftest-parse flow-function-type-param-object-type
;;   "var a: {} => c;")

(js2-deftest-parse flow-function-type-param-function-type
  "var a: (b => c) => d;")

(js2-deftest-parse flow-function-type-param-no-closed
  "var a: (;"
  :syntax-error "("
  :errors-count 2)

(js2-deftest-parse flow-function-type-param-free
  "var a: b => c;")

(js2-deftest-parse flow-function-type-params
  "var a: (b, c) => d;")

(js2-deftest-parse flow-function-type-params-trailing-comma
  "var a: (b,) => c;")

(js2-deftest-parse flow-function-type-params-no-closed
  "var a: (b => c;"
  :syntax-error "c")

(js2-deftest-parse flow-function-type-params-no-closed-2
  "var a: (b, => c;"
  :syntax-error "=>"
  :errors-count 2)

(js2-deftest-parse flow-function-type-params-named
  "var a: (b: c) => d;")

(js2-deftest-parse flow-function-type-params-named-2
  "var a: (b: c, d) => e;")

(js2-deftest-parse flow-function-type-params-named-no-closed
  "var a: (b:) => d;"
  :syntax-error ":")

(js2-deftest-parse flow-function-type-params-named-no-closed-2
  "var a: (b:,) => d;"
  :syntax-error ":")

(js2-deftest-parse flow-function-type-params-optional
  "var a: (b?: c) => d;")

(js2-deftest-parse flow-function-type-params-optional-2
  "var a: (b?) => d;"
  :syntax-error "?")

(js2-deftest-parse flow-function-type-params-rest
  "var a: (...b) => c;")

(js2-deftest-parse flow-function-type-params-rest-2
  "var a: (...b: c) => d;")

(js2-deftest-parse flow-function-type-params-rest-3
  "var a: (b, ...c) => d;")

(js2-deftest-parse flow-function-type-params-rest-4
  "var a: (b?: c, ...d) => e;")

(js2-deftest-parse flow-function-type-params-rest-5
  "var a: (...b, c) => d;"
  :syntax-error "b"
  :errors-count 5)

(js2-deftest-parse flow-function-type-params-type-params
  "var a: <b>(c) => d;")

(js2-deftest-parse flow-function-type-params-type-params-2
  "var a: <b>c => d;"
  :syntax-error ">"
  :errors-count 2)

;; Object type

(js2-deftest-parse flow-object-type
  "var a: {};")

(js2-deftest-parse flow-object-type-prop
  "var a: {b: c};")

;; (js2-deftest-parse flow-object-type-prop-no-value
;;   "var a: { b };")

;; (js2-deftest-parse flow-object-type-prop-optional
;;   "var a: { b?: c };")

;; (js2-deftest-parse flow-object-type-prop-readonly
;;   "var a: { +b: c };")

;; (js2-deftest-parse flow-object-type-prop-writeonly
;;   "var a: { -b: c };")

;; (js2-deftest-parse flow-object-type-prop-readonly-optional
;;   "var a: { +b?: c };")

;; (js2-deftest-parse flow-object-type-prop-method
;;   "var a: { b(): c };")

;; (js2-deftest-parse flow-object-type-prop-method-getset
;;   "var a: { get b(): c };")

;; ;; (js2-deftest-parse flow-object-type-prop-method-optional
;; ;;   "var a: { b?(): c };")

;; (js2-deftest-parse flow-object-type-prop-indexed
;;   "var a: { [b]: c };")

;; (js2-deftest-parse flow-object-type-prop-indexed-named
;;   "var a: { [b: c]: d };")

;; (js2-deftest-parse flow-object-type-prop-call
;;   "var a: { (): b };")

;; (js2-deftest-parse flow-object-type-prop-call-2
;;   "var a: { <b>(): c };")

;; (js2-deftest-parse flow-object-type-prop-rest
;;   "var a: { ...b };")

;; (js2-deftest-parse flow-object-type-prop-rest-2
;;   "var a: { ...b: c };")

;; (js2-deftest-parse flow-object-type-props
;;   "var a: { b: c, d: e };")

;; (js2-deftest-parse flow-object-no-closed
;;   "var a: { b:;")

;; (js2-deftest-parse flow-object-no-closed-2
;;   "var a: { b: c;")

;; (js2-deftest-parse flow-object-no-closed-3
;;   "var a: { b: c,;")

;; (js2-deftest-parse flow-object-type-props-trailing-commas
;;   "var a: { b: c, };")

;; (js2-deftest-parse flow-object-type-exact
;;   "var a: {| b: c |};")

;; (js2-deftest-parse flow-object-type-exact-no-closed
;;   "var a: {| b: c };")

;; (js2-deftest-parse flow-object-type-exact-no-closed-2
;;   "var a: {| b: c;")

;; (js2-deftest-parse flow-object-type-exact-with-union
;;   "var a: {| b: c | d |};")

;; (js2-deftest-parse flow-object-type-exact-with-union-2
;;   "var a: {| b: | c |};")

;; (js2-deftest-parse flow-object-type-exact-with-union-3
;;   "var a: { b: c |};")

;; (js2-deftest-parse flow-object-type-exact-with-union-no-closed
;;   "var a: {| b: | c };")

;; (js2-deftest-parse flow-object-type-exact-with-union-no-closed-2
;;   "var a: {| b: | c;")

;; Intersection type
;; Union type
;; Variables
;; Functions
;; Classes
;; Interface
;; Type aliases
;; Opaque type aliases
;; Modules
;; Type casing expr
;; Declares
;; Utilities
