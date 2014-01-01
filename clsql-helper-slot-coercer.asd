;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clsql-helper.system)
    (defpackage :clsql-helper.system
	(:use :common-lisp :asdf))))

(in-package clsql-helper.system)

(defsystem :clsql-helper-slot-coercer
  :description "A library providing a single overwritde of (setf slot-value-using-class)
   so that obvious type coercions occur when setting slots on clsql:standard-db-objects

   USE CAUTION, LOADING SYSTEM WILL CHANGE THE SEMANTICS OF CLSQL:VIEW-CLASS
   (slightly, and probably in a good way)"
  :licence "BSD"
  :version "0.1"
  :components ((:file "set-slot-value-using-class"))
  :depends-on (:clsql-helper :closer-mop))

(defsystem :clsql-helper-slot-coercer-test
  :description "Tests for a library providing a clutch of utilities to make
     working with clsql easier"
  :licence "BSD"
  :version "0.1"
  :components ((:module :tests
			:serial t
			:components ((:file "set-slot-value-using-class"))))
  :depends-on (:clsql-helper-slot-coercer :lisp-unit2))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :clsql-helper-slot-coercer))))
  (asdf:load-system :clsql-helper-slot-coercer-test)
  (asdf:test-system :clsql-helper)
  (let ((*package* (find-package :clsql-helper-slot-coercer-test)))
    (eval (read-from-string "(run-tests)"))))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
