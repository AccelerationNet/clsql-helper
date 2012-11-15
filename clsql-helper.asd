;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clsql-helper.system)
    (defpackage :clsql-helper.system
	(:use :common-lisp :asdf))))

(in-package clsql-helper.system)

(defsystem :clsql-helper
  :description "A library providing a clutch of utilities to make working with clsql easier"
  :licence "BSD"
  :version "0.1"
  :serial T
  :components ((:file "package")
               (:file "iterate-clauses")
               (:file "connections")
               (:file "date")
               (:file "clsql")
               (:file "dirty")
               (:file "migrations"))
  :depends-on (:iterate :clsql :closer-mop :cl-ppcre
                :cl-interpol :symbol-munger :alexandria
                :md5 :access))

(defsystem :clsql-helper-test
  :description "Tests for a library providing a clutch of utilities to make
     working with clsql easier"
  :licence "BSD"
  :version "0.1"
  :components ((:module :tests
			:serial t
			:components ((:file "clsql")
                                     (:file "iterate-clauses"))))
  :depends-on (:clsql-helper :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :clsql-helper))))
  (asdf:load-system :clsql-helper-test)

  ;; this is just so we can test stuff that requires a db connection
  ;; not really a big deal if it fails, we will just skip a couple of tests
  (when (ignore-errors (asdf:load-system :clsql-sqlite3))
    (pushnew :clsql-sqlite3 *features*))

  (let ((*package* (find-package :clsql-helper-test)))
    (eval (read-from-string "(run-tests :all)"))))

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