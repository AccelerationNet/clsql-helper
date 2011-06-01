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
  :components ((:file "date")
               (:file "clsql"))
  :depends-on (:iterate :clsql :closer-mop :cl-ppcre
                :cl-interpol :symbol-munger :alexandria))

#+ASDF-SYSTEM-CONNECTIONS
(asdf:defsystem-connection adwcodebase-clsql-postgres-connection
  :description "the part of adwcode base dedicated to postgresql"
  :requires (:adwcodebase :clsql-postgresql-socket :cl-ppcre)
  :components ((:module :src
                 :components ((:file "postgres")))))

#+ASDF-SYSTEM-CONNECTIONS
(asdf:defsystem-connection adwcodebase-clsql-postgres3-connection
  :description "the part of adwcode base dedicated to postgresql"
  :requires (:adwcodebase :clsql-postgresql-socket3 :cl-ppcre)
  :components ((:module :src
                 :components ((:file "postgres")))))

#+ASDF-SYSTEM-CONNECTIONS
(asdf:defsystem-connection adwcodebase-clsql-odbc-connection
  :description "the part of adwcode base dedicated to postgresql"
  :requires (:adwcodebase :clsql-odbc )
  :components ((:module :src
                 :components ((:file "mssql-db-object")))))

(defsystem :clsql-helper-test
  :description "Tests for a library providing a clutch of utilities to make
     working with clsql easier"
  :licence "BSD"
  :version "0.1"
  :components ((:module :tests
			:serial t
			:components ((:file "clsql"))))
  :depends-on (:clsql-helper :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :clsql-helper))))
  (asdf:load-system :clsql-helper-test)
  (let ((*package* (find-package :clsql-helper-test)))
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