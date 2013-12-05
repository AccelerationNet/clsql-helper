(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)

(defmethod (setf closer-mop:slot-value-using-class)
    (new
     (class clsql-sys::standard-db-class)
     (object clsql-sys:standard-db-object)
     (slot closer-mop:standard-effective-slot-definition))
  "Ensure that if we try to set a slot on a db-object to a value whos type doesnt match
   that we coerce that value to an appropriate clsql type

   Conversions:
      strings to date
      numbers to double-float
      symbols to strings
      bits to booleans

   "
  (let ((spec-type (clsql-sys::specified-type slot)))
  (if spec-type
      (cond
        ((and (subtypep spec-type 'boolean)
              (typep new '(integer 0 1)))
         (setf (closer-mop:slot-value-using-class
                class object slot)
               (= 1 new)))
        ;; should have been an integer, but got a string
        ((and (subtypep spec-type 'integer)
              (typep new 'string))
         (setf new (trim-and-nullify new))
         (setf (closer-mop:slot-value-using-class
                class object slot)
               (when new (parse-integer new))))

        ;; should have been an float, but got a string
        ((and (subtypep spec-type '(or number double-float))
              (stringp new))
         (setf (closer-mop:slot-value-using-class
                class object slot)
               (relaxed-parse-float new)))

        ;; got a number that wasnt a double-float, but should have been
	((and (subtypep spec-type 'double-float)
              (typep new 'number)
	      (not (typep new 'double-float)))
	 (setf (closer-mop:slot-value-using-class class object slot)
	       (coerce new 'double-float)))

        ;; should have been a datetime
        ((and (subtypep spec-type 'clsql-sys:wall-time)
              (not (typep new '(or null clsql-sys:wall-time))))
         (let ((it (convert-to-clsql-datetime new)))
           (unless it (error "bad-type-conversion to datetime ~A" it))
           (setf (closer-mop:slot-value-using-class class object slot) it )))

        ;; should have been a date
        ((and (subtypep spec-type 'clsql-sys:date)
              (not (typep new '(or null clsql-sys:date))))
         (let ((it (convert-to-clsql-date new)))
           (unless it (error "bad-type-conversion to date ~A" it))
           (setf (closer-mop:slot-value-using-class class object slot) it)))

	;; we specified a string, we have a value and the value isnt a string
	((and new (not (stringp new)) (subtypep (clsql-sys::specified-type slot) 'string))
	 (setf (closer-mop:slot-value-using-class class object slot)
	       (princ-to-string new)))
	(t (call-next-method)))
      (call-next-method))))
