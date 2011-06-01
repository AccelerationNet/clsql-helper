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

   "
  (if (clsql-sys::specified-type slot)
      (cond
	((and (typep new 'string) (< 0 (length new)))
	 (cond
	   ((subtypep (clsql-sys::specified-type slot) 'clsql-sys:wall-time)
	    (setf (closer-mop:slot-value-using-class
		   class object slot)
		  (convert-to-clsql-datetime new)))
	   ((subtypep (clsql-sys::specified-type slot) 'clsql-sys:date)
	    (setf (closer-mop:slot-value-using-class
		   class object slot)
		  (convert-to-clsql-date new)))
	   (T (call-next-method))))
	((and (typep new 'number)
	      (not (typep new 'double-float))
	      (clsql-sys::specified-type slot)
	      (subtypep (clsql-sys::specified-type slot) 'double-float))
	 (setf (closer-mop:slot-value-using-class class object slot)
	       (coerce new 'double-float)))

	;; we specified a string, we have a value and the value isnt a string
	((and new (not (stringp new)) (subtypep (clsql-sys::specified-type slot) 'string))
	 (setf (closer-mop:slot-value-using-class class object slot)
	       (princ-to-string new)))
	(t (call-next-method)))
      (call-next-method)))
