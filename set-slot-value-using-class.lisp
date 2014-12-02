(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)

(defmethod (setf closer-mop:slot-value-using-class)
    (new
     (class clsql-sys::standard-db-class)
     (object clsql-sys:standard-db-object)
     (slot closer-mop:standard-effective-slot-definition))
  "Ensure that if we try to set a slot on a db-object to a value whos type doesnt match
   that we coerce that value to an appropriate clsql type

   Conversions run through `coerce-value-to-db-type`

   "
  (let* ((spec-type (clsql-sys::specified-type slot))
         val coerced?)
    ;; skip bad conversions and leave it for the db to signal
    (handler-bind ((type-coercion-error
                     (lambda (c)
                       (unless (member (to-type c) (list 'clsql:wall-time 'clsql:date))
                         (continue c)))))
      (multiple-value-setq (val coerced?)
        (coerce-value-to-db-type new spec-type)))
    
    ;; we want to prevent infinite recursion if we didnt convert, if we did,
    ;; give all multimethods a shot at the new value
    (if coerced?
        (setf (closer-mop:slot-value-using-class class object slot) val)
        (call-next-method))))
