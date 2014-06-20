(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(defgeneric copy-instance (i)
  (:documentation
   "Makes a copy of an instance via allocate instance then copies all
    slot-values and calls reinitialize-instance
   ")
  (:method (i)
    (iter (with i-class = (class-of i))
      (with c = (allocate-instance i-class))
      (for sd in (closer-mop:class-slots i-class))
      (for sn = (closer-mop:slot-definition-name sd))
      (if (slot-boundp i sn)
          (setf (slot-value c sn)
                (slot-value i sn))
          (slot-makunbound c sn))
      (finally
       (reinitialize-instance c)
       (return c)))))

(defparameter *default-object-diff-ignore-list*
  '(diff original diff-state
    id view-database
    dateentered dateenteredhistory
    date-entered username FieldsChanged datemodified
    clsql-helper:dirty-slots
    clsql-helper:record-dirty?
    clsql-helper:dirty-test
    clsql-helper::%retrieved-at
    clsql-helper::%history-select-fn))

(defmethod object-diff-equals (v1 v2)
  (cond
    ((and (typep v1 'clsql:date) (typep v2 'clsql:wall-time))
     (setf v2 (convert-to-clsql-date v2)))
    ((and (typep v2 'clsql:date) (typep v1 'clsql:wall-time))
     (setf v1 (convert-to-clsql-date v1))))
  (typecase v1
    (float
     ;; handle monetary amounts that dont
     ;; equal because of float nonsense
     (< (abs (- v1 (or v2 0.0))) 0.01))
    ((or clsql-sys:wall-time clsql-sys:date)
     (equalp v1 v2))
    (T (equal v1 v2))))

(defmethod object-diff-list
    (o1 o2 &key (ignored-slot-names *default-object-diff-ignore-list*)
           (compare #'object-diff-equals)
     &aux (same-type? (eql (type-of o1) (type-of o2))))
  (setf ignored-slot-names (alexandria:ensure-list ignored-slot-names))
  (iter
    (with slots =
          (if same-type?
              (access::class-slot-definitions o1)
              (union (access:class-slot-definitions o1)
                     (access:class-slot-definitions o2)
                     :key #'closer-mop:slot-definition-name)))
    (for s in slots)
    (for reader = (or (first (closer-mop:slot-definition-readers s))
                      (closer-mop:slot-definition-name s)))
    (for name = (closer-mop:slot-definition-name s))
    ;; skip join slots for db stuff
    ;; skip ignored slots
    (when (or (clsql-helper:join-slot? s)
              (member name ignored-slot-names :test #'access:equalper))
      (next-iteration))

    (for v1 = (access:access o1 reader))
    (for v2 = (access:access o2 reader))
    (for values-equal? = (funcall compare v1 v2))
    (unless values-equal?
      ;; TODO: convert to an object instead of a list
      (collect (list name v1 v2)))))

(define-condition merging-values ()
  ((target :accessor target :initarg :target :initform nil)
   (from :accessor from :initarg :from :initform nil)
   (value :accessor value :initarg :value :initform nil)
   (slot :accessor slot :initarg :slot :initform nil)))

(define-condition merge-conflict (error)
  ((already-saved :accessor already-saved :initarg :already-saved :initform nil
                 :documentation "The object that was saved while we were working")
   (slot :accessor slot :initarg :slot :initform nil)
   (old-value :accessor old-value :initarg :old-value :initform nil)
   (saved-value :accessor saved-value :initarg :saved-value :initform nil)
   (pending-value :accessor pending-value :initarg :pending-value :initform nil)))

(define-condition merge-conflicts (error)
  ((conflicts :accessor conflicts :initarg :conflicts :initform nil)))

(defmethod merge-changes (original already-saved pending)
  (let* ((saved-changes (object-diff-list original already-saved))
         (pending-changes (object-diff-list original pending))
         (to-save
           (iter (for change in pending-changes)
             (for (slot ov nv) = change)
             (for saved? = (find slot saved-changes :key #'first :test #'eql))
             (for saved-nv = (third saved?))
             (cond
               (saved?
                (unless (object-diff-equals saved-nv nv)
                  (restart-case (error 'merge-conflict
                                       :already-saved already-saved
                                       :slot slot
                                       :old-value (access:access original slot)
                                       :saved-value saved-nv 
                                       :pending-value nv)
                    (overwrite ()
                      :report "Save the pending value"
                      (collect change))
                    (skip ()
                      :report "Use the already saved value (dont save my value)"))))
               (t (collect change)))))
         )
    (iter (for change in to-save)
      (for (slot ov nv) = change)
      (with-simple-restart (abort "Skip merging this value")
        (signal 'merging-values
                :target already-saved :from pending
                :slot slot :value nv)
        (setf (access:access already-saved slot) nv)))
    already-saved))
