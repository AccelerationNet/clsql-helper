(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(defclass dirty-slot ()
  ((slot-name :accessor slot-name :initarg :slot-name :initform nil)
   (old-value :accessor old-value :initarg :old-value :initform nil)
   (new-value :accessor new-value :initarg :new-value :initform nil)))

(defmethod print-object ((o dirty-slot) s)
  "Print the auto-print-items for this instance."
  (print-unreadable-object (o s :type t :identity t)
    (ignore-errors (format s "~A" (slot-name o)))))

(defun make-dirty-slot (name old new)
  (make-instance 'dirty-slot :slot-name name :old-value old :new-value new))

(defclass dirty-db-slots-mixin ()
  ((dirty-slots :accessor dirty-slots :initarg :dirty-slots :initform nil
                :db-kind :virtual)
   (dirty-test :accessor dirty-test :initarg :dirty-test :initform `((T . ,#'equalp))
               :db-kind :virtual))
  (:metaclass clsql-sys::standard-db-class))

(defmethod initialize-instance :after ((o dirty-db-slots-mixin)
                                       &key &allow-other-keys)
  ;; lets reset any changes caused by initialization
  ;; Not sure if this is correct, but other mixins are setting
  ;; values during init, which are not particularly dirty
  ;; (ie they havent changed, just got set)
  (reset-dirty o))

(defun find-dirty-test ( o slot-name )
  (or
   (iter (for (slot-names . test) in (dirty-test o))
     (when (or (eql t slot-names)
               (member slot-name (alexandria:ensure-list slot-names)))
       (return test)))
   #'equalp))

(defun %dirty-before (new class object slot
                      &aux (name (closer-mop:slot-definition-name slot)))
  ;; if its not bound but has an init form, then we are in object creation
  (when (and
         *record-this-dirty-slot* ;; dont record if updating from database
         (slot-boundp object 'dirty-slots)
         (slot-boundp object 'dirty-test)
         (not (member name '(dirty-slots dirty-test)))
         ;; only db-slots count for this plugin
         (eql :base (clsql-sys::view-class-slot-db-kind slot)))
    (let* ((test-fn (find-dirty-test object name))
           (old (when (slot-boundp object name)
                      (closer-mop:slot-value-using-class class object slot)))
           (dirty? (not (funcall test-fn new old))))
      (when dirty?
        (pushnew (make-dirty-slot name old new) (dirty-slots object) :key #'slot-name)))))

(defun reset-dirty ( o )
  (setf (dirty-slots o) nil))

(defgeneric slot-dirty? ( object slot-name &key all?)
  (:documentation "determines whether or not a slot on a given object is dirty
      slot-name can be a list and all? determines if we want to not if all of
      them or dirty or if any of them are dirty")
  (:method ((o dirty-db-slots-mixin) slot-name &key (all? nil))
    (iter (for sn in (alexandria:ensure-list slot-name))
      (let ((res (find (clsql-sys::to-slot-name sn)
                       (dirty-slots o)
                       :key #'slot-name)))
        (if all?
            (always res)
            (thereis res))))))

(defmethod clsql-sys::get-slot-values-from-view :after
    ((o dirty-db-slots-mixin) slotdefs vals)
  "This setfs slot values from the database values during select, so it makes sense to reset after
   ward"
  (reset-dirty o))

(defmethod clsql-sys::update-slot-from-db-value :around ((o dirty-db-slots-mixin) slot value)
  " disable dirty slot recording if the value is from the database "
  (let ( *record-this-dirty-slot* )
    (call-next-method)))

(defmethod fill-identifier! :around ((o dirty-db-slots-mixin) &key database)
  (declare (ignore o database))
  (let ( *record-this-dirty-slot* )
    (call-next-method)))

(defmethod clsql-sys:update-instance-from-records :after
    ((o dirty-db-slots-mixin) &key &allow-other-keys)
  (reset-dirty o))

(defmethod (setf closer-mop:slot-value-using-class) :before
    (new
     (class clsql-sys::standard-db-class)
     (object dirty-db-slots-mixin)
     (slot closer-mop:standard-effective-slot-definition))
  (%dirty-before new class object slot))

(defmacro defmethod-when-possible (name args &body body)
  (when (typep (ignore-errors (fdefinition name)) 'standard-generic-function)
    `(handler-case
      ;; I dont want a broken one of these to cause the whole library to be broken.
      ;; clsql tends to lag a bit behind clsql-helper
      (defmethod ,name ,args ,@body)
      (error (c) (declare (ignorable c))
       ;; I think it is fine just ignoring the definition that doesnt work instead of
       ;; complaining
       (format *error-output* "~%warning skipping method definition: ~%~A~%" c)
       ))))

(defmethod-when-possible clsql-sys::view-classes-and-storable-slots ((object dirty-db-slots-mixin))
  ;; todo: broken for update-instance-from-record and to be removed when clsql catches up
  (let ((classes-and-slots (call-next-method)))
    (iter (for class-and-slots in classes-and-slots)
      (for defs = (iter (for slot-def in (clsql-sys::slot-defs class-and-slots))
                    (when (slot-dirty? object slot-def)
                      (collect slot-def))))
      (when defs
        (setf (clsql-sys::slot-defs class-and-slots) defs)
        (collect class-and-slots)))))

(defmethod-when-possible clsql-sys::view-classes-and-storable-slots ((object dirty-db-slots-mixin)
                                                                    &key to-database-p)
  (let ((classes-and-slots (call-next-method)))
    (if (or (null to-database-p)
            ;; we only filter updates currently (presumably inserts dont have unneeded data)
            (null (clsql-sys::view-database object)))
        classes-and-slots
        (iter (for class-and-slots in classes-and-slots)
          (for defs = (iter (for slot-def in (clsql-sys::slot-defs class-and-slots))
                        (when (and (eql :base (clsql-sys::view-class-slot-db-kind slot-def))
                                   (slot-dirty? object slot-def))
                          (collect slot-def))))
          (when defs
            (setf (clsql-sys::slot-defs class-and-slots) defs)
            (collect class-and-slots))))))

#| reimplementation for normal classes (different metaclass)

(defclass dirty-slots-mixin ()
  ((dirty :accessor dirty :initarg :dirty :initform nil)
   (dirty-test :accessor dirty-test :initarg :dirty-test :initform `((T . ,#'equalp)))))

(defmethod (setf closer-mop:slot-value-using-class) :before
    (new
     (class standard-class)
     (object dirty-slots-mixin)
     (slot closer-mop:standard-effective-slot-definition))
  (%dirty-before new class object slot))

|#