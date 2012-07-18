(in-package :clsql-helper)

;;;; BEWARE These are largely the same (COPY/PASTA) and I have tried to comment the differences
;;;; because of all the macrology I couldnt quite figure out how to abstract it

(defmethod  %coerce-to-step (step)
  (etypecase step
    (clsql-sys:duration step)
    (keyword (ecase step
               (:second +a-second+)
               (:minute +a-minute+)
               (:hour +an-hour+)
               (:day +a-day+)
               (:month +a-month+)
               (:year +a-year+)))))

(iterate::defclause-driver
    (FOR date FROM-DATE start &optional BY (step '+a-day+) )
  "iterates through dates from start by step
   HAS NO END CHECK eg an infinite loop unless you prevent it
  "
  (iterate::top-level-check)
  (let* ((dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init )))
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-date ,start)))
     ;; this is a hack to ensure that "previous" clauses works, rather than using step
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:date+ ,date ,dur-var))))
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start &optional BY (step '+a-day+) )
  "iterates through dates from start by step
   HAS NO END CHECK eg an infinite loop unless you prevent it
  "
  (iterate::top-level-check)
  (let* ((dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init )))
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-datetime ,start)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:time+ ,date ,dur-var))))
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATE start TO end &optional BY (step '+a-day+) )
  "iterates through dates from start to end exclusive of end"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:date))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test `(if (and ,end-var (clsql-sys:date>= ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-date ,start)
                 ,end-var (convert-to-clsql-date ,end)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:date+ ,date ,dur-var)))
                 test)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start TO end &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start to end exclusive of end"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:wall-time))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test `(if (and ,end-var (clsql-sys:time>= ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-datetime ,start)
                 ,end-var (convert-to-clsql-datetime ,end)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:time+ ,date ,dur-var)))
                 test)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATE start THRU end &optional BY (step '+a-day+) )
  "iterates through dates from start to end inclusively"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:date))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test `(if (and ,end-var (clsql-sys:date> ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-date ,start)
                 ,end-var (convert-to-clsql-date ,end)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:date+ ,date ,dur-var)))
                 test)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start THRU end &optional BY (step '+a-day+) )
  "iterates through dates from start to end inclusively"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:wall-time))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test `(if (and ,end-var (clsql-sys:time> ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-datetime ,start)
                 ,end-var (convert-to-clsql-datetime ,end)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:time+ ,date ,dur-var)))
                 test)
     :variable date)))



