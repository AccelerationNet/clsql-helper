(in-package :clsql-helper)

;;;; BEWARE These are largely the same (COPY/PASTA) and I have tried to comment the differences
;;;; because of all the macrology I couldnt quite figure out how to abstract it

(iterate::defclause-driver
    (FOR date FROM-DATE start &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start by step
   HAS NO END CHECK eg an infinite loop unless you prevent it
  "
  (iterate::top-level-check)
  (let* ((dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (setqs (iterate::do-dsetq date `(clsql-sys:date+ ,date ,dur-var))))
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var ,step
                 ,date (convert-to-clsql-date ,start)))
     :step (list setqs)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start by step
   HAS NO END CHECK eg an infinite loop unless you prevent it
  "
  (iterate::top-level-check)
  (let* ((dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (setqs (iterate::do-dsetq date `(clsql-sys:time+ ,date ,dur-var))))
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var ,step
                 ,date (convert-to-clsql-datetime ,start)))
     :step (list setqs)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATE start TO end &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start to end exclusive of end"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:date))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (setqs (iterate::do-dsetq date `(clsql-sys:date+ ,date ,dur-var)))
         (test `(if (and ,end-var (clsql-sys:date>= ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var ,step
                 ,date (convert-to-clsql-date ,start)
                 ,end-var (convert-to-clsql-date ,end)))
     :step (list setqs test)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start TO end &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start to end exclusive of end"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:wall-time))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (setqs (iterate::do-dsetq date `(clsql-sys:time+ ,date ,dur-var)))
         (test `(if (and ,end-var (clsql-sys:time>= ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var ,step
                 ,date (convert-to-clsql-datetime ,start)
                 ,end-var (convert-to-clsql-datetime ,end)))
     :step (list setqs test)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATE start THRU end &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start to end inclusively"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:date))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (setqs (iterate::do-dsetq date `(clsql-sys:date+ ,date ,dur-var)))
         (test `(if (and ,end-var (clsql-sys:date> ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var ,step
                 ,date (convert-to-clsql-date ,start)
                 ,end-var (convert-to-clsql-date ,end)))
     :next (list test)
     :step (list setqs)
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start THRU end &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start to end inclusively"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:wall-time))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (setqs (iterate::do-dsetq date `(clsql-sys:time+ ,date ,dur-var)))
         (test `(if (and ,end-var (clsql-sys:time> ,date ,end-var)) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var ,step
                 ,date (convert-to-clsql-datetime ,start)
                 ,end-var (convert-to-clsql-datetime ,end)))
     :next (list test)
     :step (list setqs)
     :variable date)))

