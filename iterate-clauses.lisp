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
               (:year +a-year+)
               (:negative-second +a-negative-second+)
               (:negative-minute +a-negative-minute+)
               (:negative-hour +a-negative-hour+)
               (:negative-day +a-negative-day+)
               (:negative-month +a-negative-month+)
               (:negative-year +a-negative-year+)))))

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


(defun %date-test-fn-finder (start end &key date? to?)
  `(let* ((neg? ,(if date?
                    `(clsql-sys:date<= ,end ,start)
                    `(clsql-sys:time<= ,end ,start)))
          (test-fn (cond
                     ((and (not ,date?) ,to?       (not neg?)) #'clsql-sys:time>=)
                     ((and (not ,date?) (not ,to?) (not neg?)) #'clsql-sys:time>)
                     ((and (not ,date?) ,to?       neg?) #'clsql-sys:time<=)
                     ((and (not ,date?) (not ,to?) neg?) #'clsql-sys:time<)
                     ((and ,date?       ,to?       (not neg?)) #'clsql-sys:date>=)
                     ((and ,date?       (not ,to?) (not neg?)) #'clsql-sys:date>)
                     ((and ,date?       ,to?       neg?) #'clsql-sys:date<=)
                     ((and ,date?       (not ,to?) neg?) #'clsql-sys:date<)))
          )
    ;;(break "~S ~S~%~A ~A ~A ~A" ,start ,end ,date? ,to? neg? test-fn)
    test-fn
    ))

(defun %date-test-form (date end-var test)
  `(when (and ,end-var (funcall ,test ,date ,end-var))
    (go ,iterate::*loop-end*)))

(iterate::defclause-driver
    (FOR date FROM-DATE start TO end &optional BY (step '+a-day+) )
  "iterates through dates from start to end exclusive of end"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:date))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test-var (iterate::make-var-and-default-binding 'test)))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-date ,start)
                 ,end-var (convert-to-clsql-date ,end)
                 ,test-var ,(%date-test-fn-finder date end-var :date? t :to? t)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:date+ ,date ,dur-var)))
                 (%date-test-form date end-var test-var))
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start TO end &optional BY (step '(clsql:make-duration :day 1)) )
  "iterates through dates from start to end exclusive of end"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:wall-time))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test-var (iterate::make-var-and-default-binding 'test)))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-datetime ,start)
                 ,end-var (convert-to-clsql-datetime ,end)
                 ,test-var ,(%date-test-fn-finder date end-var :date? nil :to? t)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:time+ ,date ,dur-var)))
                 (%date-test-form date end-var test-var))
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATE start THRU end &optional BY (step '+a-day+) )
  "iterates through dates from start to end inclusively"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:date))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test-var (iterate::make-var-and-default-binding 'test)))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-date ,start)
                 ,end-var (convert-to-clsql-date ,end)
                 ,test-var ,(%date-test-fn-finder date end-var :date? t :to? nil)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:date+ ,date ,dur-var)))
                 (%date-test-form date end-var test-var))
     :variable date)))

(iterate::defclause-driver
    (FOR date FROM-DATETIME start THRU end &optional BY (step '+a-day+) )
  "iterates through dates from start to end inclusively"
  (iterate::top-level-check)
  (let* ((end-var (iterate::make-var-and-default-binding 'end :type 'clsql-sys:wall-time))
         (dur-var (iterate::make-var-and-default-binding 'dur :type 'clsql-sys:duration))
         (init-var (iterate::make-var-and-default-binding 'init ))
         (test-var (iterate::make-var-and-default-binding 'test)))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq
                 ,dur-var (%coerce-to-step ,step)
                 ,init-var T
                 ,date (convert-to-clsql-datetime ,start)
                 ,end-var (convert-to-clsql-datetime ,end)
                 ,test-var ,(%date-test-fn-finder date end-var :date? nil :to? nil)))
     :next (list (iterate::do-dsetq date
                   `(if ,init-var
                     (progn (setf ,init-var nil) ,date)
                     (clsql-sys:time+ ,date ,dur-var)))
                 (%date-test-form date end-var test-var))
     :variable date)))



