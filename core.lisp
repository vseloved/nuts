;;; NUTS-CORE
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :nuts-core)

(locally-enable-literal-syntax :sharp-backq)

;; basic ops

(defvar *log-out* #p"nuts.log"
        "Output file for logging")

(defmacro with-log-file ((var) &body body)
  `(with-open-stream (,var (if *log-out*
                               (open *log-out*
                                     :direction         :output
                                     :if-exists         :append
                                     :if-does-not-exist :create)
                               (make-broadcast-stream *standard-output*)))
     ,@body))

(defun logf (long-format-p control-string &rest args)
  "Print log message formatted according to <_:arg control-string /> ~
and <_:arg args>.  The destination of the message depends on the value ~
of <_:var *log-out* />: if is NIL, the message is printed to the ~
standard output; otherwise, <_:var *log-out* /> must be a pathname, ~
in which case the message is appended to the corresponding file (the ~
file is created if it does not exist).  If <_arg: long-format-p /> is ~
T, the message is prepended with the execution time. Returns NIL."
  (with-log-file (out)
    (format out "~:[~*~;~a | ~]~?"
            long-format-p
            (nth-value 1 (now)) control-string args)))

(defun logp (object)
  "Print <_:arg object /> to the log file designated by the pathname ~
<_:var *log-out* /> or to the standard output if <_:var *log-out* /> ~
is NIL. Returns <_:arg object />."
  (with-log-file (out)
    (print object out)))

(defmacro monitor (form)
  "Evaluate <_:arg form /> logfing the result.  If evaluation raises
an error, log and reraise it."
  (with-gensyms (e values)    
    `(handler-case ,form
       (error (,e)
         (logf nil "ERROR ~a~%" ,e)
         (error ,e))
       (:no-error (&rest ,values)
         (logf t "~a => ~{~a~^ ~}~%" ',form ,values)
         (values-list ,values)))))

(defmacro check (pred &rest args)
  "Check, if the <_:arg pred /> is satisfied. Pred should be a literal name, ~
not a function object.
Log the result."
  (with-gensyms (rez)
    `(let ((,rez (funcall ',pred ,@args)))
       (logf nil "  ~:[FAIL~; OK ~]   | ~a ~{~a ~}~%" ,rez ',pred ',args)
       (values ,rez ',args))))

;; tests

(defvar *test-thunks* (make-hash-table)
  "Compiled test closures")

(defmacro deftest (name (&rest args) &body body)
  "A test is just a named closure, stored in <_:var *test-thunks* /> hash-table.
It can reference other tests by name the same, as ordinary functions.
Meta-tests (test suites) can be created just by calling some tests inside ~
the other one. Thus it is possible to shadow real functions and macros with ~
tests (only inside a test), so use with caution."
  `(progn
     (when (gethash ',name *test-thunks*)
       (warn "Redefining test ~a" ',name))
     (setf (gethash ',name *test-thunks*)
           (lambda ,args
             (cumulative-and
              ,@(mapcar #`(if-it (when (listp _)
                                   ;; test referenced like a function
                                   (gethash (car _) *test-thunks*))
                                 (progn
                                   (warn "Used test ~a in function call ~
position"
                                         (car _))
                                   `(funcall ,it ,@(cdr _)))
                                 _)
                        body))))
     (values ',name
             (to-string *test-thunks*))))

(defvar *catch-errors?* t
  "Intercept error signals from tests?")

(defmacro run-test (&rest tests)
  "Run <_:arg tests />, each one supplied as a list ~
<_:pseudo (test-name args) />. If no <_:arg tests /> are provided, ~
run all tests in <_:var *test-thunks* />"
  (with-gensyms (name names args largs i total err errors rez rezs failed)
    `(let* ((,names ',(if tests
                          (mapcar #`(car (mklist _)) tests)
                          (hash-table-keys *test-thunks*)))
            (,args (list ,@(if tests (mapcar #``(list ,@(cdr (mklist _))) tests)
                               (loop :repeat (hash-table-count *test-thunks*)
                                     :collect nil))))
            (,total (length ,names)))
       (with-log-file (out) (terpri out))
       (logf t "Running ~a test~:p...~%" ,total)
       (let* ((,i 0)
              (,errors ())
              (,rezs (mapcar (lambda (,name ,largs)
                               (logf t "test #~a: ~a~#[;~a~]~%"
                                     (incf ,i) ,name ,largs)
                               (let ((,rez
                                      (handler-case
                                          (if-it (gethash ,name *test-thunks*)
                                                 (multiple-value-bind
                                                       (,rez ,err)
                                                     (apply it ,largs)
                                                   (push ,err ,errors)
                                                   ,rez)
                                                 (progn
                                                   (logf t "No such test: ~a~%" ,name)
                                                   nil))
                                        (error (,err)
                                          (unless *catch-errors?* (error ,err))
                                          (logf nil  "ERROR ~a~%" (type-of ,err))
                                          (push ,err ,errors)))))
                                 (logf t "test #~a result: ~a~%" ,i ,rez)
                                 ,rez))
                             ,names ,args)))
         (let ((,failed (mapfil (let ((,i 0))
                                  #`((incf ,i)
                                     (unless (eq _ t) ,i)))
                                ,rezs)))
           (logf t "Total tests run: ~a    Failed ~a: ~{#~a ~}~%"
                 ,total (length ,failed) ,failed))
         (values ,rezs ,errors)))))

;;; end