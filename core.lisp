;;; NUTS-CORE
;;; (c) Vsevolod Dyomkin, Oleksandr Manzyuk. see LICENSE file for permissions

(in-package :nuts-core)

(locally-enable-literal-syntax :sharp-backq)

;; basic ops

(defvar *logg-out* #p"nuts.log"
        "Output file for logging")

(defun logg (form &optional (long-format t))
 "Log the <_:arg form />'s return value to <_:var *log-output* /> file"
 (if *logg-out*
     (with-open-file (out *logg-out* :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create)
       (format out "~:[~*~;~a | ~]~a~%" long-format (nth-value 1 (now)) form))
     (format t "~:[~*~;~a | ~]~a~%" long-format (nth-value 1 (now)) form)))

(defmacro check (pred &rest args)
  "Check, if the <_:arg pred /> is satisfied. Pred should be a literal name, ~
not a function object.
Log the result."
  (with-gensyms (rez)
    `(let ((,rez (funcall ',pred ,@args)))
       (logg (format nil "~a ~{~a ~}-> ~a~%"
                     ',pred '(,@args) ,rez) nil)
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
           (lambda (,@args)
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
  (with-gensyms (name names args largs i total err errors rez rezs)
    `(let* ((,names ',(if tests
                          (mapcar #`(car (mklist _)) tests)
                          (let (lst)
                            (maphash (lambda (k v) (push k lst)) *test-thunks*)
                            lst)))
            (,args (list ,@(if tests (mapcar #``(list ,@(cdr (mklist _))) tests)
                               (loop :repeat (hash-table-count *test-thunks*)
                                     :collect nil))))
            (,total (length ,names)))
       (logg (format nil "Running ~a test~:p..." ,total))
       (let* ((,i 0)
              (,errors ())
              (,rezs (mapcar (lambda (,name ,largs)
                               (logg (format nil "test #~a: ~a~#[;~a~]"
                                             (incf ,i) ,name ,largs))
                               (let ((,rez
                                      (handler-case
                                          (if-it (gethash ,name *test-thunks*)
                                                 (multiple-value-bind
                                                       (,rez ,err)
                                                     (if ,largs
                                                         (apply it ,largs)
                                                         (funcall it))
                                                   (push ,err ,errors)
                                                   ,rez)
                                                 (progn
                                                   (logg (format nil
                                                                 "No such test: ~a~%"
                                                                 ,name))
                                                   nil))
                                        (error (,err)
                                          (unless *catch-errors?* (error ,err))
                                          (logg (format nil "ERROR ~a~%"
                                                        (type-of ,err))
                                                nil)
                                          (push ,err ,errors)))))
                                 (logg (format nil "test #~a result: ~a~%"
                                               ,i ,rez))
                                 ,rez))
                      ,names ,args)))
         (values ,rezs ,errors)))))

(locally-disable-literal-syntax :sharp-backq)

;;; end