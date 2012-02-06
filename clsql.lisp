;;; NUTS-CLSQL core
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :nuts-clsql)
(named-readtables:in-readtable rutils-readtable)

;; predicates

(defmethod obj-equall-by-slots
    ((obj1 standard-db-object) (obj2 standard-db-object)
     &optional (test #'equal))
  "Slot-by-slot comparison.
If objs are of different classes the result is NIL."
  (let ((cls (class-of obj1)))
    (when (eq cls (class-of obj2))
      (handler-case
          (apply #'every test
                 (mapcar (lambda (obj)
                           (mapcar #`(slot-value obj
                                                 (closer-mop:slot-definition-name %))
                                   (remove-if #`(eq (closer-mop:slot-definition-name %)
                                                    'CLSQL-SYS::VIEW-DATABASE)
                                    (closer-mop:class-slots (class-of obj)))))
                         (list obj1 obj2)))
        (unbound-slot () nil)))))

(defmethod obj-equal
    ((obj1 standard-db-object) (obj2 standard-db-object)
     &optional (test #'equal))
  (obj-equal-by-slots obj1 obj2 test))


;; database context

(defvar *test-db-type* :postgresql-socket
  ":database-type part CLSQL connection spec")

(defmacro w/db-conn (conn-spec &body body)
  "Executes <_:arg body /> in the binding of <_:var *default-database* /> ~
to the database connection.
If connection is already established by the above, reuses it"
  `(if *default-database*
       (progn ,@body)
       (let ((*default-database* (connect ,conn-spec
                                          :database-type *test-db-type*
                                          :if-exists :old
                                          :make-default nil)))
         (unwind-protect
              (progn ,@body)
           (disconnect)))))

(defmacro w/db-context ((conn-spec &rest view-classes) &body body)
  "Inside this macro we create the DB (connection spec <_:arg conn-spec />), ~
connect to it, create the appropriate tables (for <_:arg view-classes />) ~
and execute the <_:arg body />.
Afterall everything is wiped out"
  (with-gensyms (db-spec counter)
    `(let ((,db-spec (database-name-from-spec ,conn-spec *test-db-type*)))
       (unwind-protect
            (progn (create-database ,conn-spec :database-type *test-db-type*)
                   (w/db-conn ,conn-spec
                     (mapc #'create-view-from-class ',view-classes)
                     ,@body))
         (let ((,counter 0))
           (unless (dowhile (< ,counter 10)
                     (handler-case
                         (return (destroy-database ,conn-spec
                                                   :database-type
                                                   *test-db-type*))
                       (error () (incf ,counter))))
             (warn "Can't drop the DB ~a. Try to do it manually."
                   ,db-spec)))))))


(locally-disable-literal-syntax :sharp-backq)

;;; end
