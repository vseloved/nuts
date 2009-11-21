;;; NUTS special checks
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions


(in-package :nuts)


(locally-enable-literal-syntax :sharp-backq)

(defmacro check-t (form)
  "Checks, if <_:arg form />'s return value is non-NIL
and in this case returns T instead of a it"
  `(check 'true ,form))

(defmacro check-errs (form &rest error-types)
  "Check, that errors of <_:arg error-types /> (or any errors,
if this list is empty) are signaled. Errors are not re-signalled"
  `(handler-case
       (progn ,form
              nil)
     ,@(mapcar #``(,_ (c) (declare (ignore c)) t)
               (or error-types '(error)))))

(defmacro check-funcall (form fun-form)
  "Check, that <_:arg fun-form /> was called inside <_:arg form />"
  `(check 'inside/equalp
          (with-inter ((car ,fun-form))
            ,form)
          ,fun-form))

(locally-disable-literal-syntax :sharp-backq)

;;; end