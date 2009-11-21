;;; NUTS-CORE utils
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :nuts-core)

(locally-enable-literal-syntax :sharp-backq)


(defun true (form)
  "Opposite to <_:fun null /> (so will return T and not the value of
a form, when it's non-null)"
  (not (null form)))

(defmacro cumulative-and (&rest args)
  "Like <_:fun and />, but treats only T as true and everything else as NIL.
Besides, collects all non-T values in a second return value"
  (with-gensyms (cur non-ts rezs)
    `(let ((,rezs `(,,@args))
           ,non-ts) ; not true results
       (dotimes (i ,(length args))
         (let ((,cur (elt ,rezs i)))
           (unless (eq ,cur t)
             (push ,cur ,non-ts))))
       (if ,non-ts
           (values nil (nreverse ,non-ts))
           t))))

(defmacro now ()
  "Current DATE and TIME as multiple values, formatted as: YYYY-MM-DD, HH:MM:SS"
  (with-gensyms (sec min hour day mon year)
    `(multiple-value-bind (,sec ,min ,hour ,day ,mon ,year) (get-decoded-time)
      (values
       (format nil "~a-~2,'0d-~2,'0d" ,year ,mon ,day)
       (format nil "~2,'0d:~2,'0d:~2,'0d" ,hour ,min ,sec)))))

;;; end