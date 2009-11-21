;;; NUTS-CORE extensions
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :nuts-core)

(locally-enable-literal-syntax :sharp-backq)

;;; fixtures

;; Possible types of fixtures:
;; * lisp
;; * fit (from html tables)
;; * xml
;; * yaml
;; * json


(defgeneric load-fixture (name type stream)
  (:documentation "Read a <_:arg name />d fixture of type <_:arg type /> ~
from <_:arg stream />")
  (:method (name type stream)
    (declare (ignore name))
    (loop for obj = (read stream nil)
       while obj collect obj))
  (:method (name (type (eql :lisp)) stream)
    (declare (ignore name))
    (loop for obj = (read stream nil)
       while obj collect obj)))

(defgeneric get-fixture-ext (type)
  (:documentation "Get file extension for the fixtures of the given ~
<_:arg type />")
  (:method (type) "lisp")
  (:method ((type (eql :lisp))) "lisp"))

(defmacro with-fixtures ((base-path &rest fixture-defs) &body body)
  "A typical with- macro, which binds names in <_:arg fixture-defs />
of the following form <_:pseudo (names* [:type type] [:path path]) />
to fixtures, loaded by this same name, type (supplied or global --
<_:var *fixture-type* />) and path, that can be either supplied or
calculated as BASE-PATH/name.extension (extension is type-specific).
Several fixtures can be loaded from 1 file, if several names are supplied
in 1 <_:arg fixture-def />."
; If * is supplied as name, all fixtures of
;a given type are loaded from the specified file (in this case it is an error,
;if path isn't specified)"
  (macrolet ((get-from-def (what)
               `(and (listp def) (getf (cdr def) ,what))))
    `(let* (,@(iter (:for def :in fixture-defs)
                    (:for type := (get-from-def :type))
                    (:for path := (get-from-def :path))
                    (:for names := ;(if (eql (car def) '*)
                                   ;    (get-all-fixture-names type path)
                          (loop :for sym :in def :until (keywordp sym)
                                :collect sym))
                    (:for stream := (gensym "STREAM"))
                    (:append (cons `(,stream ,(when path `(open ,path)))
                                   (mapcar 
                                    #``(,_ (load-fixture
                                            ,(string _)
                                            ,type
                                            ,(if path stream
                                                 `(open ,(format nil
                                                                 "~a/~(~a~).~a"
                                                                 base-path
                                                                 _
                                                                 (get-fixture-ext type))))))
                                           names)))))
       ,@body)))


;; context

(defmacro with-teardown (teardown-form &body body)
  "Executes a <_:arg teardown-form /> on unwinding"
  `(unwind-protect (progn ,@body)
     ,teardown-form))

(defmacro with-context ((&key setup teardown) &body body)
  "Executes <_:arg body /> inside the bindings, established by ~
<_:arg setup /> pairs, with <_:arg teardwon />"
  `(bind ,setup
     (with-teardown ,teardown
       ,@body)))


;; interception

(defvar *inter-acc* '()
  "Accumulator for intercepted forms. Needed to be dynamic for ~
use in <_:fun eval />")

(defmacro with-inter ((&rest name-val-pairs) &body body)
  "Instead of calling functions with names (first parts) in ~
<_:arg name-val-pairs /> collects the calling forms to a list and returns it.
Substitutes the form with the second part of the name-value-pair (value).
Captures the arguments, with which the function was intended to be applied, ~
so that they can be referred to in the value-producing form.
Won't work on CL-USER functions"
  (with-gensyms (old-defs nv-pairs names)
    `(let* ((,nv-pairs (mapcar #'mklist ',name-val-pairs))
            (,names (mapcar (compose #'car #'mklist) ,nv-pairs))
            (,old-defs (mapcar #'fdefinition ,names)))
       (setf *inter-acc* nil)
       (mapcar (lambda (name nv-pair)
                 (setf (fdefinition name)
                       (compile nil (eval
                                     `(lambda (&rest inter-args)
                                        (push (nconc (list ',name) inter-args)
                                              *inter-acc*)
                                        ,(cadr nv-pair))))))
               ,names
               ,nv-pairs)
       ,@body
       (mapcar (lambda (name old-def)
                 (setf (fdefinition name) old-def))
               ,names
               ,old-defs)
       *inter-acc*)))

;;; end