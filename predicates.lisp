;;; NUTS additional predicates
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :nuts)
(named-readtables:in-readtable rutils-readtable)


;; insideness

(defun inside (seq items &key key test)
  "Tests, wheater <_:arg items /> are contained in <_:arg seq />uence ~
according to its type.
<_:arg test /> and <_:arg key /> can be provided for matching"
  (every #'true
         (map 'list
              (inside-test seq :key key :test test)
              items)))

(defgeneric inside-test (seq &key)
  (:documentation "Returns a closure for testing on being <_:fun inside />
for the appropriate <_:arg seq />uence type")
  (:method (seq &key)
    (error "Not implemeted for sequence type ~a" (type-of seq)))
  (:method ((seq string) &key)
    #`(true (cl-ppcre:scan % seq)))
  (:method ((seq list) &key key test)
    (labels ((inside-list (item lst &key key test)
               (or (funcall (if test test #'eql)
                            item
                            (if key (mapcar key lst) lst))
                   (when (listp lst)
                     (dolist (sub lst)
                       (when (inside-list item sub :key key :test test)
                         (return t)))))))
      #`(inside-list % seq :key key :test test))))

(defun inside/equalp (seq items)
  "<_:fun inside /> with <_:std EQUALP /> test"
  (inside seq items :test #'equalp))



(defgeneric match-id (obj1 obj2 &optional id-getter)
  (:documentation "Tests <_:arg obj1 />'s and <_:arg obj2 />'s ~
<_:slot id />s equality (with <_:fun = />).
<_:slot id />s are extracted with <_:arg id-getter />.
Will signal slot-missing errors, if there is no <_:slot id /> slot ~
for this object class.")
  (:method (obj1 obj2 &optional id-getter)
    "If objs are of different classes the result is NIL."
    (let* ((cls (class-of obj1))
           (id-getter (or id-getter
                          #`(slot-value % (intern "ID"
                                                  (symbol-package
                                                   (class-name cls)))))))
      (when (eql cls (class-of obj2))
        (= (funcall id-getter obj1) (funcall id-getter obj2))))))

;;; end
