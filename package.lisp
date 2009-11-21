;;; NUTS-CORE & NUTS package definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :cl-user)

(defpackage #:nuts-core
  (:use :common-lisp
        #:rutils.usr)
  (:export ;; basic ops
           #:*logg-out*
           #:logg
           #:check

           ;; test definition
           #:*test-thunks*
           #:*catch-errors?*
           #:deftest
           #:run-test

           ;; fixtures
           #:load-fixture
           #:get-fixture-ext
           #:with-fixtures
           
           ;; teardown
           #:with-teardown
           #:with-context
            
           ;; interception
           #:with-inter
            
           ;; utils
           #:true
           #:cumulative-and))

(defpackage #:nuts
  (:use :common-lisp #:rutils.usr #:nuts-core)
  (:export ;; predicates
           #:inside
           #:inside-test
           #:inside/equalp
           #:match-id

           ;; checks
           #:check-t
           #:check-errs
           #:check-funcall
           #:errs))

(eval-always
  (rutils.user:export-exported-symbols "NUTS-CORE" "NUTS")
  (pushnew :nuts *features*))

;;; end