;;; NUTS-CLSQL package definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package :cl-user)

(defpackage :nuts-clsql
  (:use :common-lisp
        :rutils.user
        :nuts-trivial
        :clsql-user)
  (:export #:*test-db-type*
           #:w/db-context))

;;; end