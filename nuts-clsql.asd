;;; NUTS-CLSQL system definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package #:asdf)

(defsystem #:nuts-clsql
  :name "Add-ons to NUTS to gracefully work with CLSQL"
  :version '(0 1 0)
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :serial t
  :components ((:file "package-clsql")
               (:file "clsql"))
  :depends-on (:nuts-trivial :rutils
               :clsql :clsql-postgresql-socket))

;;; end