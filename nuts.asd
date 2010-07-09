;;; NUTS system definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package #:asdf)

(defsystem #:nuts
  :name "Wrapper over NUTS-CORE for real-world usage"
  :version '(0 3 1)
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :serial t
  :components ((:file "package")
               (:file "predicates")
               (:file "checks"))
  :depends-on (:rutils :nuts-core :cl-ppcre :closer-mop))

;;; end
