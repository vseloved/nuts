;;; NUTS-CORE system definition
;;; (c) Vsevolod Dyomkin, see LICENSE file for permissions

(in-package #:asdf)


(defsystem #:nuts-core
  :name "Non-Unit Test Suite"
  :version "0.2.0"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :long-description "A test library with main design goals of ~
universality and extensibility to allow for creation on top of it of ~
different testing models and workflows to tackle the problem of ~
test automation in various software projects.

Thus it's design is built on the concept of defining only functionally ~
orthogonal operations and a framework for integrating special-purpose add-ons.

At the same time, there's a goal to provide an out-of-the-box solution for ~
simple ad-hoc testing.

Although the project is called Non-Unit Test Suite, the name is chosen not ~
to state, that concepts from Unit Testing are refuted and not used, but rather ~
to emphasize, that Unit Testing is not considered THE solution to the testing ~
problem, but rather one of the approaches, which may be used with varying ~
efficiency in different spheres."
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "core")
               (:file "extensions"))
  :depends-on (:rutils))

;;; end