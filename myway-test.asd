#|
  This file is a part of myway project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage myway-test-asd
  (:use :cl :asdf))
(in-package :myway-test-asd)

(defsystem myway-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:myway
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "rule")
                 (:test-file "myway"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
