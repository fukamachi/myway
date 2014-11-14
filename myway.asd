#|
  This file is a part of myway project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Sinatra-compatible routing library.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage myway-asd
  (:use :cl :asdf))
(in-package :myway-asd)

(defsystem myway
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-ppcre
               :quri
               :map-set
               :alexandria
               :cl-utilities)
  :components ((:module "src"
                :components
                ((:file "myway" :depends-on ("route" "mapper" "util"))
                 (:file "rule")
                 (:file "route" :depends-on ("rule"))
                 (:file "mapper" :depends-on ("route" "util"))
                 (:file "util"))))
  :description "Sinatra-compatible routing library."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op myway-test))))
