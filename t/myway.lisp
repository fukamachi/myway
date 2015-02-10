(in-package :cl-user)
(defpackage myway-test
  (:use :cl
        :myway
        :prove))
(in-package :myway-test)

(plan nil)

(defparameter *mapper* (make-mapper))

(is (find-route *mapper* "/") nil)
(is-values (dispatch *mapper* "/" :method :GET) '(nil nil))

(connect *mapper* "/"
         (lambda (params)
           (declare (ignore params))
           "Hello, World!"))

(ok (find-route *mapper* "/"))
(is (find-route *mapper* "/" :method :POST) nil)
(is (dispatch *mapper* "/" :method :GET) "Hello, World!")

(connect *mapper* "/post"
         (lambda (params)
           (declare (ignore params))
           "posted")
         :method :POST)

(is (find-route *mapper* "/post") nil)
(ok (find-route *mapper* "/post" :method :POST))
(is-values (dispatch *mapper* "/post" :method :GET) '(nil nil))
(is (dispatch *mapper* "/post" :method :POST) "posted")

(connect *mapper* "/new"
         (lambda (params)
           (declare (ignore params))
           "new"))

(is (dispatch *mapper* "/new" :method :GET) "new")

(is (funcall (to-app *mapper*)
             '(:request-method :GET :path-info "/"))
    "Hello, World!")

(connect *mapper* "/id/:n"
         (lambda (params)
           (if (oddp (parse-integer (getf params :n)))
               "odd"
               (next-route)))
         :name 'odd-id)

(connect *mapper* "/id/*"
         (lambda (params)
           (declare (ignore params))
           "even")
         :name 'even-id)

(is (dispatch *mapper* "/id/1") "odd")
(is (dispatch *mapper* "/id/2") "even")

(finalize)
