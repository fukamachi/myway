(in-package :cl-user)
(defpackage myway
  (:use :cl)
  (:import-from :myway.mapper
                :mapper
                :make-mapper
                :member-route
                :add-route
                :next-route
                :dispatch)
  (:import-from :myway.route
                :route
                :make-route)
  (:import-from :myway.util
                :function-name)
  (:export :make-mapper
           :connect
           :next-route
           :dispatch

           :*env*
           :to-app

           :route
           :mapper
           :add-route
           :find-route
           :make-route))
(in-package :myway)

(defun connect (mapper url fn &key (method '(:GET)) regexp (name (when (functionp fn)
                                                                   (function-name fn))))
  (add-route mapper
             (make-route url
                         :method method
                         :regexp regexp
                         :name name
                         :handler (typecase fn
                                    (function fn)
                                    (T (lambda (params)
                                         (declare (ignore params))
                                         fn))))))

(defun find-route (mapper url &key (method '(:GET)) regexp name)
  (car
   (member-route mapper
                 (make-route url
                             :method method
                             :regexp regexp
                             :name name))))

(defparameter *env* nil)

(defun to-app (mapper)
  (lambda (env)
    (let ((*env* env))
      (destructuring-bind (&key method path-info &allow-other-keys) env
        (dispatch mapper path-info :method method)))))
