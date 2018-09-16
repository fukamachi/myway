(in-package :cl-user)
(defpackage myway
  (:use :cl)
  (:import-from :myway.mapper
                :mapper
                :clear-routes
                :mapper-routes
                :make-mapper
                :member-route
                :add-route
                :next-route
                :dispatch)
  (:import-from :myway.route
                :route
                :route-name
                :route-handler
                :equal-route
                :match-route
                :url-for)
  (:import-from :alexandria
                :delete-from-plist)
  (:export :make-mapper
           :connect
           :next-route
           :dispatch

           :*env*
           :to-app

           :mapper
           :clear-routes
           :mapper-routes
           :add-route
           :find-route

           :route
           :route-name
           :route-handler
           :equal-route
           :match-route
           :url-for))
(in-package :myway)

(defun connect (mapper url fn &key (method '(:GET)) regexp name)
  (add-route mapper
             (make-instance 'route
                            :url url
                            :method method
                            :regexp regexp
                            :name name
                            :handler fn)))

(defun find-route (mapper url &rest args &key method regexp name (route-class 'route) &allow-other-keys)
  (declare (ignore method regexp name))
  (car
   (member-route mapper
                 (apply #'make-instance route-class
                        :url url
                        (delete-from-plist args :route-class)))))

(defparameter *env* nil)

(defun to-app (mapper)
  (lambda (env)
    (let ((*env* env))
      (destructuring-bind (&key request-method path-info &allow-other-keys) env
        (dispatch mapper path-info :method request-method)))))
