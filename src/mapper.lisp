(in-package :cl-user)
(defpackage myway.mapper
  (:use :cl)
  (:import-from :myway.route
                :route-rule
                :route-handler
                :equal-route
                :match-route)
  (:import-from :myway.rule
                :rule-methods
                :rule-url)
  (:import-from :myway.util
                :make-collector)
  (:import-from :map-set
                :map-set-index)
  (:export :mapper
           :clear-routes
           :make-mapper
           :mapper-routes
           :member-route
           :add-route
           :next-route
           :dispatch))
(in-package :myway.mapper)

(defstruct mapper
  (%routes (make-collector)))

(defun clear-routes (mapper)
  (setf (mapper-%routes mapper) (make-collector)))

(defun mapper-routes (mapper)
  (funcall (mapper-%routes mapper)))

(defun member-route (mapper route)
  (member route
          (mapper-routes mapper)
          :test #'equal-route))

(defun add-route (mapper route)
  (let ((routes (member-route mapper route)))
    (if routes
        (progn
          (warn "Redefining a route for ~S ~A."
                (rule-url (route-rule route))
                (map-set-index (rule-methods (route-rule route))))
          (rplaca routes route))
        (funcall (mapper-%routes mapper) route))))

(defparameter *next-route-function* nil)
(defun next-route ()
  (funcall *next-route-function*))

(defun dispatch (mapper url-string &key (method :GET) (allow-head T))
  (check-type mapper mapper)
  (labels ((dispatch-with-rules (routes)
             (loop for (route . routes) on routes
                   do (multiple-value-bind (matchp params)
                          (match-route route method url-string :allow-head allow-head)
                        (when matchp
                          (let ((*next-route-function* (lambda () (dispatch-with-rules routes))))
                            (return
                              (values
                               (typecase (route-handler route)
                                 ((or function symbol)
                                  (funcall (route-handler route) params))
                                 (T (route-handler route)))
                               T)))))
                   finally (return (values nil nil)))))
    (dispatch-with-rules (mapper-routes mapper))))
