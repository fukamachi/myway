(in-package :cl-user)
(defpackage myway.route
  (:use :cl)
  (:import-from :myway.rule
                :make-rule
                :match-rule
                :equal-rule)
  (:export :route
           :route-name
           :route-rule
           :route-handler
           :make-route
           :match-route
           :equal-route))
(in-package :myway.route)

(defstruct (route (:constructor %make-route))
  name
  rule
  handler)

(defun make-route (url &key (method '(:GET)) regexp name handler)
  (%make-route :name name
               :rule (make-rule url :method method :regexp regexp)
               :handler handler))

(defun equal-route (route1 route2)
  (and (eq (route-name route1) (route-name route2))
       (equal-rule (route-rule route1) (route-rule route2))))

(defun match-route (route method url-string &key allow-head)
  (check-type route route)
  (match-rule (route-rule route) method url-string :allow-head allow-head))
