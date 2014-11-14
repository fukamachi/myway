(in-package :cl-user)
(defpackage myway.route
  (:use :cl)
  (:import-from :myway.rule
                :make-rule
                :match-rule
                :equal-rule
                :rule-url-for)
  (:export :route
           :route-name
           :route-rule
           :route-handler
           :match-route
           :equal-route
           :url-for))
(in-package :myway.route)

(defclass route ()
  ((name :initarg :name
         :initform nil
         :accessor route-name)
   (rule :accessor route-rule)
   (handler :initarg :handler
            :accessor route-handler)))

(defmethod initialize-instance :after ((route route) &rest initargs &key url (method '(:GET)) regexp &allow-other-keys)
  (declare (ignore initargs))
  (setf (route-rule route)
        (make-rule url :method method :regexp regexp)))

(defgeneric equal-route (route1 route2)
  (:method ((route1 route) (route2 route))
    (and (eq (route-name route1) (route-name route2))
         (equal-rule (route-rule route1) (route-rule route2)))))

(defgeneric match-route (route method url-string &key allow-head)
  (:method ((route route) method url-string &key allow-head)
    (match-rule (route-rule route) method url-string :allow-head allow-head)))

(defun url-for (route params)
  (rule-url-for (route-rule route) params))
