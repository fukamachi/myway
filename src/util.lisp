(in-package :cl-user)
(defpackage myway.util
  (:use :cl)
  (:import-from :cl-utilities
                :with-collectors)
  (:export :make-collector))
(in-package :myway.util)

(defun make-collector ()
  (let ((none '#:none))
    (declare (dynamic-extent none))
    (with-collectors (buffer)
      (return-from make-collector
        (lambda (&optional (data none))
          (unless (eq data none)
            (buffer data))
          buffer)))))
