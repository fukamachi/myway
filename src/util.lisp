(in-package :cl-user)
(defpackage myway.util
  (:use :cl)
  (:import-from :cl-utilities
                :with-collectors)
  (:export :make-collector
           :function-name))
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

(defun function-name (fn)
  (when (symbolp fn)
    (return-from function-name fn))
  #+ccl (ccl:function-name fn)
  #-ccl
  (multiple-value-bind (lambda closurep name) (function-lambda-expression fn)
    (declare (ignore closurep))
    (cond
      (lambda nil)
      ((and (listp name)
            (or (eq (car name) 'labels)
                (eq (car name) 'flet)))
       (cadr name))
      ((and (listp name)
            (eq (car name) 'lambda)) nil)
      (T name))))
