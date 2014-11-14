(in-package :cl-user)
(defpackage myway.rule
  (:use :cl)
  (:import-from :quri
                :url-encode)
  (:import-from :map-set
                :map-set
                :make-map-set
                :ms-insert
                :ms-member-p
                :map-set-index)
  (:import-from :alexandria
                :ensure-list)
  (:export :rule
           :regex-rule
           :make-rule
           :match-rule
           :equal-rule
           :rule-url-for))
(in-package :myway.rule)

(defun list-to-map-set (elements)
  (let ((ms (make-map-set)))
    (dolist (el elements ms)
      (ms-insert ms el))))

(defvar *default-rule-methods*
  (list-to-map-set '(:GET)))

(defstruct (rule (:constructor %make-rule))
  (methods *default-rule-methods* :type map-set)
  url
  regex
  format-string
  param-keys)

(defstruct (regex-rule (:include rule)
                       (:constructor %make-regex-rule)))

(defun make-rule (url &key (method :GET) regexp)
  (let ((rule (if regexp
                  (%make-regex-rule :methods (list-to-map-set (ensure-list method)) :url url)
                  (%make-rule :methods (list-to-map-set (ensure-list method)) :url url))))
    (if regexp
        (setf (rule-regex rule) (rule-url rule)
              (rule-format-string rule) (ppcre:regex-replace-all "\\(.+?\\)" (rule-regex rule) "~A"))
        (compile-rule rule))
    rule))

(defun compile-rule (rule)
  (check-type rule rule)
  (loop with list = (ppcre:split "(?::([\\w-]+)|(\\*))" (rule-url rule)
                                 :with-registers-p t :omit-unmatched-p t)
        for (prefix name) on list by #'cddr
        collect (ppcre:regex-replace-all
                 "[^\\?\\/\\w-]" prefix
                 #'escape-special-char
                 :simple-calls t) into re
        collect prefix into cs
        if (string= name "*")
          collect :splat into names
          and collect "(.*?)" into re
          and collect "~A" into cs
        else if name
               collect (read-from-string (concatenate 'string ":" name)) into names
               and collect "([^/?]+)" into re
               and collect "~A" into cs
        finally
           (setf (rule-regex rule) (format nil "^~{~A~}$" re)
                 (rule-format-string rule) (ppcre:regex-replace-all "~A\\?" (format nil "~{~A~}" cs)
                                                                    "~:[~;~:*~A~]")
                 (rule-param-keys rule) names)))

(defun escape-special-char (char)
  (let ((enc (url-encode (string char))))
    (cond
      ((string= char " ") (format nil "(?:~A|~A)" enc (escape-special-char #\+)))
      ((string= enc char) (ppcre:quote-meta-chars enc))
      (t enc))))

(defun match-method-p (rule method &key allow-head)
  (check-type rule rule)
  (check-type method keyword)
  (flet ((method-equal (rule-method)
           (or (eq :ANY rule-method)
               (eq method rule-method)
               (and (eq method :HEAD)
                    allow-head
                    (string= :GET rule-method)))))
    (some #'method-equal (map-set-index (rule-methods rule)))))

(defun match-rule (rule method url-string &key allow-head)
  (check-type rule rule)
  (check-type method keyword)
  (check-type url-string string)
  (when (match-method-p rule method :allow-head allow-head)
    (multiple-value-bind (matchp values)
        (ppcre:scan-to-strings (rule-regex rule) url-string)
      (when matchp
        (values matchp
                (if (regex-rule-p rule)
                    `(:captures ,(coerce values 'list))
                    (loop for key in (rule-param-keys rule)
                          for val across values
                          if (eq key :splat)
                            collect val into splat
                          else if val
                            append (list key val) into result
                          finally
                             (return (if splat
                                         `(:splat ,splat ,@result)
                                         result)))))))))

(defun equal-rule (rule1 rule2)
  (and (let ((rule2-methods (rule-methods rule2)))
         (every (lambda (rule)
                  (ms-member-p rule2-methods rule))
                (map-set-index (rule-methods rule1))))
       (string= (rule-url rule1) (rule-url rule2))))

(defgeneric rule-url-for (rule params)
  (:method ((rule rule) params)
    (let ((url (apply #'format nil (rule-format-string rule)
                      (loop for key in (rule-param-keys rule)
                            if (eq key :splat)
                              collect (pop (getf params key))
                            else if (getf params key)
                                   collect (url-encode (getf params key))
                                   and do (remf params key)
                            else
                              collect ""))))
      (values
       (ppcre:regex-replace-all
        "\\?"
        (ppcre:regex-replace-all "(.\\?)+$" url "") "")
       params)))
  (:method ((rule regex-rule) params)
    (values (apply #'format (rule-format-string rule)
                   (getf params :captures))
            (and (remf params :captures) params))))
