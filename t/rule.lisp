(in-package :cl-user)
(defpackage myway-test.rule
  (:use :cl
        :myway.rule
        :prove))
(in-package :myway-test.rule)

(plan nil)

(subtest "method"
  (ok (match-rule (make-rule "/hello")
                  :GET "/hello")
      "GET")

  (ok (match-rule (make-rule "/hello" :method :GET)
                  :GET "/hello")
      "GET")

  (ok (match-rule (make-rule "/hello" :method :GET)
                  :HEAD "/hello" :allow-head t)
      "HEAD is allowed for GET rule")

  (ok (match-rule (make-rule "/new" :method :POST)
                  :POST "/new"))

  (is (match-rule (make-rule "/new" :method :POST)
                  :GET "/new")
      nil
      "GET fails for POST rule")

  (ok (match-rule (make-rule "/new" :method '(:GET :POST))
                  :GET "/new")
      "GET or POST")

  (ok (match-rule (make-rule "/new" :method '(:GET :POST))
                  :POST "/new")
      "GET or POST")

  (ok (match-rule (make-rule "/new" :method '(:ANY))
                  :POST "/new")
      "ANY"))

(subtest "with named parameters"
  (is-values (match-rule (make-rule "/hello/:name" :method :GET)
                         :GET "/hello/fukamachi")
             '("/hello/fukamachi" (:name "fukamachi"))
             "match")
  (is-values (match-rule (make-rule "/hello/:name" :method :GET)
                         :GET "/hello/fukamachi/eitaro")
             '(nil)
             "containing a slash")
  (is-values (match-rule (make-rule "/hello/:name" :method :GET)
                         :GET "/bye/fukamachi")
             '(nil)
             "not match")
  (is-values (match-rule (make-rule "/blog/:post-id" :method :GET)
                         :GET "/blog/10")
             '("/blog/10" (:post-id "10")))
  (is-values (match-rule (make-rule "/tag/:tag" :method :GET)
                         :GET "/tag/#lisp")
             '("/tag/#lisp" (:tag "#lisp")))
  (is-values (match-rule (make-rule "/hello/?:name?" :method :GET)
                         :GET "/hello/Eitaro")
             '("/hello/Eitaro" (:name "Eitaro")))
  (is-values (match-rule (make-rule "/hello/?:name?" :method :GET)
                         :GET "/hello/")
             '("/hello/" nil))
  (is-values (match-rule (make-rule "/hello/?:name?" :method :GET)
                         :GET "/hello")
             '("/hello" nil))
  (is-values (match-rule (make-rule "/say/:hello/to/:name" :method :GET)
                         :GET "/say/hello/to/fukamachi")
             '("/say/hello/to/fukamachi" (:hello "hello" :name "fukamachi"))
             "multiple named parameters"))

(subtest "splat"
  (is-values (match-rule (make-rule "/say/*/to/*" :method :GET)
                         :GET "/say/hello/to/world")
             '("/say/hello/to/world" (:splat ("hello" "world")))))

(subtest "regex rules"
  (is-values (match-rule (make-rule "/hello/([\\w]+)" :method :GET :regexp t)
                         :GET "/hello/world")
             '("/hello/world" (:captures ("world")))))

(subtest "optional parameters"
  (is-values (match-rule (make-rule "/?:foo?/?:bar?" :method :GET)
                         :GET "/hello/world")
             '("/hello/world" (:foo "hello" :bar "world")))
  (is-values (match-rule (make-rule "/?:foo?/?:bar?" :method :GET)
                         :GET "/hello")
             '("/hello" (:foo "hello")))
  (is-values (match-rule (make-rule "/?:foo?/?:bar?" :method :GET)
                         :GET "/")
             '("/" nil))
  (is-values (match-rule (make-rule "/hello/?:name?" :method :GET)
                         :GET "/hello")
             '("/hello" nil)))

(subtest "splat and normal cases"
  (is-values (match-rule (make-rule "/:foo/*" :method :GET)
                         :GET "/foo/bar/baz")
             '("/foo/bar/baz" (:splat ("bar/baz") :foo "foo"))))

(subtest "escape"
  (is-values (match-rule (make-rule "/te+st/" :method :GET)
                         :GET "/te%2Bst/")
             '("/te%2Bst/" nil)
             "escape +")
  (is-values (match-rule (make-rule "/te st/" :method :GET)
                         :GET "/te%2Bst/")
             '("/te%2Bst/" nil)
             "escape space")
  (is-values (match-rule (make-rule "/test$/" :method :GET)
                         :GET "/test%24/")
             '("/test%24/" nil)
             "escape $")
  (is-values (match-rule (make-rule "/te.st/" :method :GET)
                         :GET "/te.st/")
             '("/te.st/" nil)
             "escape .")
  (is-values (match-rule (make-rule "/te.st/" :method :GET)
                         :GET "/te0st/")
             '(nil)
             "escape .")
  (is-values (match-rule (make-rule "/test(bar)/" :method :GET)
                         :GET "/test%28bar%29/")
             '("/test%28bar%29/" nil)
             "escape ()"))

(subtest "url-for"
  (is (rule-url-for (make-rule "/hello/?:name?") '(:name "Eitaro"))
      "/hello/Eitaro")
  (is (rule-url-for (make-rule "/hello/?:name?") nil)
      "/hello")
  (is (rule-url-for (make-rule "/hello/?:name?") '(:name "Eitaro Fukamachi"))
      "/hello/Eitaro%20Fukamachi"))

(finalize)
