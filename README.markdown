# My Way

My Way is a Sinatra-compatible URL routing library. This was originally written as Clack.Util.Route, a part of [Clack](http://clacklisp.org/).

## Usage

```common-lisp
(use-package :myway)

(defvar *mapper* (make-mapper))

(connect *mapper* "/" "Welcome to My Way.")

(connect *mapper* "/hello/?:name?"
         (lambda (params)
           (format nil "Hello, ~A" (or (getf params :name)
                                       "Guest"))))

(dispatch *mapper* "/")
;=> "Welcome to My Way."
;   T

(dispatch *mapper* "/hello")
;=> "Hello, Guest"
;   T

(dispatch *mapper* "/hello/Eitaro")
;=> "Hello, Eitaro"
;   T

(dispatch *mapper* "/hello/Eitaro" :method :POST)
;=> NIL
;   NIL
```

### next-route

```common-lisp
(connect *mapper* "/guess/:who"
         (lambda (params)
           (if (string= (getf params :who) "Eitaro")
               "You got me!"
               (next-route))))

(connect *mapper* "/guess/*"
         (lambda (params)
           (declare (ignore params))
           "You missed!"))
```

### to-app

`to-app` makes a Clack app from `mapper`.

```common-lisp
(to-app *mapper*)
;=> #<CLOSURE (LAMBDA (MYWAY::ENV) :IN TO-APP) {100E24F13B}>

(clack:clackup (to-app *mapper*))
```

## Installation

```common-lisp
(ql:quickload :myway)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
