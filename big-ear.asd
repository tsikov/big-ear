(in-package #:cl-user)
(defpackage #:big-ear-asd
  (:use #:cl #:asdf))
(in-package #:big-ear-asd)

(defsystem #:big-ear
  :version      "0.1.0"
  :description  "Fetch market data and store in db"
  :author       "Petko Tsikov <tsikov@gmail.com>"
  :serial       t
  :license      "MIT"
  :depends-on   (#:drakma)
  :components   ((:module "src"
                          :components
                          ((:file "big-ear"))))
  :in-order-to ((test-op (test-op big-ear-test))))

(defsystem #:big-ear-test
  :description "Fetch market data and store in db test system"
  :author "Petko Tsikov <tsikov@gmail.com>"
  :license "MIT"
  :depends-on (#:prove)
  :defsystem-depends-on (#:prove-asdf)
  :serial t
  :components ((:module "t"
                        :components
                        ((:test-file "big-ear"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
