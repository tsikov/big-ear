(in-package #:cl-user)
(defpackage #:be.test
  (:use #:cl #:prove))
(in-package #:be.test)

(setf prove:*enable-colors* t)

(plan 1)

(ok t)

(finalize)
