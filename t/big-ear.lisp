(in-package #:cl-user)
(defpackage #:be.test
  (:use #:cl #:prove #:vcr))
(in-package #:be.test)

(setf prove:*enable-colors* t)

(plan 1)

(with-vcr "kraken"

  (subtest "REQUEST"
    (let* ((uri "https://api.kraken.com/0/public/Ticker?pair=xbtusd")
           (response (be::request uri)))
      (is (type-of response) 'cons
          "Can make an API request")))

  (subtest "utils"
    (subtest "log-errors"
      ;; if an error is thrown, log it and continue execution.
      (is-print (be::log-errors
                 (error "Test error"))
                "ERROR: Test error"
                "When an error is thrown, log it and continue execution")))
      
  (subtest "TICKER"
    (let ((ticker (be::ticker "xbtusd,ethusd")))
      (is (type-of ticker) 'cons
          "Ticker data can be obtained via API")))

  ) ; with-vcr

(finalize)
