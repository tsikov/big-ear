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

  (subtest "TICKER"
    (let ((ticker (be::ticker "xbtusd,ethusd")))
      (is (type-of ticker) 'cons
          "Ticker data can be obtained via API")))

  ) ; with-vcr

(finalize)
