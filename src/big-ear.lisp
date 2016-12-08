(in-package #:cl-user)
(defpackage #:big-ear
  (:nicknames :be)
  (:use #:cl)
  (:export #:ticker))
(in-package #:big-ear)

;; make it possible for drakma to get json response as text
;; unless it is already possible
(pushnew '("application" . "json")
         drakma:*text-content-types*
         :test 'equal)

(defparameter +uri+ "https://api.kraken.com/0/public/")

(defstruct (ticker (:type list))
  pair ask bid last volume volume/24h volume-wa
  volume-wa/24h n-trades n-trades/24h low low/24h high high/24h open)

(defun ensure-number (str/num)
  "Ensure item is a number"
  (if (typep str/num 'number)
      str/num
      (read-float-from-string str/num)))

(defun read-float-from-string (str)
  (with-input-from-string (in str)
    (read in)))

(defun get-info (ticker-data indicator)
  "Get ticker data info by key"
  (let ((bindings '((:ask           :a second)
                    (:bid           :b second)
                    (:last          :c second)
                    (:volume        :v second)
                    (:volume/24h    :v third)
                    (:volume-wa     :p second)
                    (:volume-wa/24h :p third)
                    (:n-trades      :t second)
                    (:n-trades/24h  :t third)
                    (:low           :l second)
                    (:low/24h       :l third) 
                    (:high          :h second)
                    (:high/24h      :h third)
                    (:open          :b second))))
    
    (destructuring-bind (_ key pos)
        (assoc indicator bindings)
      (declare (ignore _))
      (ensure-number
       (funcall pos (assoc key ticker-data))))))

(defun kraken->be (list)
  "map kraken symbols to big ear symbols"
  (let ((bindings '((:+xxbtzusd+ . :btcusd)
                    (:+xethzusd+ . :ethusd))))
    (sublis bindings list)))

(defun get-error (response)
  (cdr (assoc :error response)))

(defun get-result (response)
  (cdr (assoc :result response)))

(defun decode-json (encoded-json-string)
  (json:decode-json-from-string encoded-json-string))

(defun handle-response (json-encoded-string)
  (let* ((response (decode-json json-encoded-string))
         (error (get-error response))
         (result (get-result response)))
    (if error
        (error "Kraken said: ~A" error)
        result)))

(defun handle-ticker-response (response)
  "Substitute symbols from ticker response with ones suitable for
Big Ear."
  (kraken-be (handle-response response)))

(defun request (uri)
  "Make an HTTP request"
  (handle-response (drakma:http-request uri)))

(defun ticker (pair)
  "Ticker data"
  (let* ((full-url (format nil "~A~A~A" +uri+ "Ticker?pair=" pair))
         (response (kraken->be (request full-url))))
    (loop for (pair . pair-data) in response
         collect (make-ticker :pair pair
                              :ask (get-info pair-data :ask)
                              :bid (get-info pair-data :bid)
                              :last (get-info pair-data :last)
                              :volume (get-info pair-data :volume)
                              :volume/24h (get-info pair-data :volume/24h)
                              :volume-wa (get-info pair-data :volume-wa)
                              :volume-wa/24h (get-info pair-data :volume-wa/24h)
                              :n-trades (get-info pair-data :n-trades)
                              :n-trades/24h (get-info pair-data :n-trades/24h)
                              :low (get-info pair-data :low)
                              :low/24h (get-info pair-data :low/24h)
                              :high (get-info pair-data :high)
                              :high/24h (get-info pair-data :high/24h)
                              :open (get-info pair-data :open)))))
