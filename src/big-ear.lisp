(in-package #:cl-user)
(defpackage #:big-ear
  (:nicknames :be)
  (:use #:cl)
  (:export #:start
           #:get-pair-stream))
(in-package #:big-ear)

;; make it possible for drakma to get json response as text
;; unless it is already possible
(pushnew '("application" . "json")
         drakma:*text-content-types*
         :test 'equal)

(defparameter +uri+ "https://api.kraken.com/0/public/")
(defparameter request-pause-in-seconds 5)
(defparameter +kraken-pairs+ '(:xbtusd :xbteur :xbtgbp :xbtjpy ; btc
                               :ethxbt :ethusd :etheur :ethgbp ; eth
                               :ltcxbt :ltcusd :ltceur         ; ltc
                               :repxbt :repusd :repeur)        ; rep
  "needed for easy constuction of ticker URL. Also as a refference")

(defparameter +pairs+ '(:btcusd :btceur :btcgbp :btcjpy ; btc
                        :ethbtc :ethusd :etheur :ethgbp ; eth
                        :ltcbtc :ltcusd :ltceur         ; ltc
                        :repbtc :repusd :repeur))       ; rep

(defparameter +storage-directory+
  (asdf:system-relative-pathname :big-ear :storage/))

(defun storage-path (pair)
  "Returns the full path of the storage file"
  (string-downcase
   (format nil "~A~A.lisp" +storage-directory+ pair)))

(defun get-pair-stream (pair)
  "Load a file given a pair"
  (with-open-file (stream (storage-path pair)
                     :direction :input
                     :if-does-not-exist :error)
    stream))

(defstruct (ticker (:type list))
  pair timestamp ask bid last volume volume/24h volume-wa
  volume-wa/24h n-trades n-trades/24h low low/24h high high/24h open)

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun get-unix-time ()
  "Returns UNIX timestamp. Sound Waves and Alien Sea use it for compatibility with the underlying OS."
  (universal-to-unix-time (get-universal-time)))

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
  (let ((bindings '(;; btc
                    (:+xxbtzusd+ . :btcusd)
                    (:+xxbtzeur+ . :btceur)
                    (:+xxbtzgbp+ . :btcgbp)
                    (:+xxbtzjpy+ . :btcjpy)
                    ;; eth
                    (:+xethxxbt+ . :ethbtc) ; btc
                    (:+xethzusd+ . :ethusd) 
                    (:+xethzeur+ . :etheur)
                    (:+xethzgbp+ . :ethgbp)
                    (:+xethzjpy+ . :ethjpy)
                    ;; ltc
                    (:+xltcxxbt+ . :ltcbtc) ; btc
                    (:+xltczusd+ . :ltcusd)
                    (:+xltczeur+ . :ltceur)
                    ;; REP
                    (:+xrepxxbt+ . :repbtc) ; btc
                    (:+xrepzusd+ . :repusd)
                    (:+xrepzeur+ . :repeur))))
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

(defun request (uri)
  "Make an HTTP request"
  (handle-response (drakma:http-request uri)))

(defun ticker (pairs)
  "Ticker data"
  (let* ((full-url (format nil "~A~A~A" +uri+ "Ticker?pair=" pairs))
         (response (kraken->be (request full-url))))
    (loop for (pair . pair-data) in response
       collect (make-ticker
                :pair pair
                :timestamp (get-unix-time)
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

(defun list-to-comma-list (list)
  "Convert an ordinary list to a comma seprarated list as a string"
  (format nil "~{~A~^,~}" list))

(defun save-record-to-file (pair ticker-data)
  "Save a record to a file given a pair symbol"
  (with-open-file (s (storage-path pair)
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format s "~A~%" (rest (assoc pair ticker-data)))))

(defun start ()
  "Start fetching data"
  (loop
     (let ((ticker-data (ticker (list-to-comma-list +kraken-pairs+))))
       (mapcar #'(lambda (pair)
                   (save-record-to-file pair ticker-data))
               +pairs+)
       (print (get-unix-time))
     (sleep request-pause-in-seconds))))
