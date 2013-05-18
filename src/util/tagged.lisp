(defpackage :util.tagged
  (:use :common-lisp :util.util :local-time)
  (:export rebase outbase false-p
	   
	   datetime-p datetime make-datetime datetime-days datetime-secs datetime-tz
	   datetime-encode date-encode time-encode datetime-now datetime-today s
	   month-p weekday-p time-p date-p date&time-p iso8601/w iso8601/w-d iso8601/ym iso8601/ymd iso8601/hm
	   iso8601/ext iso8601/extz
	   chrono-number
	   ofMinute ofHour ofDay ofWeek ofMonth ofYear AD between earlier later
	   +local-time+ +server-tz+ +utc-tz+
	   +typed-true+ +typed-false+
	   +typed-pi+ +typed-half-pi+ +typed-three-halves-pi+ +typed-two-pi+

	   problem problem-p problem-trace problem-message problem-stack typed-problem?
	   
	   typed- typed+ typed* typed/ typed-floor typed-ceil typed-expt typed-log quadrant typed-cos typed-sin
	   typed-and typed-or typed-xor typed-not boolean-typed< typed< typed= boolean-typed=

	   typed-problem? typed-null? typed-number? typed-string? typed-datetime? typed-time? typed-date?
	   typed-date&time? typed-month? typed-weekday?

	   typed-str typed-esc parse-tagged-num fully-parse-tagged-num parse-tagged-date))

(in-package :util.tagged)

;; Actually this:
;; TZ 0 -- Printable Month = days 0 to 11, printable weekday = days 12 to 16. Thats it.
;; TZ 1 -- Local Time, Not in a zone.
;; TZ 2 -- UTC, Z, Server Time.
;; TZ 3 -- more timezones.

;; Timezones with no DATE cannot be added. Timezones (except 0) are stored normalized to UTC, they can be added, compared
;; and subtracted without conversion.
;;
;; ZERO seconds means 'whatever I'm comparing with', as does ZERO days. 

;; NUMBERS are fixed point, with 5 decimals of accuracy. Thats 100,000
(defconstant +typed-true+ 100000)
(defconstant +typed-false+ 0)
(defun bool->typed (bool) (if bool +typed-true+ +typed-false+))
(defun rebase (number) (truncate (* number +typed-true+)))
(defun outbase (number) (truncate number +typed-true+))
(defun false-p (thing) (or (null thing) (and (numberp thing) (zerop thing))))

(defconstant +chrono-name+ 0)
;;(defconstant +weekday-name+ 1)
(defconstant +local-time+ 1)
(defconstant +server-tz+ 2)
(defconstant +utc-tz+ 2)

(defconstant +seconds+ 000000)
(defconstant +minutes+ 100000)
(defconstant +hours+   200000)
(defconstant +days+    300000)
(defconstant +weeks+   400000)
(defconstant +months+  500000)
(defconstant +years+   600000)

(defstruct datetime (tz 0 :type (unsigned-byte 14)) (secs 0 :type (unsigned-byte 18)) (days 0 :type (unsigned-byte 32)))
;; I need a printing routine for this that respects timezones, language, and all that other jazz..

(defun datetime< (a b)
  (cond ((< (datetime-days a) (datetime-days b)) t)
	((> (datetime-days a) (datetime-days b)) nil)
	(t  (< (datetime-secs a) (datetime-secs b)))))

(defun datetime= (a b)
  (format *loop* "~%Compare TZ: ~a ~a" (datetime-tz a) (datetime-tz b))
  (and (or (= 1 (datetime-tz a)) (= 1 (datetime-tz b)) (= (datetime-tz a) (datetime-tz b)))
       (= (datetime-days a) (datetime-days b))
       (or (zerop (datetime-secs a)) (zerop (datetime-secs b)) (= (datetime-secs a) (datetime-secs b)))))

(defun datetime-encode (sec min hour day month year &optional tz)
  (declare (ignore tz))
  (let ((local (local-time:encode-timestamp 0 sec min hour day month year)))
    (make-datetime :tz +server-tz+ :days (1+ (local-time:day-of local)) :secs (1+ (local-time:sec-of local)))))

(defun date-encode (day month year &optional tz)
  (declare (ignore tz))
  (let ((local (local-time:encode-timestamp 0 0 0 0 day month year)))
    (make-datetime :tz +server-tz+ :days (1+ (local-time:day-of local)) :secs 0)))

(defun time-encode (sec min hour &optional tz)
  (declare (ignore tz))
  (let ((local (local-time:encode-timestamp 0 sec min hour 0 0 0)))
    (make-datetime :tz +server-tz+ :days 0 :secs (1+ (local-time:sec-of local)))))
  
;(defun datetime-today () (let ((lt (now))) (make-datetime :tz +server-tz+ :days (1+ (day-of lt)) :secs 0)))
;(defun datetime-now () (let ((lt (now))) (make-datetime :tz +server-tz+ :days (1+ (day-of lt)) :secs (1+ (sec-of lt)))))



(defun chrono-name-p (dt) (and (datetime-p dt) (= (datetime-tz dt) +chrono-name+)))
(defun month-p (dt) (and (chrono-name-p dt) (< (datetime-days dt) 12)))
(defun weekday-p (dt) (and (chrono-name-p dt) (>= (datetime-days dt) 12)))
(defun time-p (dt) (and (datetime-p dt) (not (month-p dt)) (zerop (datetime-days dt))))
(defun date-p (dt) (and (datetime-p dt) (not (month-p dt)) (not (zerop (datetime-days dt)))))
(defun date&time-p (dt) (and (datetime-p dt) (not (month-p dt)) (not (zerop (datetime-days dt)))
			     (not (zerop (datetime-secs dt)))))
(defun chrono-number (dt) 
  (unless (chrono-name-p dt) (error "~a is not a weekday or a month." dt)) ;(weekday-p dt))
  (rebase (if (< (datetime-days dt) 12)
	      (1+ (datetime-days dt))
	      (- (datetime-days dt) 11))))
      
(defun localtime+secs (dt)
  (cond ((= (datetime-tz dt) +chrono-name+) (error "Cannot perform datetime computations with month ~a" dt))
	;((= (datetime-tz dt) +weekday-name+) (error "Cannot perform datetime computations with weekday ~a" dt))
	;; I should put a clause in to handle conversion to SERVER timezone...
	(t (make-instance 'timestamp :day (max 0 (1- (datetime-days dt))) :sec (max 0 (1- (datetime-secs dt)))))))

(defun datetime-print (dt)
  (cond ((chrono-name-p dt)
	 (ecase (datetime-days dt)
	   (0 "January")
	   (1 "February")
	   (2 "March")
	   (3 "April")
	   (4 "May")
	   (5 "June")
	   (6 "July")
	   (7 "August")
	   (8 "September")
	   (9 "October")
	   (10 "November")
	   (11 "December")
	   (12 "Monday")
	   (13 "Tuesday")
	   (14 "Wednesday")
	   (15 "Thursday")
	   (16 "Friday")
	   (17 "Saturday")
	   (18 "Sunday")))
	((date&time-p dt) (format-timestring nil (localtime+secs dt) :format '((:year 4) "-" (:month 2) "-" (:day 2) " "
									       (:hour 2) ":" (:min 2) ":" (:sec 2))))
	((date-p dt) (format-timestring nil (localtime+secs dt) :format '((:year 4) "-" (:month 2) "-" (:day 2))))
	
	((time-p dt) (format-timestring nil (localtime+secs dt) :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))))


;; ****************************************
;; ** My own LOCAL-TIME reimplementation **
;; ****************************************

(eval-when (:compile-toplevel :load-toplevel :execute)                 ;; V -- Jan, Feb
  (defparameter +rot-month-days+  (coerce #(31 30 31 30 31 31 30 31 30 31 31 28) '(simple-array fixnum (*))))
  (defparameter +rot-month-startday+
    (coerce (cons 0 (loop with sum = 0 for d across +rot-month-days+ collect (incf sum d))) '(simple-array fixnum (*))))

  (defun years-to-days (years)
    "Given a number of years, returns the number of days in those years."
    (let* ((days (* years 365))
           (l1 (floor years 4))
           (l2 (floor years 100))
           (l3 (floor years 400)))
      (+ days l1 (- l2) l3))))

(defun days-to-years (days)
  (multiple-value-bind (400-years days) (floor days #.(years-to-days 400))
    (let* ((100-years (min (floor days #.(years-to-days 100)) 3))
	   (days (- days (* 100-years #.(years-to-days 100)))))
      (multiple-value-bind (4-years days) (floor days #.(years-to-days 4))
	(let ((years (min 3 (floor days #.(years-to-days 1)))))
	  (values (+ years (* 4-years 4) (* 100-years 100) (* 400-years 400))
		  (- days (* years 365))))))))

(defun weekday (day) (mod (+ day 2) 7)) ;; 0mon, 1tue, 2wed, 3thur, 4fri, 5sat, 6sun

(flet ((y+m-map (year month) (if (>= month 3) (values (- year 2000) (- month 3)) (values (- year 2001) (+ month 9)))))
  (defun epoch-days/m (year month)
    (multiple-value-bind (year month) (y+m-map year month)
      (+ (aref +rot-month-startday+ month) (years-to-days year))))
  (defun epoch-days/y (year)
    ;; Compute the day, then subtract out 2000 jan and feb. Then if a leap year, subtract out the extra day because it
    ;; wouldn't have occured yet at the start of the year.
    (max (- (years-to-days (- year 2000)) 31 28 (if (zerop (mod year 4)) 1 0)) 0))
  (defun epoch-days/w (year week)
    (let ((day (epoch-days/y year)))
      ;;(format *loop* "~%year d1: ~a" day) ;; Hmmm... 
      (incf day (if (<= (weekday day) 3) (- 3 (weekday day)) (- 10 (weekday day))))
      ;;(format *loop* ", first thur: ~a" day)
      (+ day -3 (* week 7)))))  ;; Move to monday, and add in the appropriate weeks.
  


;; Returns # of epoch-days for a given year/month/day.
(defun ymd (year month day)
  (when (or (< year 2000) (> month 12) (and (= year 2000) (< month 3)))
    (error "Can't represent dates before 2000/March/01."))
  (+ (1- day) (epoch-days/m year month)))
(defun yo (year days) (+ (1- days) (epoch-days/y year)))
(defun ywd (year weeks day) ;; Weeks is 1-53
  ;;(format *loop* "~%yw: ~a ~a" year weeks)
  (+ (1- day) (epoch-days/w year (1- weeks))))

(defun s (seconds-from-march-3-2000)
  (multiple-value-bind (day second) (floor seconds-from-march-3-2000 local-time:+seconds-per-day+)
    (make-datetime :tz +local-time+ :days (1+ day) :secs (1+ second))))
(defun datetime-now () (let ((seconds (- (get-universal-time) #.(encode-universal-time 0 0 0 1 3 2000 0))))
			 (decf seconds (* 7 +minutes-per-hour+ +seconds-per-minute+))
			 (let ((dt (s seconds)))
			   (setf (datetime-tz dt) +server-tz+) dt)))
(defun datetime-today () (let ((dt (datetime-now))) (setf (datetime-secs dt) 0) dt))

(flet ((days->year (day) 
	 (multiple-value-bind (years remaining) (days-to-years day); (1- (datetime-days dt)))
	   ;; If Remaining days falls somewhere within first 10 rotated months
	   (values (+ years (if (find remaining +rot-month-startday+ :test #'< :end 11) 2000 2001))
		   remaining))))
  (defun datetime-year (dt) (days->year (1- (datetime-days dt))))
  (defun datetime-week (dt)
    (let ((day (1- (datetime-days dt))))
      (incf day (- 3 (weekday day))) ;; Mov to nearest thursday.
      (let ((year (days->year day))) 
	(values (1+ (truncate (- day (ymd year 1 1)) 7)) year))))
  (defun datetime-month (dt)  ;; months (1-12), year, day
    (multiple-value-bind (year days) (datetime-year dt)
      (let ((mpos (or (position days +rot-month-startday+ :test #'<) 13)))
	(values (if (>= mpos 11) (- mpos 10) (+ mpos 2)) year (1+ (- days (elt +rot-month-startday+ (1- mpos)))))))))
      
(defun year-difference (a b)
  (multiple-value-bind (yeara daya) (datetime-year a)
    (multiple-value-bind (yearb dayb) (datetime-year b)
      (if (or (> dayb daya) (> (datetime-secs b) (datetime-secs b)))
	  (- yeara yearb 1)
	  (- yeara yearb)))))

(defun iso8601/w (out dt)
  (multiple-value-bind (week year) (datetime-week dt)
    (format out "~4,'0d-W~2,'0d" year week)))
(defun iso8601/w-d (out dt)
  (iso8601/w out dt)
  (let ((wd (weekday (1- (datetime-days dt))))) (or (zerop wd) (format out "-~d" (1+ wd)))))
	
;(defun iso8601/wt (dt)
;  nil)
;(defun iso8601/wtz (dt)
;  nil)

(defun iso8601/ym (out dt) (multiple-value-bind (m year) (datetime-month dt) (format out "~4,'0d-~2,'0d" year m)))
(defun iso8601/ymd (out dt) (multiple-value-bind (m year d) (datetime-month dt)
			      (format out "~4,'0d-~2,'0d-~2,'0d" year m d)))

(defun iso8601/hm (out dt) 
  (multiple-value-bind (h r) (truncate (datetime-secs dt) local-time:+seconds-per-hour+)
    (format out "~2,'0d:~2,'0d" h (truncate r local-time:+seconds-per-minute+))))
(defun iso8601/ext (out dt) (iso8601/ymd out dt) (princ #\T out) (iso8601/hm out dt))
(defun iso8601/extz (out dt) (iso8601/ext out dt) (when (= +utc-tz+ (datetime-tz dt)) (princ #\Z out)))


(defun ofMinute (time period) (declare (ignore time period)) (error "ofMinute not implemented."))
(defun ofHour (time period) (declare (ignore time period)) (error "ofHour not implemented."))
(defun ofDay (time period) (declare (ignore time period)) (error "ofDay not implemented."))

;; Now must have dates in order to process..
(defun ofWeek (time period)
  (or (date-p time) (error "~a is not a date." time))
  (ncase period
    (+seconds+ (error "Second of Week not implemented."))
    (+minutes+ (error "Minute of Week not implemented."))
    (+hours+   (error "Hour of Week not implemented."))
    (+days+    (make-datetime :tz +chrono-name+ :days (+ 12 (weekday (1- (datetime-days time)))) :secs 0))
    (otherwise (error "ofWeek expects a period of 0=Seconds, 1=Minutes, 2=Hour or 3=Day but got ~d instead." period))))

(defun ofMonth (time period)
  (or (date-p time) (error "~a is not a date." time))
  (ncase period
    (+seconds+ (error "Second of Month not implemented."))
    (+minutes+ (error "Minute of Month not implemented."))
    (+hours+   (error "Hour of Month not implemented."))
    (+days+    (rebase (nth-value 2 (datetime-month time))))
	       ;; (make-datetime :tz +chrono-name+ :days (+ 12 (weekday (1- (datetime-days time)))) :secs 0))
    (otherwise (error "ofWeek expects a period of 0=Seconds, 1=Minutes, 2=Hour or 3=Day but got ~d instead." period))))

(defun ofYear (time period)
  (cond ((not (date-p time)) (error "~a is not a date." time))
	((eq +seconds+ period) (error "Second of Year not implemented."))
	((eq +minutes+ period) (error "Minute of Year not implemented."))
	((eq +hours+   period) (error "Hour of Year not implemented."))
	((eq +days+    period) (error "Day of Year not implemented."))
	((eq +weeks+   period) (rebase (datetime-week time)));(error "Week of Year not implemented."))
	((eq +months+  period) (make-datetime :tz +chrono-name+ :days (1- (timestamp-month (localtime+secs time))) :secs 0))
	(t (error "ofYear expects a period of 0=Seconds, 1=Minutes, 2=Hour, 3=Day, 4=Week or 5=Month but got ~d instead."
		  period))))

(defun ad (time period)
  (cond ((not (date-p time)) (error "~a is not a date." time))
	((eq period +seconds+) (error "Seconds AD not implemented."))
	((eq period +minutes+) (error "Minutes AD not implemented."))
	((eq period   +hours+) (error "Hours AD not implemented."))
	((eq period    +days+) (error "Days AD not implemented."))
	((eq period   +weeks+) (error "Weeks AD not implemented."))
	((eq period  +months+) (error "Months AD not implemented."))
	((eq period   +years+) (rebase (timestamp-year (localtime+secs time))))
	(t(error "AD expects a period of 0=Seconds, 1=Minutes, 2=Hour, 3=Day, 4=Week, 5=Month or 6=years but got ~d instead."
		 period))))

(defun between (then now period)
  ;; Should work with times... and dates... 
  (cond ((not (datetime-p then)) (error "between then value ~a is not a date or time (was a ~a)." then (type-of then)))
	((not (datetime-p now))  (error "between now ~a is not a date or time (was a ~a)." now (type-of now)))
	((and (>= period +days+) (time-p then)) (error "~a is not a date." then))
	((and (>= period +days+) (time-p now)) (error "~a is not a date." now))
	((eq period +seconds+) (rebase (timestamp-difference (localtime+secs now) (localtime+secs then))))
	((eq period +minutes+)
	 (rebase (truncate (timestamp-difference (localtime+secs now) (localtime+secs then)) +seconds-per-minute+)))
	((eq period   +hours+)
	 (rebase (truncate(timestamp-difference(localtime+secs now)(localtime+secs then)) +seconds-per-hour+)))
	((eq period    +days+)
	 (rebase (truncate(timestamp-difference(localtime+secs now)(localtime+secs then)) +seconds-per-day+)))
	((eq period   +weeks+) (rebase (truncate (timestamp-difference (localtime+secs now) (localtime+secs then))
						 (* +seconds-per-day+ +days-per-week+))))
	((eq period  +months+) (rebase (truncate (local-time:timestamp-difference (localtime+secs now) (localtime+secs then))
						 (* 30 +seconds-per-day+))))
	((eq period +years+) (rebase (timestamp-whole-year-difference (localtime+secs now) (localtime+secs then))))
	(t (error "between expects a period of 0=Seconds, 1=Minutes, 2=Hour, 3=Day, 4=Week, 5=Month or 6=year but got ~d instead."  period))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problem
  (trace nil :read-only t :type (simple-array (unsigned-byte 32) (5)))
  (message nil :read-only t :type string)
  (stack nil :read-only t :type cons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun typed- (a b) (- a b))
(defun typed+ (a b) (+ a b))
(defun typed* (a b) (outbase (* a b)))
(defun typed/ (a b) 
  ;(format *loop* "~%Typed ~a/~a --> ~a" (typed-str a) (typed-str b) (unless (zerop b) (typed-str (truncate a (outbase b)))))
  (truncate a (outbase b)))
(defun typed-floor (a b) (rebase (floor a b)))
(defun typed-ceil  (a b) (rebase (ceiling a b)))
(defun typed-expt (a b) (rebase (expt (outbase a) (outbase b))))
(defun typed-log  (a b) (rebase (log  (outbase a) (outbase b))))

(defconstant +typed-pi+ 314158)
(defconstant +typed-half-pi+ (truncate +typed-pi+ 2))
(defconstant +typed-three-halves-pi+ (* 3 +typed-half-pi+))
(defconstant +typed-two-pi+  (* +typed-pi+ 2))
(defconstant +costable+ 
;; 2000 entries over the range (0, pi/2] (plus a bonus one (pi/2) at the end for linear interpolation)
  (make-array 2001 :element-type 'fixnum
	      :initial-contents
	      (loop for i from 0 to 2000
		    with fragment = (/ pi 2 2000)
		    collecting (rebase (cos (* fragment i))))))
(defun 100linear (amount v1 v2) (truncate (+ (* amount v1) (* (- 100 amount) v2)) 100))
(defun trig-val (value)
  (assert (< -1 value +typed-half-pi+))
  (multiple-value-bind (index remainder) (truncate (* value 2000) +typed-half-pi+)
    ;;(format t "~%LOOK AT: ~a (remainder: ~d) (value: ~d)" index remainder value)
    (100linear (- 100 (truncate (* 100 remainder) +typed-half-pi+)) (elt +costable+ index) (elt +costable+ (1+ index)))))

;;  1__|__0
;;  2  |  3
(defun quadrant (value)
  ;; How do I know what quadrant I'm in? Keep mod 2pi... and look.
  (let ((remainder (mod value +typed-two-pi+)))
    (cond ((< remainder +typed-half-pi+)            0)
	  ((< remainder +typed-pi+)                 1)
	  ((< remainder +typed-three-halves-pi+)    2)
	  (t                                        3))))
(defun typed-cos (angle &aux (value (mod angle +typed-half-pi+))) ;; X component.
  ;;(format *loop* "~%Q~d: ~d" (quadrant angle) value)
  (ecase (quadrant angle)
    (0 (trig-val value))
    (1 (- (trig-val (- +typed-half-pi+ 1 value))))
    (2 (- (trig-val value))) 
    (3 (trig-val (- +typed-half-pi+ 1 value)))))
(defun typed-sin (angle &aux (value (mod angle +typed-half-pi+)))
  (ecase (quadrant angle)
    (0 (trig-val (- +typed-half-pi+ 1 value)))
    (1 (trig-val value))
    (2 (- (trig-val (- +typed-half-pi+ 1 value))))
    (3 (- (trig-val value)))))

(defun typed-and (a b) (if (not (false-p a))
			   (if (not (false-p b)) b +typed-false+)
			   +typed-false+))
(defun typed-or  (a b) (if (not (false-p a)) a (if (not (false-p b)) b +typed-false+)))
(defun typed-xor (a b) (if (false-p a)
			   (if (not (false-p b)) b +typed-false+)
			   (if (not (false-p b)) +typed-false+ a)))
(defun typed-not (a) (bool->typed (false-p a)))

(defun boolean-typed< (a b)
  (cond ((problem-p a) (not (problem-p b)))
	((problem-p b) nil)
	((null a) (not (null b)))
	((null b) nil)
	((numberp a) (or (not (numberp b)) (< a b)))
	((numberp b) nil)
	((stringp a) (or (not (stringp b)) (string-lessp a b)))
	((stringp b) nil)
	((datetime-p a) (datetime< a b))))

(defun typed< (a b) (bool->typed (boolean-typed< a b)))

(defun boolean-typed= (a b)
  (cond ((problem-p a) (problem-p b))
	((problem-p b) nil)
	((null a) (null b))
	((null b) nil)
	((numberp a) (and (numberp b) (= a b)))
	((numberp b) nil)
	((stringp a) (and (stringp b) (string= a b)))
	((stringp b) nil)
	((datetime-p a) (datetime= a b))))

(defun typed= (a b) (bool->typed (boolean-typed= a b)))

(defun typed-problem? (a) (if (problem-p a) +typed-true+ +typed-false+))
(defun typed-null? (a) (if (null a) +typed-true+ +typed-false+))
(defun typed-number? (a) (if (numberp a) +typed-true+ +typed-false+))
(defun typed-string? (a) (if (stringp a) +typed-true+ +typed-false+))

(defun typed-datetime? (a) (if (datetime-p a) +typed-true+ +typed-false+))
(defun typed-time? (a) (if (time-p a) +typed-true+ +typed-false+))
(defun typed-date? (a) (if (date-p a) +typed-true+ +typed-false+))
(defun typed-date&time? (a) (if (date&time-p a) +typed-true+ +typed-false+))
(defun typed-month? (a) (if (month-p a) +typed-true+ +typed-false+))
(defun typed-weekday? (a) (if (weekday-p a) +typed-true+ +typed-false+))


(defun print-number (a) 
  (multiple-value-bind (n remainder) (truncate a +typed-true+)
    (if (zerop remainder)
	(format nil "~d" n)
	(string-right-trim '(#\0) (format nil "~d.~5,'0d" n (abs remainder))))))

(defun typed-str (a)
  (cond ((problem-p a) "ERR")
	((null a) "")
	((numberp a) (print-number a))
	((stringp a) a)
	((datetime-p a) (datetime-print a))))

(defun typed-esc (a)
  (cond ((problem-p a) "ERR")
	((null a) "NIL")
	((numberp a) (print-number a))
	((stringp a) a)
	((datetime-p a) (datetime-print a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(flet ((finalize (negative-p numbers deci-point)
	 (setf deci-point (or deci-point (length numbers))
	       numbers (nconc (make-list (max 0 (- (+ deci-point 5) (length numbers))) :initial-element #\0)
			      (nthcdr    (max 0 (- (length numbers) (+ deci-point 5))) numbers)))
	 ;;(format *loop* "~%FINALIZE ~a ~a" numbers negative-p)
	 (loop for n in numbers
	       for pow = 1 then (* 10 pow)
	       sum (* pow (- (char-code n) (char-code #\0))) into accum
	       finally (return (ignore-errors (coerce (if negative-p (- 0 accum) accum) '(signed-byte 64)))))))
  (defun parse-tagged-num (str &optional (start 0) (end (length str)) german-p)
    (assert (not (< end start)))
    (let ((negative-p) (numbers)(symbols)(realend start))
      (loop for i from start to (1- end)
	    ;do (setf realend i) 
	    ;while (< start end)
	    ;do (format *loop* "~%GO: ~a" i)
	    do (cond ((member (elt str i) '(#\- #\+ #\. #\, #\` ) :test #'char-equal)
		      (when (and (char= (elt str i) #\-) (or (member #\- symbols :key #'car) numbers)) (return))
		      (incf realend)
		      (push (cons (elt str i) (length numbers)) symbols))
		     ((digit-char-p (elt str i)) (incf realend) (push (elt str i) numbers))
		     ((or (char-equal (elt str i) #\$)
			  (string-equal "CAD" str :start2 i :end2 (min end (+ i 3)))
			  (string-equal "USD" str :start2 i :end2 (min end (+ i 3)))
			  (string-equal "GBP" str :start2 i :end2 (min end (+ i 3))))
		      (setf german-p nil)
		      (when numbers (return)))
		     ((or (string-equal "EUR" str :start2 i :end2 (min end (+ i 3))))
		      (setf german-p t)
		      (when numbers (return)))
		     (t (return))));; (when numbers (return)))))
      ;;(format *loop* "~%DONE: ~a" realend)
      (awhen (assoc #\- symbols) (when (zerop (cdr it)) (setf negative-p t)))
      (values (and numbers (finalize negative-p numbers
				     (cond ((and german-p (<= (count #\, symbols :key #'car) 1)) (cdr (assoc #\, symbols)))
					   ((and german-p (= (count #\` symbols :key #'car) 1)) (cdr (assoc #\` symbols)))
					   ((<= (count #\. symbols :key #'car) 1) (cdr (assoc #\. symbols)))
					   ((<= (count #\, symbols :key #'car) 1) (cdr (assoc #\, symbols)))
					   (t (return-from parse-tagged-num (values nil 0))))))
	      (- realend start)))))
      
(defun fully-parse-tagged-num (str)
  (multiple-value-bind (result consumed) (parse-tagged-num str)
    (when (= (length str) consumed) result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parsing times is a son of a bitch, especially because I wanted as much flexibility as possible.
;; Basically it occurs in three steps:  A parser extracts data and seperators. 
;; They are bound to *data* and *hint*, and then the time extractor works.
;; The time extractor works on common time patterns, and when its done, it has destructively removed that data
;; and hints from *data* and *hint*.  Finally, date extraction happens on what is left, which should be between
;; 0 and 3 numbers/months/days.  Its operation is messy hackish code that embodies some rules of thumb. All three
;; stages operate on the temporary time object held in *dt*, which is then returned.

(macrolet ((month (month &rest names)
	     (let ((acc nil))
	       (loop for name in (sort (copy-list names) #'< :key #'length)
		  with size = 0
		  do (if (= size (setf size (length name)))
			 (push `(string= string ,name) (car acc))
			 (push (list `(string= string ,name) size) acc))
		  finally (map-into acc #'nreverse acc))
	       `(progn (defun ,(intern (format nil "~a-P" month)) (string)
			 (case (length string)
			   ,@(mapcar #'(lambda (group) `(,(car group) (and (or ,@(cdr group)) ,(car group)))) acc)))
		       (declaim (inline ,(intern (format nil "~a-P" month)))))))
	   (day (day &rest names) `(month ,day ,@names))
	   (tz  (tz &rest abbrevs) `(month ,tz ,@abbrevs))
	   (period (name &rest abbrevs) `(month ,name ,@abbrevs)))
  (month january "january" "januar" "janvier" "jan")
  (month february "february" "februar" "fevrier" "février" "feb" "fév" "fev")
  (month march  "march" "märz" "marz" "mars" "mar" "mär")
  (month april "april" "avril" "apr" "avr")
  (month may "may" "mai")
  (month june "june" "juni" "juin"  "jui" "jun")
  (month july "july" "juli" "juillet" "jui" "jul")
  (month august "august" "août" "aout" "aug" "aoû" "aou")
  (month september  "september" "septembre" "sep" "sept")
  (month october  "october" "oktober" "octobre" "oct" "okt")
  (month november "november" "novembre" "nov")
  (month december  "december" "dezember" "décembre" "dec" "déc" "dez")
  (day monday "monday" "mon")
  (day tuesday "tuesday" "tue")
  (day wednesday "wednesday" "wed")
  (day thursday "thursday" "thur")
  (day friday "friday" "fri")
  (day saturday "saturday" "sat")
  (day sunday "sunday" "sun")
  (tz utc "z" "utc")
  (tz mdt "mdt" "har")
  (tz mst "mst" "hnr")
  (period am "am" "midnight" "a.m." "a.m")
  (period pm "pm" "noon" "p.m." "p.m"))

(defvar *data*)
(defvar *hint*)
(defvar *dt*)
(defvar *offset*) ;; Can't be applied immediately until I have computed DAYS... so I just store it.
(flet ((parse-compressed-time ()
	 ;;(format *loop* "~%Pcompressed: I've got: ~a~%~a" *data* *hint*)
	 (when (and (not (eq (cadr *hint*) #\:))  (eq (caadr *data*) 'number))
	   (let* ((d (concatenate 'string (cdadr *data*) (make-string (- 6 (length (cdadr *data*))) :initial-element #\0))))
	     (setf (datetime-secs *dt*) 1) ;; Time is valid!
	     (incf (datetime-secs *dt*) (* (parse-integer d :start 0 :end 2) local-time:+seconds-per-hour+))
	     (incf (datetime-secs *dt*) (* (parse-integer d :start 2 :end 4) local-time:+seconds-per-minute+))
	     (incf (datetime-secs *dt*) (parse-integer d :start 4 :end 6))
	     ;;(format *loop* "~%~d hours to output." (subseq d 0 2))
	     ;;(format *loop* "~%~d mins to output." (subseq d 2 4))
	     ;;(format *loop* "~%~d secs to output." (subseq d 4 6))
	     (rplacd *data* (cddr *data*))
	     (rplacd *hint* (cddr *hint*))
	     t)))
       (parse-extended-time ()
	 ;;(format *loop* "~%Pextended: I've got: ~a~%~s" *data* *hint*)
	 (unless (and (eq (caadr *data*) 'number) (<= (length (cdadr *data*)) 2) (<= 0 (parse-integer (cdadr *data*)) 23))
	   (error "Could not make sense of hours..."))
	 (setf (datetime-secs *dt*) 1) ;; Time is valid!
	 (incf (datetime-secs *dt*) (* (parse-integer (cdadr *data*)) local-time:+seconds-per-hour+)) ;; Add hours... 
	 ;;(format *loop* "~%ADD ~d hours to output." (cdadr *data*))
	 (rplacd *data* (cddr *data*))
	 (when (eq (cadr *hint*) #\:)
	   (unless (and (eq (caadr *data*) 'number) (<= (length (cdadr *data*)) 2)
			(<= 0 (parse-integer (cdadr *data*)) 59))
	     (error "Could not make sense of minutes..."))
	   (incf (datetime-secs *dt*) (* (parse-integer (cdadr *data*)) local-time:+seconds-per-minute+)) ;; Add minutes
	   ;;(format *loop* "~%ADD ~d minutes to output." (cdadr *data*))
	   (rplacd *data* (cddr *data*))
	   (rplacd *hint* (cddr *hint*))
	   (when (eq (cadr *hint*) #\:)
	     (unless (and (eq (caadr *data*) 'number) (<= (length (cdadr *data*)) 2)
			  (<= 0 (parse-integer (cdadr *data*)) 60))
	       (error "Could not make sense of seconds"))
	     (incf (datetime-secs *dt*) (parse-integer (cdadr *data*))) ;; Add seconds to output...
	     ;;(format *loop* "~%ADD ~d seconds to output." (cdadr *data*))
	     (rplacd *data* (cddr *data*))
	     (rplacd *hint* (cddr *hint*)))))
       (parse-pm ()
	 (when (eq (caadr *data*) 'period)
	   ;; roll back 12 to zero hour.
	   (when (and (<= (* 12 local-time:+seconds-per-hour+) (1- (datetime-secs *dt*)))
		      (< (1- (datetime-secs *dt*)) (* 13 local-time:+seconds-per-hour+)))
	     (decf (datetime-secs *dt*) (* 12 local-time:+seconds-per-hour+))) ;; Roll back to zero hour.
	   (when (eq (cdadr *data*) 'pm)
	     ;;(format *loop* "~%Bump hours to the next 12...") ;; Add 12 hours
	     (incf (datetime-secs *dt*) (* 12 local-time:+seconds-per-hour+)))
	   (rplacd *data* (cddr *data*))
	   (rplacd *hint* (cddr *hint*))))
       (parse-timezone ()
	 (when (member (cadr *hint*) '(#\+ #\-))
	   (cond ((not (eq (caadr *data*) 'number)) (error "Could not make sense of time offset"))
		 ((eq (caddr *hint*) #\:)
		  (unless (and (eq (caaddr *data*) 'number)
			       (<= 0 (parse-integer (cdadr *data*)) 12)
			       (<= 0 (parse-integer (cdaddr *data*)) 59))
		    (error "Could not make sense of time offset"))
		  ;;(format *loop* "~%time offset ~a ~s : ~s" (cadr *hint*) (cdadr *data*) (cdaddr *data*))
		  (setf *offset* (+ (* (parse-integer (cdadr *data*)) local-time:+seconds-per-hour+)
				    (* (parse-integer (cdaddr *data*)) local-time:+seconds-per-minute+)))
		  (when (eq (cadr *hint*) #\-) (setf *offset* (- 0 *offset*)))
		  (rplacd *data* (cdddr *data*))
		  (rplacd *hint* (cdddr *hint*)))
		 ((<= (length (cdadr *data*)) 2)
		  ;;(format *loop* "~%time offset ~a ~s : 0" (cadr *hint*) (cdadr *data*))
		  (setf *offset* (* (parse-integer (cdadr *data*)) local-time:+seconds-per-hour+))
		  (when (eq (cadr *hint*) #\-) (setf *offset* (- 0 *offset*)))
		  (rplacd *hint* (cddr *hint*))
		  (rplacd *data* (cddr *data*)))
		 ((= (length (cdadr *data*)) 4)
		  ;;(format *loop* "~%time offset ~a ~s : ~s" (cadr *hint*)
		  ;;	  (subseq (cdadr *data*) 0 2) (subseq (cdadr *data*) 2 4))
		  (setf *offset* (+ (* (parse-integer (cdadr *data*) :start 0 :end 2) local-time:+seconds-per-hour+)
				    (* (parse-integer (cdadr *data*) :start 2 :end 4) local-time:+seconds-per-minute+)))
		  (when (eq (cadr *hint*) #\-) (setf *offset* (- 0 *offset*)))
		  (rplacd *hint* (cddr *hint*))
		  (rplacd *data* (cddr *data*)))
		 (t (error "Could not make sense of time offset."))))))
  (defun finalize-time ()
    (aif (position #\T *hint* :test #'char-equal :start 1)
	 (progn (let ((*data* (nthcdr it *data*))
		      (*hint* (nthcdr it *hint*)))
		  (declare (special *data* *hint*))
		  (or (parse-compressed-time)
		      (parse-extended-time))
		  (parse-pm) (parse-timezone))
		(rplacd (nthcdr (1- it) *hint*) (nthcdr (1+ it) *hint*)))
	 (aif (position #\: *hint* :test #'char-equal :start 1)
	      (progn (let ((*data* (nthcdr (- it 1) *data*))
			   (*hint* (nthcdr (- it 1) *hint*)))
		       (declare (special *data* *hint*))
		       (parse-extended-time)
		       (parse-pm) (parse-timezone))
		     (rplacd (nthcdr (1- it) *hint*) (nthcdr (1+ it) *hint*)))
	      (awhen (position 'period *data* :key #'car :start 1)
		(let ((*data* (nthcdr (- it 2) *data*))
		      (*hint* (nthcdr (- it 2) *hint*)))
		  (declare (special *data* *hint*))
		  (parse-extended-time)
		  (parse-pm) (parse-timezone))
		(rplacd (nthcdr (- it 2) *hint*) (nthcdr it *hint*)))))))

(flet ((set-zone (zone)
	 ;;(format *loop* "~%SET THE ZONE TO: ~a" zone)
	 (ecase zone
	   (utc (setf (datetime-tz *dt*) +utc-tz+)))))
  (defun finalize-zone ()
    (awhen (position 'zone *data* :test #'eq :key #'car :start 1)
      (when (position 'zone *data* :test #'eq :key #'car :start (1+ it))
	(error "Couldn't parse time, two zones specified."))
      (set-zone (cdr (elt *data* it)))
      (awhen (nthcdr (1- it) *hint*) (rplacd it (cddr it)))
      (awhen (nthcdr (1- it) *data*) (rplacd it (cddr it))))))

(defun this-month () (nth-value 4 (decode-universal-time (get-universal-time))))
(defun this-year  () (nth-value 5 (decode-universal-time (get-universal-time))))

(flet ((finalize-week (pos)
	 (destructuring-bind ((beftype . befdata) (afttype . aftdata) &rest r) (nthcdr pos *data*)
	   (let ((daytype (caar r)) (daydata (cdar r)))
	     (cond ((and (eq afttype 'number) (<= (length aftdata) 2)
			 (eq beftype 'number) (>= 4 (length befdata) 2))
		    ;;(format *loop* "~%WEEK ~a year ~a" aftdata befdata)
		    (setf (datetime-days *dt*) (1+ (ywd (parse-integer befdata) (parse-integer aftdata)
							    (or (and (eq 'number daytype) (parse-integer daydata)) 1)))))
		   ((and (eq afttype 'number) (= (length aftdata) 4))
		    ;;(format *loop* "~%WEEK ~a of year ~a" (concatenate 'string "20" (subseq aftdata 2 4))
		    ;;	  (subseq aftdata 0 2))
		    (setf (datetime-days *dt*) (1+ (ywd (+ 2000 (parse-integer aftdata :start 0 :end 2))
							    (parse-integer aftdata :start 2 :end 4)
							    (or (and (eq 'number daytype) (parse-integer daydata)) 1)))))
		   ((and (eq afttype 'number) (= (length aftdata) 2))
		    ;;(format *loop* "~%WEEK ~a of this year." aftdata)
		    (setf (datetime-days *dt*) (1+ (ywd (this-year) (parse-integer aftdata)
							    (or (and (eq 'number daytype) (parse-integer daydata)) 1)))))
		   (t (error "Could not understand week ~aW~a." befdata aftdata)))))))
  (defun finalize-day (endian) 
    (let ((checkday nil)
	  (daypos nil)
	  (monthpos nil))
      (awhen (position 'day *data* :key #'car :start 1)
	;;(format *loop* "~%Extracted day... I should guess endianness and checksum it.")
	(setf daypos it)
	(setf checkday (cdr (elt *data* it)))
	(rplacd (nthcdr (1- it) *data*) (nthcdr (1+ it) *data*))
	(rplacd (nthcdr it *hint*) (nthcdr (1+ it) *hint*)))

      (awhen (position #\w *hint* :test #'char-equal :start 1)
	(finalize-week it)
	(return-from finalize-day))

      (awhen (position 'month *data* :key #'car :start 1)
	;;(format *loop* "~%Found month: ~a" it)
	;;(format *loop* "~%Found month... I should guess endianness then turn it to a number.")
	(setf (car (elt *data* it)) 'number)
	(setf monthpos it))

      (let ((n (let* ((n1 (position 'number *data* :start 1 :key #'car))
		      (n2 (and n1 (position 'number *data* :start (1+ n1) :key #'car)))
		      (n3 (and n2 (position 'number *data* :start (1+ n2) :key #'car))))
		 (list n1 n2 n3))))
	(symbol-macrolet ((n1 (first n)) (n2 (second n)) (n3 (third n)))
	  (when monthpos (cond ((eq n1 monthpos) (setf monthpos 0))
			       ((eq n2 monthpos) (setf monthpos 1))
			       ((eq n3 monthpos) (setf monthpos 2))))
	  (when daypos (cond ((eq n1 daypos) (setf daypos 0))
			     ((eq n2 daypos) (setf daypos 1))
			     ((eq n3 daypos) (setf daypos 2))))
	  (and n1 (setf n1 (cdr (elt *data* n1))))
	  (and n2 (setf n2 (cdr (elt *data* n2))))
	  (and n3 (setf n3 (cdr (elt *data* n3))))
	
	  ;; Handle ORDINALS.
	  (when (and n1 (null n2) (null n3) (null monthpos) (or (= (length n1) 3) (= (length n1) 7))) ;; Probably a day.
	    ;;(format *loop* "~%Set year to current and day to: ~a" n1)
	    (return-from finalize-day))
	  (when (and n2 (null n3) (null monthpos) (= (length n2) 3) (or (= (length n1) 2) (= (length n1) 4)))
	    ;;(format *loop* "~%Set to year ~a and day ~a" n1 n2)
	    (when (= (length n1) 2) (setf n1 (concatenate 'string "20" n1)))
	    (setf (datetime-days *dt*) (1+ (yo (parse-integer n1) (parse-integer n2))))
	    (return-from finalize-day))

	  (when (and monthpos (eq monthpos daypos)) (setf daypos (mod (1+ daypos) 3)))
	  ;;(format *loop* "~%BEFORE DATE PARSE: ~a ~a ~a / ~a, ~a" n1 n2 n3 monthpos daypos)
	  (cond ;; 3 items 
	    ((and n3 (= (length n1) 4)) nil)
	    ((and n3 (= (length n3) 4) (or (eq endian 'little) (eq daypos 0)))  (let ((dt n1)) (setf n1 n3 n3 dt)))
	    ((and n3 (= (length n3) 4)) (let ((mt n1) (dt n2)) (setf n1 n3 n2 mt n3 dt)))
	    ((and n3 (eq endian 'big)) nil)
	    ((and n3 (eq endian 'mid) (or (null monthpos) (eq monthpos 0))) (rotatef n3 n2 n1))
	    (n3 (rotatef n1 n3))
	    ;; 2 items only.
	    ((and monthpos daypos) (let ((mt (elt n monthpos)) (dt (elt n daypos))) (setf n1 "2010" n2 mt n3 dt)))
	    ((and n2 (= (length n1) 4)) (setf n3 "01"))
	    ((and n2 (= (length n2) 4)) (let ((mt n1)) (setf n1 n2 n2 mt n3 "01")))
	    ((and n2 (or (eq endian 'big) (eq endian 'mid))) (setf n3 n2 n2 n1 n1 "2010"))
	    (n2 (setf n3 n1 n1 "2010"))
	    ;; 1 item only
	    ((= (length n1) 8) (setf n3 (subseq n1 6 8) n2 (subseq n1 4 6) n1 (subseq n1 0 4)))
	    (monthpos (setf n2 (elt n monthpos) n1 "2010" n3 "01"))
	    (daypos (setf n3 (elt n daypos) n1 "2010" n2 (format nil "~2,'0d" (this-month))))
	    (n1 (setf n2 "01" n3 "01")) ;; Assume year
	    (t (return-from finalize-day)))
	  (when (= (length n1) 2) (setf n1 (concatenate 'string "20" n1)))
	  ;;(format *loop* "~%SET YEAR/MON/DAY ~a/~a/~a" n1 n2 n3)
	  (setf (datetime-days *dt*) (1+ (ymd (parse-integer n1) (parse-integer n2) (parse-integer n3)))))))))
       
(flet ((dump-spaces ()
	 (loop for d on (cdr *data*)
	       for h on *hint*
	       when (and (equal (cadr d) '(nil)) (or (char= (cadr h) #\Space) (char= (cadr h) #\-)))
	       do (rplacd d (cddr d))
	       and do (rplacd h (cddr h)))))
  (defun finalize-date (endian data hints)
    ;;(format *loop* "~%finalize-date ~a ~a ~a" endian data hints)
    ;;(format *loop* "~%COUNTS: ~d / ~d" (count '(nil) data :test #'equalp) (count '(nil) data :test-not #'equalp))
    (when (>= (count '(nil) data :test #'equalp) (* 2 (count '(nil) data :test-not #'equalp)))
      ;; If we have more than twice as many unmatchables as matchables... its nothing.
      (return-from finalize-date nil))
    (let ((*hint* (cons 'root hints))
	  (*data* (cons 'root data))
	  (*dt* (make-datetime :tz +local-time+ :days 0 :secs 0))
	  (*offset* nil))
      (declare (special *hint* *data* *dt* *offset*))
      (handler-case (progn (dump-spaces)          ;;(format *loop* "~%AFTDUMP DATA: ~s~%        HINT: ~s" *data* *hint*)
			   (finalize-time)        ;;(format *loop* "~%AFTTIME DATA: ~s~%        HINT: ~s" *data* *hint*)
			   (finalize-zone)        ;;(format *loop* "~%AFTZONE DATA: ~s~%        HINT: ~s" *data* *hint*)
			   (finalize-day endian)  ;;(format *loop* "~%AFTDAY  DATA: ~s~%        HINT: ~s" *data* *hint*)
			   (when *offset*
			     ;;(format *loop* "~%Apply offset of: ~a" *offset*)
			     (multiple-value-bind (daychange secs)
				 (truncate (+ (datetime-secs *dt*) *offset*) local-time:+seconds-per-day+)
			       (unless (zerop (datetime-days *dt*))
				 (incf (datetime-days *dt*) daychange))
			       (setf (datetime-secs *dt*) secs
				     (datetime-tz *dt*) +utc-tz+)))
			   (values *dt* (datetime-print *dt*)))
	(error () nil)))))
      

(defun punc/whitespace-p (c) (member c '(#\- #\+ #\: #\Space #\/ #\\ #\, #\. #\;)))
(defun punc/whitespace-p/nd (c) (member c '(#\- #\+ #\: #\Space #\/ #\\ #\, #\;)))
(defun parse-tagged-date (str &optional (start 0) (end (length str)) (endian 'mid))
  (when (or (<= end start) (punc/whitespace-p (elt str start))) (return-from parse-tagged-date nil))
  (loop with data = nil
        with hints = nil
        with try-tz-p = nil
        with pos = start
        for epos = (position-if #'punc/whitespace-p str :start pos :end end)
        as test = (nstring-downcase (subseq str pos epos))
        do (block nil
	     (macrolet ((test (fn value) `(awhen (,fn test) (push (copy-list ,value) data) (incf pos it) (return))))
	       (multiple-value-bind (n consumed)
		   (ignore-errors (parse-integer test :end (position-if-not #'digit-char-p test)))
		 (when n (push (cons 'number (subseq test 0  (position-if-not #'digit-char-p test))) data)
		       (incf pos consumed) (return)))
	       (test monday-p    '(day 1)) (test tuesday-p   '(day 2)) (test wednesday-p '(day 3))
	       (test thursday-p  '(day 4)) (test friday-p    '(day 5)) (test saturday-p  '(day 6)) (test sunday-p   '(day 7))
	       (test january-p    '(month . "01")) (test february-p   '(month . "02")) (test march-p      '(month . "03"))
	       (test april-p      '(month . "04")) (test may-p        '(month . "05")) (test june-p       '(month . "06"))
	       (test july-p       '(month . "07")) (test august-p     '(month . "08")) (test september-p  '(month . "09"))
	       (test october-p    '(month . "10")) (test november-p   '(month . "11")) (test december-p   '(month . "12"))
	       (test utc-p        '(zone . utc)) (test mdt-p        '(zone . mdt)) (test mst-p        '(zone . mst))
	       (let ((val (nstring-downcase (subseq str pos (position-if #'punc/whitespace-p/nd str :start pos :end end)))))
		 (when (or (am-p val) (pm-p val))
		   (setf epos (+ pos (length val)))
		   (setf test val)
		   (test am-p '(period . am)) (test pm-p '(period . pm))))
	       ;;(format *loop* "~%WHAT IS THIS: ~a" test)
	       ;;(sleep .1)
	       (push '(nil) data)))
        do (flet ((next-char () (setf pos (or (position #\Space str :start (1+ pos) :end end :test-not #'char=) end))))
	     ;;(format *loop* "~%DATA: ~s~%HINTS: ~s" data hints)
	     (cond ((= pos end))
		   ((not (= pos (or epos end)))
		    (if try-tz-p 
			(progn (pop hints) (pop data) (push (elt str pos) hints) (next-char) (setf try-tz-p nil))
			(progn (push #\nul hints) (setf try-tz-p t))))
		   (t (push (elt str pos) hints) (next-char)  (setf try-tz-p nil))))
        while (< pos end)
        finally (return-from parse-tagged-date (finalize-date endian (nreverse data) (nreverse hints)))))

;; MOVE TO A TESTS PACKAGE... 
;; (And turn them into tests...)

(parse-tagged-date "2001-10-05") ;; Sees year, big endian
(parse-tagged-date "10/05/2001") ;; Sees year, little endian
(iso8601/ymd *loop* (parse-tagged-date "10/05/01"))   ;; Mid endian
(parse-tagged-date "05/Oct/01")  ;; Little endian.
(parse-tagged-date "Wednesday 05, October 2001") ;; Sees day+month+year, little endian.
(parse-tagged-date "10:30 pm Wednesday 05, October 2001") ;; Sees day+month+year, little endian.
(parse-tagged-date "10:30:25 a.m. Wednesday October 05 2001") 

(iso8601/ymd *loop* (parse-tagged-date "2010-10"))
(iso8601/ymd *loop* (parse-tagged-date "2010-10-1"))
(iso8601/ymd *loop* (parse-tagged-date "2010-10-31"))
(iso8601/ymd *loop* (parse-tagged-date "2005-5-20"))
(parse-tagged-date "2010-Oct") 
(parse-tagged-date "2010, Oct")
(parse-tagged-date "2010W48")
(parse-tagged-date "20101005T114222")
(parse-tagged-date "20101005T114222Z")
(parse-tagged-date "20101005T114222UTC")
(parse-tagged-date "2010-10-05T11:42:22")    
(parse-tagged-date "2010-10-05T11:42:22Z")   
(parse-tagged-date "2010-10-05T11:42:22UTC") 
		  
(parse-tagged-date "2011-002 12pm")
(parse-tagged-date "2011-016 12pm")
(parse-tagged-date "2011-031 12pm")
(parse-tagged-date "2011-032 12pm")
(parse-tagged-date "2011-256 12pm")
(parse-tagged-date "2011-365 12pm")
(parse-tagged-date "2011-366 12pm")

(parse-tagged-date "10")       
(parse-tagged-date "2010")     
(parse-tagged-date "2011")     
(parse-tagged-date "2011-W22") 

(parse-tagged-date "w2010") 
(parse-tagged-date "2004W01") 
(parse-tagged-date "2005W01") 
(parse-tagged-date "2006W01") 
(parse-tagged-date "2007W01") 
(parse-tagged-date "2008W01") 
(parse-tagged-date "2009W01") 
(parse-tagged-date "2010W01") 
(parse-tagged-date "2011W01") 


(parse-tagged-date "2005W32")
(iso8601/w *loop* (parse-tagged-date "2005W32-6"))
(iso8601/w *loop* (parse-tagged-date "2009W46"))
(datetime-week (parse-tagged-date "2005W32"))
(datetime-week (parse-tagged-date "2009W46"))
(parse-tagged-date "2010W01")
(parse-tagged-date "2010W01-1")
(parse-tagged-date "2010W01-2")
(parse-tagged-date "2010W01-3") 
(parse-tagged-date "2010W01-4") 
(parse-tagged-date "2010W01-5") 
(parse-tagged-date "2010W01-6") 
(parse-tagged-date "2010W01-7") 
(parse-tagged-date "2010W01-8") 
(parse-tagged-date "2010W02")
(parse-tagged-date "2011W04")
;; (datetime-week (make-datetime :tz 2 :secs 0 :days 1))
(parse-tagged-date "2010W04")
(parse-tagged-date "W46")      
(parse-tagged-date "W46-2")
(parse-tagged-date "W46 1pm")
(parse-tagged-date "2010W461")              ;; Return NIL, because input is WRONG.
(parse-tagged-date "2010 and this?") ;; Return Nil, because input is WRONG.

(parse-tagged-date "2010W24 11:44 UTC")
(parse-tagged-date "2010W24 UTC")

(parse-tagged-date "wednesday 3")
(parse-tagged-date "tuesday 6")

(parse-tagged-date "11:42:22 Z")
(parse-tagged-date "11:42:22 am")
(parse-tagged-date "12 noon")
(parse-tagged-date "12 midnight")
(parse-tagged-date "11:42:22 a.m.")
(parse-tagged-date "11:42:22 p.m")

(parse-tagged-date "08:42:22 Z")
(parse-tagged-date "11:42:22Z")
(parse-tagged-date "11:42:22pm Z")
(parse-tagged-date "11:42 22, Z, 33")  ;; Return NIL because input is WRONG.
(parse-tagged-date "11:42:22-1033")    
(parse-tagged-date "11:42:22+1033")    
(parse-tagged-date "11:42:22 -1033")
(parse-tagged-date "11:42:22-10:33") 
(parse-tagged-date "11 : 42 : 22+1033")




;; Here are some invalid ones that (as of Sat Feb 12, 2011) passed for dates.
;; ALL OF THEM SHOULD BE REMOVED.

(remove nil (list "" " " " oo" "oo"
		  "Centennial Place East" "Jenny" "Peggy" "Jake" "Larry" "Cooling Fans, need about 4" "Conduit 3/4 600 feet"
		  "Desk & Chair for sales persons in office." "Eau Claire Place I" "Eau Claire Place II" 
		  "Birchcliff Energy Building" "Selkirk House" "Calgary Place II" "Canada Place" "Jamieson Place" "Le Rocc"
		  "Harry Hays Building" "Place 926" "ERCB Building" "MNP Place" "Chevron Plaza" "Roslyn Building"
		  "Calgary Place I" "Place 9-6" "AMEC Place" "Place 800" "Alpine Building" "Bradie Building" "Calgary House"
		  "Herald Annex Building" "Lougheed Building" "Hanover Place" "Guinness House" "Life Plaza" "Elveden House"
		  "Sierra Place" "Iveagh House" "Encor Place" "Manulife House" "Herald Building" "Herald Block" 
		  "AXYS Building" "Barron Building" "Royal Bank Building" "Molson's Bank Building" "Hudson's Block" 
		  "Calgary Public Building" "Doll Block" "Burns Building" "Sundog Place" "Gulf Canada Square"
		  "Grain Exchange Building" "Encana Place" "Le Germain" "Palliser One" "Fording Place" "People Place"
		  "Calgary Chamber of Commerce Building" "dinburgh Place Condominium" "Grange Building"
		  "CB Richard Ellis/Dennis Djonlich" "Jeff Landels" "CB Richard Ellis/Chris Conrad" "Colliers/Aly Lalani"
		  "N/A" "Sean McCullough" "Kelsey Oilund" "David or Alec Neill" "Dave Drader" "Cheryl Leal" "Chris McKee"
		  "Craig Finn" "Jim Sekora" "" "dennisdjonlich@cbrecom" "gordonmoss@govabca" "chrisconrad@cbrecom"
		  "ryanhandley@brookfieldcom" "jhampaul@mlpmca" "jmilino@macbainca" "kenerdman@snclavalinomcom"
		  "realex@realexorg" "dpilip@ay-abcom" "koilund@calgary-homescom" "mbrody@realexorg"
		  "kenerdman@snclavalinomcom" "lsullivan@20viccom" "hsiviglia@narlandcom" "agravelle@berezanca"
		  "cfinn@calgarychambercom" "" "jim@mccollegegroupcom" "mikemcauliffe@rcirogerscom" "murray@amdencom")
	:key #'parse-tagged-date)
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THERE ARE SOME FULLY-PARSE NUM ERRORS...

;; slashes and dashes probably shouldn't be treated as commas...

;2002-02-02
;1979/98
;1979/2002
;1907/86
;1979/90
;403-
