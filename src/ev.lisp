(in-package :ev)

(defparameter *loops* (make-hash-table))
(defparameter *watchers* (make-hash-table))
(defparameter *callbacks* (make-hash-table))
(defparameter *reschedule-callbacks* (make-hash-table))

(defclass ev-loop () 
  ((event-loop :accessor event-loop
               :initarg :ev-loop
               :documentation "the libev event_loop"))
  (:default-initargs . (:ev-loop (ev_loop_new EVFLAG_AUTO))))

(defclass ev-watcher ()
  ())

(defclass ev-io-watcher (ev-watcher)
  ((watcher-pointer :accessor ev-pointer
                    :initform (foreign-alloc 'ev_io))))

(defclass ev-timer (ev-watcher)
  ((timer-pointer :accessor ev-pointer
                  :initform (foreign-alloc 'ev_timer))))

(defclass ev-periodic (ev-watcher)
  ((periodic-pointer :accessor ev-pointer
                     :initform (foreign-alloc 'ev_periodic))))

(defmethod initialize-instance :after ((self ev-loop) &key)
  (let ((ptr (event-loop self)))
    (setf (gethash (pointer-address ptr) *loops*) self)
    (tg:finalize self (lambda () 
                        (if (eq 1 (ev_is_default_loop ptr)) ;; we don't own the ev default loop
                            (ev_loop_destroy (ev_default_loop 0))
                            (ev_loop_destroy ptr))))))

(defmethod initialize-instance :after ((self ev-watcher) &key)
  (let ((ptr (ev-pointer self)))
    (tg:finalize self (lambda () 
                        (cffi:foreign-free ptr)))
    (setf (gethash (pointer-address ptr) *watchers*) self)))

(defun callback-key (watcher)
  (pointer-address (ev-pointer watcher)))

(defgeneric ev-callback (ev-loop watcher events))
(defgeneric set-io-watcher (ev-loop watcher fd event-type function))
(defgeneric set-timer (ev-loop watcher function timeout &key repeat))
(defgeneric stop-watcher (loop watcher))
(defgeneric start-watcher (loop watcher))
(defgeneric event-dispatch (ev-loop))

(defmethod stop-watcher :before ((loop ev-loop) watcher)
  (when (= 1 (ev_is_pending (ev-pointer watcher)))
    (ev_invoke_pending (event-loop loop))))

(defmethod stop-watcher ((loop ev-loop) (watcher ev-io-watcher))
  (when (= 1 (ev_is_active (ev-pointer watcher)))
    (ev_io_stop (event-loop loop) (ev-pointer watcher)))
  (remhash (callback-key watcher) *watchers*))

(defmethod stop-watcher ((loop ev-loop) (watcher ev-timer))
  (when (= 1 (ev_is_active (ev-pointer watcher)))
    (ev_timer_stop (event-loop loop) (ev-pointer watcher)))
  (remhash (callback-key watcher) *watchers*))

(defmethod stop-watcher ((loop ev-loop) (watcher ev-periodic))
  (when (= 1 (ev_is_active (ev-pointer watcher)))
    (ev_periodic_stop (event-loop loop) (ev-pointer watcher)))
  (remhash (callback-key watcher) *watchers*))

(defmethod set-io-watcher ((loop ev-loop) (watcher ev-io-watcher) fd event-type function)
  (setf (gethash (callback-key watcher) *callbacks*)
        function)
  (ev_io_init (ev-pointer watcher) 'ev_callback fd event-type))

(defmethod set-timer ((loop ev-loop) (watcher ev-timer) function timeout &key (repeat 0.0d0))
  (setf (gethash (callback-key watcher) *callbacks*)
        function)
  (ev_timer_init (ev-pointer watcher) 'ev_callback timeout repeat))

(defmethod set-perodic ((loop ev-loop) (watcher ev-periodic) cb offset interval reschedule-cb)
  (setf (gethash (callback-key watcher) *callbacks*)
        cb)
  (when reschedule-cb
    (setf (gethash (callback-key watcher) *reschedule-callbacks*)
          reschedule-cb))
  (ev_periodic_init (ev-pointer watcher) 'ev_callback offset interval (if reschedule-cb
                                                                       'ev_reschedule_callback
                                                                       (cffi:null-pointer))))

(defcallback ev_reschedule_callback ev_tstamp ((watcher :pointer) (now ev_tstamp))
  (let ((w (gethash (pointer-address watcher) *watchers*)))
    (funcall (gethash (callback-key w) *reschedule-callbacks*) w now)))

(defcallback ev_callback :void ((ev-loop :pointer) (watcher :pointer) (events :int))
  (let ((l (gethash (pointer-address ev-loop) *loops*))
        (w (gethash (pointer-address watcher) *watchers*)))
    (ev-callback l w events)))

(defmethod ev-callback ((loop ev-loop) (watcher ev-watcher) events)
  (format t "Callback dispatch hit~%")
  (funcall (gethash (callback-key watcher) *callbacks*) loop watcher events))

(defmethod event-dispatch ((loop ev-loop))
  (ev_run (event-loop loop) 0))

(defmethod event-dispatch :before ((loop ev-loop))
  (maphash (lambda (k v) 
             (start-watcher loop v)) *watchers*))

(defmethod start-watcher ((loop ev-loop) (watcher ev-io-watcher))
  (ev_io_start (event-loop loop) (ev-pointer watcher)))

(defmethod start-watcher ((loop ev-loop) (watcher ev-timer))
  (ev_timer_start (event-loop loop) (ev-pointer watcher)))

(defmethod start-watcher ((loop ev-loop) (watcher ev-periodic))
  (ev_periodic_start (event-loop loop) (ev-pointer watcher)))
