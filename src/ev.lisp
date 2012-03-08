(in-package :ev)

(defparameter *loops* (make-hash-table))
(defparameter *watchers* (make-hash-table))
(defparameter *callbacks* (make-hash-table))

(defclass ev-loop () 
  ((event-loop :accessor event-loop
               :initarg :ev-loop
               :documentation "the libev event_loop"))
  (:default-initargs . (:ev-loop (ev_loop_new EVFLAG_AUTO))))

(defclass ev-watcher ()
  ())

(defclass ev-io-watcher (ev-watcher)
  ((watcher :accessor watcher
            :initform (foreign-alloc 'ev_io))))

(defclass ev-timer (ev-watcher)
  ((timer :accessor watcher
          :initform (foreign-alloc 'ev_timer))))

(defmethod initialize-instance :after ((self ev-watcher) &key))

(defmethod initialize-instance :after ((self ev-loop) &key)
  (let ((ptr (event-loop self)))
    (setf (gethash (pointer-address ptr) *loops*) self)
    (tg:finalize self (lambda () 
                        (unless (eq 1 (ev_is_default_loop ptr)) ;; we don't own the ev default loop
                          (ev_loop_destroy ptr))))))

(defmethod initialize-instance :after ((self ev-io-watcher) &key)
  (setf (gethash (pointer-address (watcher self)) *watchers*) self))

(defmethod initialize-instance :after ((self ev-timer) &key)
  (setf (gethash (pointer-address (watcher self)) *watchers*) self))

(defun callback-key (watcher) 
  (pointer-address (watcher watcher)))

(defgeneric ev-callback (ev-loop watcher events))
(defgeneric set-io-watcher (ev-loop watcher fd event-type function))
(defgeneric set-timer (ev-loop watcher function timeout &key repeat))
(defgeneric event-dispatch (ev-loop))

(defmethod set-io-watcher ((loop ev-loop) (watcher ev-io-watcher) fd event-type function)
  (setf (gethash (callback-key watcher) *callbacks*)
        function)
  (ev_io_init (watcher watcher) 'ev_callback fd event-type)
  (ev_io_start (event-loop loop) (watcher watcher)))

(defmethod set-timer ((loop ev-loop) (watcher ev-timer) function timeout &key repeat)
  (setf (gethash (callback-key watcher) *callbacks*)
        function)
  (ev_timer_init (watcher watcher) 'ev_callback timeout (if repeat 1.0d0 0.0d0))
  (ev_timer_start (event-loop loop) (watcher watcher)))

(defcallback ev_callback :void ((ev-loop :pointer) (watcher :pointer) (events :int))
  (let ((l (gethash (pointer-address ev-loop) *loops*))
        (w (gethash (pointer-address watcher) *watchers*)))
    (ev-callback l w events)))

(defmethod ev-callback ((loop ev-loop) (watcher ev-watcher) events)
  (format t "Callback dispatch hit~%")
  (funcall (gethash (callback-key watcher) *callbacks*) loop watcher events))

(defmethod event-dispatch ((loop ev-loop))
  (ev_run (event-loop loop) 0))
