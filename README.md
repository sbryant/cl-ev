cl-ev
-----
cl-ev is an attempt at  cffi bindings for libev.

API Support
-----------
We support the following watchers:

* Periodic
* IO
* Timer
* Idle

TODO
----
Support the following watchers:

* Signal
* Child
* Stat
* Fork

Example Usage
-------------
This is a port of the example libev program.

```common-lisp
(ql:quickload :ev)
(in-package :ev)

(defparameter *io-handler* (make-instance 'ev-io-watcher))
(defparameter *timer-handler* (make-instance 'ev-timer))
(defparameter *periodic-handler* (make-instance 'ev-periodic))

(defun io-cb (loop watcher events)
  (format t "IO Callback hit! loop ~S watcher ~S events ~S~%" loop watcher events))

(defun timer-cb (loop watcher events)
  (format t "Timer Callback hit! loop ~S watcher ~S events ~S~%" loop watcher events))

(defun periodic-cb (loop watcher events)
  (format t "Periodic Callback hit! loop ~S watcher ~S events ~S~%" loop watcher events))

(defun run-loop ()
  (let ((l (make-instance 'ev-loop)))
    (set-timer l *timer-handler* #'timer-cb 5.5d0)
    (set-io-watcher l *io-handler* 0 EV_READ #'io-cb)
    (set-perodic l *periodic-handler* #'periodic-cb 0.0d0 10.0d0 nil)
    (event-dispatch l)))
```
