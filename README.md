cl-ev
-----
cl-ev is an attempt at  cffi bindings for libev. 

Example Usage 
-------------
This is a port of the example libev program. 

```common-lisp
(ql:quickload :ev)
(in-package :ev)

(defcallback stdin-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (format t "stdin ready~%")
  (ev_io_stop loop w)
  (ev_break loop EVBREAK_ALL))

(defcallback timeout-cb :void ((loop :pointer) (w :pointer) (revents :int))
  (format t "timeout ~%")
  (ev_break loop EVBREAK_ALL))

(defparameter *stdin-watcher* (foreign-alloc 'ev_io))
(defparameter *timeout-watcher* (foreign-alloc 'ev_timer))

(defun main ()
  (let ((loop (ev_default_loop 0)))
    (ev_io_init *stdin-watcher*
                'stdin-cb
                0
                EV_READ)
    (ev_io_start loop *stdin-watcher*)

    (ev_timer_init *timeout-watcher*
                   'timeout-cb
                   5.5d0
                   0.0d0)
    (ev_timer_start loop *timeout-watcher*)

    (ev_run loop 0)))
```
