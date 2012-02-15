cl-ev
-----
cl-ev is an attempt at  cffi bindings for libev. 

## Example Usage 
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

(defun ev_init (ev cb_)
  (with-foreign-slots ((active pending priority cb) ev ev_io)
    (setf active 0 
          pending 0 
          priority 0 
          cb (get-callback cb_))))

(defun ev_io_set (ev fd_ events_)
  (with-foreign-slots ((fd events) ev ev_io)
    (setf fd fd_ 
          events events_)))

(defun ev_timer_set (ev after_ repeat_)
  (with-foreign-slots ((at repeat) ev ev_timer)
    (setf at after_
          repeat repeat_)))

(defun ev_io_init (ev cb fd events) 
  (ev_init ev cb)
  (ev_io_set ev fd events))

(defun ev_timer_init (ev cb after repeat)
  (ev_init ev cb)
  (ev_timer_set ev after repeat))


(with-foreign-objects ((stdin-watcher 'ev_io)
                       (timeout-watcher 'ev_timer))
  (defun main ()
    (with-foreign-object (l :pointer)
      (setf l (ev_default_loop 0))

      (ev_io_init stdin-watcher 
                  'stdin-cb
                  0
                  EV_READ)
      (ev_io_start l stdin-watcher)

      (ev_timer_init timeout-watcher
                     'timeout-cb
                     5.5d0
                     0.0d0)

      (ev_timer_start l timeout-watcher)

      (ev_run l 0))))
```
