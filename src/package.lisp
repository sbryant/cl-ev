(defpackage ev
  (:use :cl :cffi)

  (:export :ev-loop
           :ev_now
           :event-dispatch

           :start-watcher
           :stop-watcher

           :ev-timer
           :set-timer))

(in-package :ev)
