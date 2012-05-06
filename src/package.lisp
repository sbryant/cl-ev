(defpackage ev
  (:use :cl :cffi)

  (:export :ev-loop
           :ev_now
           :event-dispatch

           :ev-watcher
           :watcher-slot
           :watcher-active-p
           :start-watcher
           :stop-watcher

           :ev-idle
           :set-idle

           :ev-timer
           :set-timer

           :ev-io-watcher
           :set-io-watcher

           :EV_READ
           :EV_WRITE))

(in-package :ev)
