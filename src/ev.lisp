;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 2.0.4
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.
(in-package :ev)
(define-foreign-library libev 
  (:unix (:or "libev.4.dylib" "libev.4.so" "libev.dylib" "libev.so"))
  (t (:default "libev")))

(use-foreign-library libev)


;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cl:defconstant EV_MINPRI -2)

(cl:defconstant EV_MAXPRI 2)

(cl:defconstant EV_COMPAT3 1)

(cl:defconstant EV_FEATURES #x7f)

(cl:defconstant EV_FEATURE_CODE (cl:logand #x7f 1))

(cl:defconstant EV_FEATURE_DATA (cl:logand #x7f 2))

(cl:defconstant EV_FEATURE_CONFIG (cl:logand #x7f 4))

(cl:defconstant EV_FEATURE_API (cl:logand #x7f 8))

(cl:defconstant EV_FEATURE_WATCHERS (cl:logand #x7f 16))

(cl:defconstant EV_FEATURE_BACKENDS (cl:logand #x7f 32))

(cl:defconstant EV_FEATURE_OS (cl:logand #x7f 64))

(cl:defconstant EV_MULTIPLICITY (cl:logand #x7f 4))

(cl:defconstant EV_PERIODIC_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_STAT_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_PREPARE_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_CHECK_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_IDLE_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_FORK_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_CLEANUP_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_SIGNAL_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_CHILD_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_ASYNC_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_EMBED_ENABLE (cl:logand #x7f 16))

(cl:defconstant EV_WALK_ENABLE 0)

(cffi:defctype ev_tstamp :double)

(cl:defconstant EV_PROTOTYPES 1)

(cl:defconstant EV_VERSION_MAJOR 4)

(cl:defconstant EV_VERSION_MINOR 4)

(defanonenum 
	(EV_UNDEF #.#xFFFFFFFF)
	(EV_NONE #.#x00)
	(EV_READ #.#x01)
	(EV_WRITE #.#x02)
	(EV__IOFDSET #.#x80)
	(EV_IO #.#x01)
	(EV_TIMER #.#x00000100)
	(EV_TIMEOUT #.#x00000100)
	(EV_PERIODIC #.#x00000200)
	(EV_SIGNAL #.#x00000400)
	(EV_CHILD #.#x00000800)
	(EV_STAT #.#x00001000)
	(EV_IDLE #.#x00002000)
	(EV_PREPARE #.#x00004000)
	(EV_CHECK #.#x00008000)
	(EV_EMBED #.#x00010000)
	(EV_FORK #.#x00020000)
	(EV_CLEANUP #.#x00040000)
	(EV_ASYNC #.#x00080000)
	(EV_CUSTOM #.#x01000000)
	(EV_ERROR #.#x80000000))

(cffi:defcstruct ev_watcher
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer))

(cffi:defctype ev_watcher ev_watcher)

(cffi:defcstruct ev_watcher_list
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(next :pointer))

(cffi:defctype ev_watcher_list ev_watcher_list)

(cffi:defcstruct ev_watcher_time
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(at :double))

(cffi:defctype ev_watcher_time ev_watcher_time)

(cffi:defcstruct ev_io
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(next :pointer)
	(fd :int)
	(events :int))

(cffi:defctype ev_io ev_io)

(cffi:defcstruct ev_timer
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(at :double)
	(repeat :double))

(cffi:defctype ev_timer ev_timer)

(cffi:defcstruct ev_periodic
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(at :double)
	(offset :double)
	(interval :double)
	(reschedule_cb :pointer))

(cffi:defctype ev_periodic ev_periodic)

(cffi:defcstruct ev_signal
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(next :pointer)
	(signum :int))

(cffi:defctype ev_signal ev_signal)

(cffi:defcstruct ev_child
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(next :pointer)
	(flags :int)
	(pid :int)
	(rpid :int)
	(rstatus :int))

(cffi:defctype ev_child ev_child)

(cffi:defctype ev_statdata :pointer)

(cffi:defcstruct ev_stat
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(next :pointer)
	(timer ev_timer)
	(interval :double)
	(path :string)
	(prev :pointer)
	(attr :pointer)
	(wd :int))

(cffi:defctype ev_stat ev_stat)

(cffi:defcstruct ev_idle
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer))

(cffi:defctype ev_idle ev_idle)

(cffi:defcstruct ev_prepare
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer))

(cffi:defctype ev_prepare ev_prepare)

(cffi:defcstruct ev_check
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer))

(cffi:defctype ev_check ev_check)

(cffi:defcstruct ev_fork
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer))

(cffi:defctype ev_fork ev_fork)

(cffi:defcstruct ev_cleanup
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer))

(cffi:defctype ev_cleanup ev_cleanup)

(cffi:defcstruct ev_embed
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(other :pointer)
	(io ev_io)
	(prepare ev_prepare)
	(check ev_check)
	(timer ev_timer)
	(periodic ev_periodic)
	(idle ev_idle)
	(fork ev_fork)
	(cleanup ev_cleanup))

(cffi:defctype ev_embed ev_embed)

(cffi:defcstruct ev_async
	(active :int)
	(pending :int)
	(priority :int)
	(data :pointer)
	(cb :pointer)
	(sent :pointer))

(cffi:defctype ev_async ev_async)

(cffi:defcunion ev_any_watcher
	(w ev_watcher)
	(wl ev_watcher_list)
	(io ev_io)
	(timer ev_timer)
	(periodic ev_periodic)
	(signal ev_signal)
	(child ev_child)
	(stat ev_stat)
	(idle ev_idle)
	(prepare ev_prepare)
	(check ev_check)
	(fork ev_fork)
	(cleanup ev_cleanup)
	(embed ev_embed)
	(async ev_async))

(defanonenum 
	(EVFLAG_AUTO #.#x00000000)
	(EVFLAG_NOENV #.#x01000000)
	(EVFLAG_FORKCHECK #.#x02000000)
	(EVFLAG_NOINOTIFY #.#x00100000)
	(EVFLAG_NOSIGFD #.0)
	(EVFLAG_SIGNALFD #.#x00200000)
	(EVFLAG_NOSIGMASK #.#x00400000))

(defanonenum 
	(EVBACKEND_SELECT #.#x00000001)
	(EVBACKEND_POLL #.#x00000002)
	(EVBACKEND_EPOLL #.#x00000004)
	(EVBACKEND_KQUEUE #.#x00000008)
	(EVBACKEND_DEVPOLL #.#x00000010)
	(EVBACKEND_PORT #.#x00000020)
	(EVBACKEND_ALL #.#x0000003F)
	(EVBACKEND_MASK #.#x0000FFFF))

(cffi:defcfun ("ev_version_major" ev_version_major) :int)

(cffi:defcfun ("ev_version_minor" ev_version_minor) :int)

(cffi:defcfun ("ev_supported_backends" ev_supported_backends) :unsigned-int)

(cffi:defcfun ("ev_recommended_backends" ev_recommended_backends) :unsigned-int)

(cffi:defcfun ("ev_embeddable_backends" ev_embeddable_backends) :unsigned-int)

(cffi:defcfun ("ev_time" ev_time) :double)

(cffi:defcfun ("ev_sleep" ev_sleep) :void
  (delay :double))

(cffi:defcfun ("ev_set_allocator" ev_set_allocator) :void
  (cb :pointer))

(cffi:defcfun ("ev_set_syserr_cb" ev_set_syserr_cb) :void
  (cb :pointer))

(cffi:defcfun ("ev_default_loop" ev_default_loop) :pointer
  (flags :unsigned-int))

(cffi:defcfun ("ev_default_loop_uc_" ev_default_loop_uc_) :pointer)

(cffi:defcfun ("ev_is_default_loop" ev_is_default_loop) :int
  (loop :pointer))

(cffi:defcfun ("ev_loop_new" ev_loop_new) :pointer
  (flags :unsigned-int))

(cffi:defcfun ("ev_now" ev_now) :double
  (loop :pointer))

(cffi:defcfun ("ev_loop_destroy" ev_loop_destroy) :void
  (loop :pointer))

(cffi:defcfun ("ev_loop_fork" ev_loop_fork) :void
  (loop :pointer))

(cffi:defcfun ("ev_backend" ev_backend) :unsigned-int
  (loop :pointer))

(cffi:defcfun ("ev_now_update" ev_now_update) :void
  (loop :pointer))

(defanonenum 
	(EVRUN_NOWAIT #.1)
	(EVRUN_ONCE #.2))

(defanonenum 
	(EVBREAK_CANCEL #.0)
	(EVBREAK_ONE #.1)
	(EVBREAK_ALL #.2))

(cffi:defcfun ("ev_run" ev_run) :void
  (loop :pointer)
  (flags :int))

(cffi:defcfun ("ev_break" ev_break) :void
  (loop :pointer)
  (how :int))

(cffi:defcfun ("ev_ref" ev_ref) :void
  (loop :pointer))

(cffi:defcfun ("ev_unref" ev_unref) :void
  (loop :pointer))

(cffi:defcfun ("ev_once" ev_once) :void
  (loop :pointer)
  (fd :int)
  (events :int)
  (timeout :double)
  (cb :pointer)
  (arg :pointer))

(cffi:defcfun ("ev_iteration" ev_iteration) :unsigned-int
  (loop :pointer))

(cffi:defcfun ("ev_depth" ev_depth) :unsigned-int
  (loop :pointer))

(cffi:defcfun ("ev_verify" ev_verify) :void
  (loop :pointer))

(cffi:defcfun ("ev_set_io_collect_interval" ev_set_io_collect_interval) :void
  (loop :pointer)
  (interval :double))

(cffi:defcfun ("ev_set_timeout_collect_interval" ev_set_timeout_collect_interval) :void
  (loop :pointer)
  (interval :double))

(cffi:defcfun ("ev_set_userdata" ev_set_userdata) :void
  (loop :pointer)
  (data :pointer))

(cffi:defcfun ("ev_userdata" ev_userdata) :pointer
  (loop :pointer))

(cffi:defcfun ("ev_set_invoke_pending_cb" ev_set_invoke_pending_cb) :void
  (loop :pointer)
  (invoke_pending_cb :pointer))

(cffi:defcfun ("ev_set_loop_release_cb" ev_set_loop_release_cb) :void
  (loop :pointer)
  (release :pointer)
  (acquire :pointer))

(cffi:defcfun ("ev_pending_count" ev_pending_count) :unsigned-int
  (loop :pointer))

(cffi:defcfun ("ev_invoke_pending" ev_invoke_pending) :void
  (loop :pointer))

(cffi:defcfun ("ev_suspend" ev_suspend) :void
  (loop :pointer))

(cffi:defcfun ("ev_resume" ev_resume) :void
  (loop :pointer))

(cffi:defcfun ("ev_feed_event" ev_feed_event) :void
  (loop :pointer)
  (w :pointer)
  (revents :int))

(cffi:defcfun ("ev_feed_fd_event" ev_feed_fd_event) :void
  (loop :pointer)
  (fd :int)
  (revents :int))

(cffi:defcfun ("ev_feed_signal" ev_feed_signal) :void
  (signum :int))

(cffi:defcfun ("ev_feed_signal_event" ev_feed_signal_event) :void
  (loop :pointer)
  (signum :int))

(cffi:defcfun ("ev_invoke" ev_invoke) :void
  (loop :pointer)
  (w :pointer)
  (revents :int))

(cffi:defcfun ("ev_clear_pending" ev_clear_pending) :int
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_io_start" ev_io_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_io_stop" ev_io_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_io_stop" ev_io_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_timer_start" ev_timer_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_timer_stop" ev_timer_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_timer_again" ev_timer_again) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_timer_remaining" ev_timer_remaining) :double
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_periodic_start" ev_periodic_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_periodic_stop" ev_periodic_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_periodic_again" ev_periodic_again) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_signal_start" ev_signal_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_signal_stop" ev_signal_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_child_start" ev_child_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_child_stop" ev_child_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_stat_start" ev_stat_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_stat_stop" ev_stat_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_stat_stat" ev_stat_stat) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_idle_start" ev_idle_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_idle_stop" ev_idle_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_prepare_start" ev_prepare_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_prepare_stop" ev_prepare_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_check_start" ev_check_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_check_stop" ev_check_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_fork_start" ev_fork_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_fork_stop" ev_fork_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_cleanup_start" ev_cleanup_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_cleanup_stop" ev_cleanup_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_embed_start" ev_embed_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_embed_stop" ev_embed_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_embed_sweep" ev_embed_sweep) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_async_start" ev_async_start) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_async_stop" ev_async_stop) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_async_send" ev_async_send) :void
  (loop :pointer)
  (w :pointer))

(cffi:defcfun ("ev_loop" ev_loop) :void
  (loop :pointer)
  (flags :int))

(cffi:defcfun ("ev_unloop" ev_unloop) :void
  (loop :pointer)
  (how :int))

(cffi:defcfun ("ev_default_destroy" ev_default_destroy) :void)

(cffi:defcfun ("ev_default_fork" ev_default_fork) :void)

(cffi:defcfun ("ev_loop_count" ev_loop_count) :unsigned-int
  (loop :pointer))

(cffi:defcfun ("ev_loop_depth" ev_loop_depth) :unsigned-int
  (loop :pointer))

(cffi:defcfun ("ev_loop_verify" ev_loop_verify) :void
  (loop :pointer))
 


