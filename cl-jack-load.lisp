(defpackage :cl-jack
  (:use :common-lisp :cffi)
  (:export "JACK-OPEN-SOUND" "PRINT-JACK-SF" "CL-JACK-PLAY-SOUND" "CL-JACK-CLOSE-SOUND" "JACKPLAY-TOGGLE-READ" "CL-JACK-SEEK"))
(in-package :cl-jack)

#-cffi-new (require :cffi (make-pathname :directory (append om-api::*externals-directory* '("FFI")) :name "load-new-cffi"))

(defun compile?-and-load (file)
  (cl-user::compile-file-if-needed file)
  (load file))

(defparameter cl-jack-files '("cl-jack" "cl-jack-midi" "cl-jack-audio" "cl-jackplay-interleaved"))

(dolist (file cl-jack-files)
  (compile?-and-load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

;; various callbacks are defined in the files above.  These all use
;; the same name #'cl-jack-process-callback (ie. the last one loaded
;; will be plugged in below).  The callback-function can aswell be
;; changed on the fly.

(defun cl-jack-init-all ()
  (progn
    (cl-jack-init-midi)
    (cl-jack-init-audio)
    (jack-set-process-callback *CLJackClient* (callback cl-jack-process-callback) 0)
    (jack-activate *CLJackClient*)))

(oa::om-add-init-func 'cl-jack-init-all)

(pushnew :cl-jack *features*)
(provide :cl-jack)

