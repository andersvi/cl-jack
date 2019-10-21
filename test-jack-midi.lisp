;;; 
;;; various tests and checks, mostly related to the midipart of Jack
;;;


(setf midiports (jack-get-ports *CLJackClient*  "" "midi" 0))

(loop for i from 0
     and port = (mem-aref midiports :string i)
     while port
     collect port)
("CLJack:midiout" "CLJack:midiout" "fluidsynth:midi")


(defcfun "jack_connect" :int
  (client :pointer)
  (source-port :string)
  (destination-port :string))

(defcfun "jack_port_name" :string
  (port (:pointer jack_port_t)))

(jack-connect *CLJackClient*
	      (jack-port-name *jack-midi-output-port*)
	      "fluidsynth:midi")

(jack-disconnect *CLJackClient*
	      (jack-port-name *jack-midi-output-port*)
	      "fluidsynth:midi")

(defcfun "jack_port_by_name" :pointer
  (client :pointer)
  (port-name :string))

(jack-client-close *CLJackClient*)

(jack-frame-now)

(clrhash *jack-seq*)

(defun play-some-notes (&optional (tempo 0.4) (dur 2))
  ;;(clrhash *jack-seq*)
  (loop with offset = 0
     for sek from 0 below dur by tempo
     do
       (let* ((start sek)
	      (dur tempo))
	 (jack-play-note *jack-seq* start dur 60 80 0))))


(setf *spiller* nil)
(setf bend 0)
(seqhash-midi-pitch-wheel-msg  *jack-seq* (jack-frame-now) (incf bend 200) 0)
(progn
  (setf *spiller* t)
  (setf rytme 0.12)
  (setf bend 0)
  (loop
     (if (not (and *spiller* (< bend 16384)))
	 (return nil)
	 (progn (jack-play-note *jack-seq* 0 rytme 60 80 0)
		(sleep rytme)))))



(seqhash-midi-pitch-wheel-msg  *jack-seq* (jack-frame-now) 8192 0)
(loop for bend from 0 to 16000 by 100
     do
     (seqhash-midi-pitch-wheel-msg  *jack-seq* (jack-frame-now) bend 0)
     (sleep 0.2))


(play-some-notes 0.4 2.0)


(progn
  (seqhash-midi-note-on *jack-seq* (jack-frame-now) (setf *thisnote* 60) 127 1)
  (seqhash-midi-note-off *jack-seq* (jack-frame-now 0.4) *thisnote* 127 1))

(seqhash-midi-program-change *jack-seq* (jack-frame-now) (random 127) 3)

(seqhash-midi-note-on *jack-seq* (jack-frame-now) (setf *thisnote* 60) 127)
(seqhash-midi-note-off *jack-seq* (jack-frame-now 0.3) *thisnote* 0)

(progn
  (seqhash-midi-program-change *jack-seq* (jack-frame-now) (random 127) 1)
  (seqhash-midi-note-on *jack-seq* (jack-frame-now) (setf *thisnote* 60) 127)
  (seqhash-midi-note-off *jack-seq* (jack-frame-now 0.3) *thisnote* 0))

(with-hash-table-iterator (get-note *jack-seq*)
  (loop (multiple-value-bind (more? time notes) (get-note)
	  (unless more? (return nil))
	  (if (> time 10000)
	      (return nil)
	      (if (zerop (mod time 2))
		  (remhash time *jack-seq*))))))

(defun play-some-notes (&optional (tempo 0.1) (dur 8))
  ;;(clrhash *jack-seq*)
  (loop with offset = 0
     for sek from 0 below dur by tempo
     do
       (let* ((start (jack-frame-now sek))
	      (end (+ start 10000))
	      (channel (random 16)))
	 (progn
	   (seqhash-midi-note-on *jack-seq* start (setf *thisnote* (+ 20 (random 100))) 80 channel)
	   (seqhash-midi-note-off  *jack-seq* end *thisnote* 0 channel)))))

(defun play-some-notes (&optional (tempo 0.1) (dur 8))
  ;;(clrhash *jack-seq*)
  (loop with offset = 0
     for sek from 0 below dur by tempo
     do
       (let* ((start sek)
	      (dur (+ 0.01 (expt (random 1.0) 2)))
	      (channel (random 16)))
	 (jack-play-note *jack-seq* start dur (+ 20 (random 100)) 80 channel))))

(defun play-some-notes (&optional (tempo 0.1) (dur 8))
  (loop with note = 60
     for sek from 0  by tempo
     do
       (let* ((start (jack-frame-now sek))
	      (end (jack-frame-now (+ sek tempo))))
	 (when  (>= sek dur)
	   (loop-finish)
	   (seqhash-midi-note-off *jack-seq* end note 50 0))
	 (progn
	   (seqhash-midi-note-on *jack-seq* start note 80 0)
	   (seqhash-midi-note-off *jack-seq* end note 50 0)
	   ))))

(let ((rytme 0.01))
  (play-some-notes rytme (+ 10 rytme)))

(loop repeat 30
   do
     (jack-play-note *jack-seq* 0 1/200 60 100)
     (sleep 1/40))

(jack-all-notes-off *jack-seq*)

(jack-midi-stop-all)

(seqhash-midi-note-on *jack-seq* (jack-frame-now) 60 127 0)
(seqhash-midi-note-off *jack-seq* (jack-frame-now) 60 127 0)

(dotimes (i 80)
  (let* ((note (mod (+ 40 i (random 80)) 120))
	 (rytme (/ i 21))
	 (dur (* rytme 1)))
    (seqhash-midi-note-on *jack-seq* (jack-frame-now rytme) note  100 0)
    (seqhash-midi-note-off *jack-seq* (jack-frame-now (+ rytme dur)) note 0 0)
    ))



(loop repeat 3 do (play-some-notes (+ 1/30 (random 0.1)) 12))

(hash-table-count *jack-seq*)
(hash-table-size *jack-seq*)
(clrhash *jack-seq*)

;; example of (non-callback-based) rt-scheduling

(setf *jack-midi-playing* nil)
(setf *jack-midi-playing* t)
(setf *play-queue* t)
(setf *play-queue* nil)

(let ((note 40))
  (loop
     (when (not *jack-midi-playing*)
       (return))
     (let ((rytme (/ 1 64)))
       (when *play-queue*
	 (let ((note (+ 40 (mod (+ (incf note) (* 12 (random 3))) 60)))
	       (rytme (/ 1 64))
	       (dur (* rytme 8)))
	   (seqhash-midi-note-on *jack-seq* (jack-frame-now rytme) note 90 0)
	   (seqhash-midi-note-off *jack-seq* (jack-frame-now (+ rytme dur)) note 0 0)))
       (sleep rytme))
     ))

(cl-user::set-up-profiler :packages '(cl-jack))
(cl-user::profile (loop repeat 300 do (play-some-notes (+ 1/30 (random 0.1)) 12)))
(env:start-environment)
