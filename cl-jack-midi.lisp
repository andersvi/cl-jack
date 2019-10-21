;;===========================================================================
;;JACK API for Common Lisp/CFFI
;;
;;This program is free software; you can redistribute it and/or modify
;;it under the terms of the GNU Lesser General Public License as published by
;;the Free Software Foundation; either version 2.1 of the License, or
;;(at your option) any later version.
;;  
;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU Lesser General Public License for more details.
;;  
;;You should have received a copy of the GNU Lesser General Public License
;;along with this program; if not, write to the Free Software 
;;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;Author: Anders Vinjar


(in-package :cl-jack)

(defvar *jack-midi-output-port* nil)
(defvar *jack-midi-input-port* nil)

;;; global pool of seqs for this client, for separate control [start/stop/pause...]:
(defun make-jack-seqs () (make-hash-table :size 1500
					  :rehash-size 1.5
					  :rehash-threshold 0.7
					 )) 
(defparameter *jack-seqs* (make-jack-seqs))

;;; event-seq is a hash-table, keys are frameno at jacks' start-of-period (ie: jack-last-frame-time)
(defun make-jack-seq () (make-hash-table))

;;; provide one default seq for global queues, external schedulers etc:
 ;default sequencer
(defvar *jack-seq*
  (setf (gethash '*jack-seq* *jack-seqs*) (make-jack-seq)))



;;; MIDI EVENTS

;; TODO: expand with support for all midi-messages

(defun jack-add-event-this-period (seq period event)
  (setf (gethash period seq)
	(sort (nconc (gethash period seq) (list event))
	      #'(lambda (a b) (< (car a) (car b))))))

(defun jack-add-event-this-frame (seq frame event)
  (push event (gethash frame seq)))

;;; SEQUENCING EVENTS
;;;
;;; seq is a hashtable, key'ing on frame-numbers

;;; version hashing on frame-number

(defun seqhash-midi-event (seq frame event)
  (jack-add-event-this-frame seq frame event))

;;; using midi-classes:

(defun seqhash-midi-note-on (seq frame noteno velocity &optional (channel 1))
  (let ((event (om-midi::make-note-on-message frame noteno velocity channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-note-off (seq frame noteno velocity &optional (channel 1))
  (let ((event (om-midi::make-note-off-message frame noteno velocity channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-program-change (seq frame program &optional (channel 1))
  (let ((event (om-midi::make-program-change-message frame program channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-control-change (seq frame control value &optional (channel 1))
  (let ((event (om-midi::make-control-change-message frame control value channel)))
    (seqhash-midi-event seq frame event)))

(defun seqhash-midi-pitch-wheel-msg (seq frame bend &optional (channel 1))
  (let ((mybend (+ bend 8192)))		;expects values between -8192->8191
    (let ((event (om-midi::make-pitch-bend-message frame bend channel)))   ;;; use bend: in OM 6.9 values are 0-16383
      (seqhash-midi-event seq frame event))))

;; erase pending note-offs for interval - don't shut off later arriving notes
(defun seqhash-clear-note-offs (seq startframe endframe noteno &optional (channel 1))
  (maphash #'(lambda (key val)
	       (let ((event (car val)))
		 (when (and (<= startframe key endframe)
			    (typep event 'midi::note-off-message)
			    (eql (om-midi::midi-key event) noteno)
			    (eql (om-midi::midi-channel event) channel))
		   (remhash key seq))))
	   seq))

;; interface to higher-level funcs:

(defun jack-start-dur-to-frames (start dur)
  (let* ((dur-frames (sec->frame dur))
	 (startframe (jack-frame-now start))
	 (endframe (+ startframe dur-frames -1)))
    (values startframe endframe)))

(defun jack-play-event (seq start event)
  (seqhash-midi-event seq (jack-frame-now start) event))

(defun jack-play-note (seq start dur noteno &optional (vel 80) (chan 0))
  (let* ((startframe (jack-frame-now start))
	 (endframe (+ startframe (sec->frame dur) -1)))
    (seqhash-clear-note-offs seq startframe endframe noteno chan)
    (seqhash-midi-note-on seq startframe noteno vel chan)
    ;; (sleep (/ (jack-get-buffer-size *CLJackClient*)
    ;; 	   (jack-get-sample-rate *CLJackClient*)))
    (seqhash-midi-note-off seq endframe noteno 0 chan)))

(defun jack-all-notes-off (seq)
  (let ((sounding-notes '()))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (mapc #'(lambda (ev) (push (list (om-midi::midi-key ev) (1- (om-midi::midi-channel ev)))
					    sounding-notes))
		       val))
	     seq)
    (clrhash seq)
    (mapc #'(lambda (note)
	      (seqhash-midi-note-off seq (jack-frame-now) (car note) 0 (cadr note)))
	  sounding-notes)))

(defun jack-all-notes-off-and-kill-seq (seq)
  (jack-all-notes-off seq)
  (sleep (float (/ 2
  		   (jack-get-buffer-size *CLJACKCLIENT*)
  		   (jack-get-sample-rate *CLJACKCLIENT*))))
  (remhash seq *jack-seqs*))

(defun jack-reset (&optional (seq *jack-seq*))
  (dotimes (ch 16)
    (dotimes (key 127)
      (seqhash-midi-note-off seq (jack-frame-now) key 0 ch))))

;;(jack-reset)

(defun jack-reset-channels ()
  (loop for ch from 0 to 16
     do (seqhash-midi-program-change *jack-seq* (jack-frame-now) ch ch)))

;;(jack-reset-channels)

(defun jack-seq-hush-this-seq (seq)
  (jack-all-notes-off seq))

(defun jack-seq-hush-all-seqs ()
  (maphash #'(lambda (key seq)
	       (declare (ignore key))
	       (jack-all-notes-off-and-kill-seq seq))
	   *jack-seqs*))

(defparameter *playing* t)		;nil=shut up
;; (setf *playing* nil)

(defun play-from-seq (port-buf seq)
  (when *playing*
    (let ((this-period (jack-last-frame-time *CLJackClient*)))
      (loop for offset from 0 below (jack-get-buffer-size *CLJACKCLIENT*)
	 for key from this-period	;events hashed on frameno
	 for events = (gethash key seq)
	 when events
	 do 
	   (dolist (midimsg events)
	     (let ((buffer (jack-midi-event-reserve port-buf offset 3))) ;offset inside period
	       (unless (null-pointer-p buffer)
		 (setf (mem-aref buffer :int8 0) (om-midi::midi-status-byte midimsg) ;command
		       (mem-aref buffer :int8 1) (om-midi::midi-data-byte-1 midimsg) ;data-byte 1
		       (mem-aref buffer :int8 2) (om-midi::midi-data-byte-2 midimsg) ;data-byte 2
		       ))))
	   (remhash key seq)))))

;; callback function handles seq-events, plugged into jacks
;; process-callback

(defun cl-jack-handle-event-seqs (nframes)
  (let ((port-buf (jack-port-get-buffer *jack-midi-output-port* nframes)))
    (jack-midi-clear-buffer port-buf)
    ;;(play-from-seq port-buf *jack-seq*)
    (maphash #'(lambda (key seq)
		 (declare (ignore key))
		 (play-from-seq port-buf seq))
	     *jack-seqs*)))



(defun cl-jack-init-midi ()

  ;; get up and running

  (unless *CLJackClient*
    (setf *CLJackClient* (jack-client-open "CLJack" JackNullOption 0)))

  (setf *jack-midi-output-port*
	(let ((port (jack-port-register *CLJackClient*
					"midiout"
					*jack-default-midi-type*
					(foreign-enum-value 'jackportflags :jackportisoutput)
					0)))
	  (when (zerop (pointer-address port)) ;0 if not allocated
	    (setf port -1)
	    (cerror "Set *jack-midi-output-port* to -1" "*jack-midi-output-port* for Jack not allocated - check jack-server"))
	  port)))

(provice :cl-jack-midi)






;;(Jack-deactivate *CLJackClient*)

