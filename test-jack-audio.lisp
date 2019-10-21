;;;
;;; tests for jack audio, players, ringbuffers, connections etc
;;; 



(defparameter *somesounds*
  '("/sound/Floratone-1m.wav"
    "/music/Bruckner/Anton_Bruckner_Symphonie_Nr.7_E-Dur.ogg"))

(setf (jack-sf-playing? (first *jack-sounds*)) nil)
(setf (jack-sf-playing? (first *jack-sounds*)) t)

(cl-jack-play-sound (first *somesounds*) 50000 '(58000 . 0))
(cl-jack-play-sound (first *somesounds*))

(setf (jack-sf-loop? (first *jack-sounds*)) nil)
(setf (jack-sf-loop? (first *jack-sounds*)) t)
(setf (jack-sf-loop? (first *jack-sounds*)) '(50000 . 0))
(cl-jack-seek (jack-sf-sound-file-handle (first *jack-sounds*)) (ms->frame 58000))
(cl-jack-seek (jack-sf-sound-file-handle (first *jack-sounds*)) (ms->frame 58000))
