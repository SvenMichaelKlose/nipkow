(load "bender/vic-20/cpu-cycles.lisp")

(defun nipkow-make-wav (name file)
  (sb-ext:run-program "/usr/bin/mplayer"
    (list "-vo" "null" "-vc" "null" "-ao" (+ "pcm:fast:file=obj/" name ".wav") file)
    :pty cl:*standard-output*))

(defun nipkow-make-filtered-wav (name gain bass tv rate)
  (sb-ext:run-program "/usr/bin/sox"
    `(,(+ "obj/" name ".wav")
      ,(+ "obj/" name "." (downcase (symbol-name tv)) ".filtered.wav")
      ,@(& bass `("bass" ,bass))
      "lowpass" ,(princ (half rate) nil)
      ,@(& gain `("compand" "0.3,1" "6:-70,-60,-20" "-1" "-90" "0.2" "gain" ,gain)))
    :pty cl:*standard-output*))

(defun downsampled-audio-name (name tv)
  (+ "obj/" name ".downsampled." (downcase (symbol-name tv)) ".wav"))

(defun nipkow-make-conversion (name tv rate)
  (sb-ext:run-program "/usr/bin/sox"
    (list (+ "obj/" name "." (downcase (symbol-name tv)) ".filtered.wav")
          "-c" "1"
          "-b" "16"
          "-r" (princ rate nil)
          (downsampled-audio-name name tv))
    :pty cl:*standard-output*))

(defun unclip (x range)
  (integer (+ (half (- 16 range)) (* (/ x 16) range))))

(defun nipkow-average-pulse ()
  (integer (/ (cpu-cycles *tv*) *nipkow-pulse-rate* 8)))

(defun nipkow-shortest-pulse ()
  (- (nipkow-average-pulse) 8))

(defun nipkow-longest-pulse ()
  (+ (nipkow-average-pulse) 8))

(defun wav2pwm (out in &key video (pause-before 16000) (pause-after 16000))
  (adotimes 44 (read-byte in))
  (with (shortest  (nipkow-shortest-pulse)
         average   (nipkow-average-pulse))
    (adotimes pause-before
      (write-byte average out))
    (awhile (read-word in)
            nil
      (let sample (bit-xor (>> ! 12) 8)
        (write-byte (+ shortest sample) out))
      (& *video?*
         (read-byte video)
        (write-byte video out)))
    (adotimes pause-after
      (write-byte average out))))

(defun wav42pwm (out in &key video (pause-before 16000) (pause-after 16000))
  (adotimes 96 (read-byte in))
  (with (shortest  (nipkow-shortest-pulse)
         average   (nipkow-average-pulse))
    (adotimes pause-before
      (write-byte average out))
    (awhile (read-byte in)
            nil
      (write-byte (+ shortest (bit-xor (>> ! 4) 8)) out)
      (write-byte (+ shortest (bit-xor (bit-and ! 15) 8)) out))
    (adotimes pause-after
      (write-byte average out))))

(defun nipkow-convert (name gain bass tv rate &key video (pause-before 16000) (pause-after 16000))
  (nipkow-make-filtered-wav name gain bass tv rate)
  (nipkow-make-conversion name tv rate))
