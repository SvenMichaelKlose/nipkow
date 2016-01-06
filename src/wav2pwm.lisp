(load "bender/vic-20/cpu-cycles.lisp")

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
