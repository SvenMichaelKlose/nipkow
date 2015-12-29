(load "bender/vic-20/cpu-cycles.lisp")

(defun pwm-pulse-rate (tv &key (shortest audio_shortest_pulse) (width audio_pulse_width))
  ; XXX Need INTEGER here because tré's FRACTION-CHARS is buggered.
  (integer (/ (? (eq tv :pal)
                 +cpu-cycles-pal+
                 +cpu-cycles-ntsc+)
              (* 8 (+ audio_shortest_pulse (half audio_pulse_width))))))

(defun print-pwm-info ()
  (format t "Nipkow player audio resolution: ~A cycles~%" (* 8 audio_pulse_width))
  (format t " ~A Hz (NTSC)~%" (pwm-pulse-rate :ntsc))
  (format t " ~A Hz (PAL)~%" (pwm-pulse-rate :pal)))

(defun unsigned (x)
  (+ x (? (< 127 x) -128 128)))

(defun unclip (x range)
  (integer (+ (half (- 16 range)) (* (/ x 16) range))))

(defun wav2pwm (out in &key video (pause-before 16000) (pause-after 16000))
  (adotimes 44 (read-byte in))
  (adotimes pause-before
    (write-byte audio_average_pulse out))
  (awhile (read-word in)
          nil
    (let sample (bit-xor (>> ! 12) 8)
      (write-byte (+ audio_shortest_pulse sample) out))
    (when (& *video?*
             (read-byte video))
      (write-byte video out)))
  (adotimes pause-after
    (write-byte audio_average_pulse out)))

; XXX Not used anywhere.
(defun wav42pwm (out in-file)
  (alet (fetch-file in-file)
    (dotimes (i (length !))
      (alet (elt ! i)
        (write-byte (+ audio_shortest_pulse (>> ! 4)) out)
        (write-byte (+ audio_shortest_pulse (bit-and ! 15)) out)))))
