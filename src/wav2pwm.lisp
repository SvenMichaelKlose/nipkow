(load "bender/vic-20/cpu-cycles.lisp")

(defun pwm-pulse-rate (tv)
  ; XXX Need INTEGER here because tré's FRACTION-CHARS is buggered.
  (integer (/ (? (eq tv :pal)
                 +cpu-cycles-pal+
                 +cpu-cycles-ntsc+)
              (* 8 (+ audio_shortest_pulse (half audio_pulse_width))))))

(defun print-pwm-info ()
  (format t "Audio resolution: ~A cycles~%" (* 8 audio_pulse_width))
  (format t "~A pulses per second (PAL).~%" (pwm-pulse-rate :pal))
  (format t "~A pulses per second (NTSC).~%" (pwm-pulse-rate :ntsc)))

(defun unsigned (x)
  (+ x (? (< 127 x) -128 128)))

(defun unclip (x range)
  (integer (+ (half (- 16 range)) (* (/ x 16) range))))

(defun wav2pwm (out in-file &optional video)
  (alet (fetch-file in-file)
    (dotimes (i (length !))
      (unless (zero? (mod i 2))
        (let sample (unclip (/ (unsigned (elt ! i)) 16) 14)
          (& (| (< sample 0)
                (< 15 sample))
             (error "Sample ~A." sample))
          (let pulse (+ audio_shortest_pulse sample)
            (princ (code-char pulse) out)))
        (when (& *video?*
                 (peek-char video))
          (princ (read-char video) out))))))

(defun wav42pwm (out in-file)
  (alet (fetch-file in-file)
    (dotimes (i (length !))
      (alet (elt ! i)
        (princ (code-char (+ audio_shortest_pulse (>> ! 4))) out)
        (princ (code-char (+ audio_shortest_pulse (bit-and ! 15))) out)))))
