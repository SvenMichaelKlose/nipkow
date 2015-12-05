(load "bender/vic-20/cpu-cycles.lisp")

(defun pwm-pulse-rate (tv)
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

(defun wav2pwm (out in-file &key video (pause-before 16000) (pause-after 16000))
  (alet (+ (list-string (maptimes [identity audio_average_pulse] pause-before))
           (fetch-file in-file)
           (list-string (maptimes [identity audio_average_pulse] pause-after)))
    (dotimes (i (length !))
      (unless (zero? (mod i 2))
        (let sample (unclip (/ (unsigned (elt ! i)) 16) *bandwidth*)
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
