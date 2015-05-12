(defvar *video?* nil)

(defvar audio_shortest_pulse #x18)
(defvar audio_longest_pulse #x28)
(defvar frame_sync_width #x08)
(defvar audio_pulse_width (- audio_longest_pulse audio_shortest_pulse))

(defvar *pulse-short* #x20)
(defvar *pulse-long* #x40)
(defvar *tape-pulse* (* 8 (+ *pulse-short* (half (- *pulse-long* *pulse-short*)))))
(load "src/wav2pwm.lisp")
(load "src/make-nipkow-dat.lisp")

(when *video?*
  (sb-ext:run-program "mplayer" '("-ao" "dummy" "-vo" "pnm" "-vf" "scale=64:48" "-endpos" "120" "video.mp4")))

(defun make-wav (name file gain bass)
  (sb-ext:run-program "/usr/bin/mplayer"
    (list "-vo" "null" "-vc" "null" "-ao" (+ "pcm:fast:file=obj/" name ".wav") file))
  (sb-ext:run-program "/usr/bin/sox"
    (list (+ "obj/" name ".wav")
          (+ "obj/" name "_filtered.wav")
          "bass" bass
          "lowpass" "2000"
          "compand" "0.3,1" "6:-70,-60,-20" "-5" "-90" "0.2" "gain" gain)))

(defun make-conversion (name tv)
  (sb-ext:run-program "/usr/bin/sox"
    (list (+ "obj/" name "_filtered.wav")
          "-c" "1"
          "-b" "16"
          "-r" (princ (pwm-pulse-rate tv) nil)
          (+ "obj/" name "_downsampled_" (downcase (symbol-name tv)) ".wav"))))

(defun make-audio (name file gain bass)
  (make-wav name file gain bass)
  (make-conversion name :pal)
  (make-conversion name :ntsc))

(make-audio "ohne_dich" "media/ohne_dich.mp3" "4" "-56")
(make-audio "mario" "media/mario.flv" "4" "-56")

(defun make (to files cmds)
  (apply #'assemble-files to files)
  (make-vice-commands cmds))

(defun make-ohne-dich-prg (name tv)
  (make (+ "obj/" name "_" tv ".prg")
        `("bender/vic-20/basic-loader.asm"
          "bender/vic-20/vic.asm"
          "src/main.asm"
          "src/audio-player.asm"
          ,(+ "src/text_" name ".asm"))
        (+ "obj/" name "_" tv ".prg.vice.txt")))

(defun padded-name (x)
  (list-string (+ (string-list x) (maptimes [identity #\ ] (- 16 (length x))))))

(defvar *tv* nil)
(defvar ohne_dich nil)
(defvar text nil)

(defun make-ohne-dich (name tv)
  (= *tv* tv)
  (alet (downcase (symbol-name tv))
    (let tapname (+ "compiled/" name "_" ! ".tap")
      (make-ohne-dich-prg name !)
      (make-vice-commands (+ "compiled/" name "_" ! ".vice.txt"))
      (with-output-file o tapname
        (write-tap o
            (bin2cbmtap (cddr (string-list (fetch-file (+ "obj/" name "_" ! ".prg"))))
                        name
                        :start #x1001))
        (? *video?*
           (with-input-file video "nipkow.dat"
             (wav2pwm o (+ "obj/" name "_downsampled_" ! ".wav") video))
           (wav2pwm o (+ "obj/" name "_downsampled_" ! ".wav"))))
      (sb-ext:run-program "/usr/bin/zip" (list (+ tapname ".zip") tapname)))))

(when *video?*
  (sb-ext:run-program "/usr/bin/mplayer" '("-ao" "dummy" "-vo" "pnm" "-vf" "scale=64:48" "-endpos" "120" "video.mp4")))

(make-ohne-dich "ohne_dich" :pal)
(make-ohne-dich "ohne_dich" :ntsc)
(make-ohne-dich "mario" :pal)
(make-ohne-dich "mario" :ntsc)
(print-pwm-info)

(defun tap-rate (tv avg-len)
  ; XXX Need INTEGER here because tré's FRACTION-CHARS is buggered.
  (integer (/ (? (eq tv :pal)
                 +cpu-cycles-pal+
                 +cpu-cycles-ntsc+)
              (* 8 avg-len))))

(alet (+ *pulse-short* (half (- *pulse-long* *pulse-short*)))
  (format t "Baud rates: ~A (NTSC), ~A (PAL)~%"
          (tap-rate :ntsc !) (tap-rate :pal !)))
(format t "Done. See directory 'compiled/'.~%")

(quit)
