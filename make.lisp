(= *model* :vic-20)

(load "bender/vic-20/cpu-cycles.lisp")

(defvar *video?* nil) ;"video.mp4")
(defvar *video-end* "20")
(defvar *irq?* t)
(defvar *nipkow-disable-interrupts?* t)
(defvar *nipkow-fx-border?* t)
(defvar *mario-pal-only?* nil)

(defvar *bandwidth* 16)
(defvar audio_shortest_pulse #x18)
(defvar audio_longest_pulse #x28)
(defvar frame_sync_width #x40)
(defvar audio_pulse_width (- audio_longest_pulse audio_shortest_pulse))
(defvar audio_average_pulse (+ audio_shortest_pulse (half audio_pulse_width)))

(load "src/wav2pwm.lisp")
(load "src/make-nipkow-dat.lisp")

(when *video?*
  (format t "Generating frame images of video ~A with mplayer…~%" *video?*)
  (| (file-exists? *video?*)
     (error "Couldn't find *VIDEO?* ~A." *video?*))
  (sb-ext:run-program "/usr/bin/mplayer"
                      `("-ao" "dummy"
                        "-vo" "pnm"
                        "-vf" "scale=64:48"
                        "-endpos" ,*video-end*
                        ,*video?*))
  (format t "Generating tape frames…~%")
  (make-nipkow-dat-test "obj/nipkow.dat" "."))

(defun make-wav (name tv file gain bass)
  (format t "Generating uncompressed audio for ~A (~A) of file ~A with mplayer…~%"
          name (symbol-name tv) file)
  (sb-ext:run-program "/usr/bin/mplayer"
                      `("-vo" "null"
                        "-vc" "null"
                        ,@(& *video?* `("-endpos" ,*video-end*))
                        "-ao" ,(+ "pcm:fast:file=obj/" name ".wav")
                        ,file))
  (format t "Filtering and companding ~A (~A) with sox…~%"
          name (symbol-name tv))
  (sb-ext:run-program "/usr/bin/sox"
                      `(,(+ "obj/" name ".wav")
                        ,(+ "obj/" name ".filtered.wav")
                        "bass" ,bass
                        "lowpass" ,(princ (half (pwm-pulse-rate tv)) nil)
                        "compand" "0.3,1" "6:-70,-60,-20" "-1" "-90" "0.2"
                        "gain" ,gain)))

(defun make-conversion (name tv)
  (format t "Downsampling ~A (~A) with sox…~%" name (symbol-name tv))
  (alet (+ "obj/" name ".downsampled." (downcase (symbol-name tv)) ".wav")
    (sb-ext:run-program "/usr/bin/sox"
                        `(,(+ "obj/" name ".filtered.wav")
                          "-c" "1"
                          "-b" "16"
                          "-r" ,(princ (/ (pwm-pulse-rate tv)
                                          (? *video?* 2 1))
                                       nil)
                          ,!))))

(defun make-audio (name file gain bass)
  (make-wav name :pal file gain bass)
  (make-conversion name :pal)
  (make-wav name :ntsc file gain bass)
  (make-conversion name :ntsc))

(unless *mario-pal-only?*
  (make-audio "ohne_dich" (| *video?* "media/ohne_dich.mp3") "0" "-56"))
(make-audio "mario" "media/mario.flv" "4" "-56")

(defun make (to files cmds)
  (apply #'assemble-files to files)
  (make-vice-commands cmds "break .stop"))

(defun make-audio-player-prg (name tv)
  (make (+ "obj/" name "." tv ".prg")
        `("bender/vic-20/basic-loader.asm"
          "bender/vic-20/vic.asm"
          "src/main.asm"
          ,@(? *video?*
              '("src/video/video-player.asm"
                "src/video/luminance-chars.asm")
              (? *irq?*
                 '("src/audio-player-irq.asm")
                 '("src/audio-player-noirq.asm")))
          ,(+ "src/text_" name ".asm"))
        (+ "obj/" name "." tv ".prg.vice.txt")))

(defun padding (x n obj)
  (maptimes [identity obj] (- n (length x))))

(defun padded-name (x)
  (list-string (+ (string-list x) (padding x 16 #\ ))))

(defvar *tv* nil)
(defvar ohne_dich nil)
(defvar text nil)

(defun make-audio-player (name src tv)
  (= *tv* tv)
  (alet (downcase (symbol-name tv))
    (let tapname (+ "compiled/" src "." ! ".tap")
      (make-audio-player-prg src !)
      (with-output-file o tapname
        (write-tap o
            (bin2cbmtap (cddr (string-list (fetch-file (+ "obj/" src "." ! ".prg"))))
                        name
                        :start #x1001))
        (with-input-file i (+ "obj/" src ".downsampled." ! ".wav")
          (? *video?*
             (with-input-file video "obj/nipkow.dat"
               (wav2pwm o i video))
             (wav2pwm o i))))
      (with-input-file i tapname
        (with-output-file o (+ "compiled/" src "." ! ".tap.wav")
          (tap2wav i o 48000 (cpu-cycles *tv*))))
      (sb-ext:run-program "/usr/bin/zip" (list (+ tapname ".zip") tapname)))))

(unless *mario-pal-only?*
  (make-audio-player "OHNE DICH (PAL)" "ohne_dich" :pal)
  (make-audio-player "OHNE DICH (NTSC)" "ohne_dich" :ntsc))
(make-audio-player "MARIO (PAL)" "mario" :pal)
(unless *mario-pal-only?*
  (make-audio-player "MARIO (NTSC)" "mario" :ntsc))

(print-pwm-info)
(format t "Done. See directory 'compiled/'.~%")
(quit)
