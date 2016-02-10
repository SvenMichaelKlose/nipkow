(= *model* :vic-20)

(defvar *video?* nil) ;"video.mp4")
(defvar *video-end* "20")
(defvar *irq?* t)
(defvar *nipkow-disable-interrupts?* t)
(defvar *nipkow-fx-border?* t)
(defvar *mario-pal-only?* nil)
(defvar *make-test-pulses?* nil)

(defvar *nipkow-pulse-rate* 4400)
(defvar *bandwidth* 16)
(defvar frame_sync_width #x40)

(load "src/convert.lisp")
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

(defun make-audio (name file gain bass)
  (nipkow-make-wav name file)
  (nipkow-convert name gain bass :pal *nipkow-pulse-rate*)
  (nipkow-convert name gain bass :ntsc *nipkow-pulse-rate*))

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
          (when *make-test-pulses?*
            (dotimes (j 8)
              (adotimes ((half *nipkow-pulse-rate*))
                (write-byte (+ (nipkow-shortest-pulse) j) o)
                (write-byte (- (nipkow-longest-pulse) j) o))))
          (? *video?*
             (with-input-file video "obj/nipkow.dat"
               (wav2pwm o i video))
             (wav2pwm o i))))
      (with-input-file i tapname
        (with-output-file o (+ "compiled/" src "." ! ".tap.wav")
          (tap2wav i o 44100 (cpu-cycles *tv*))))
      (sb-ext:run-program "/usr/bin/zip" (list (+ tapname ".zip") tapname)))))

(unless *mario-pal-only?*
  (make-audio-player "OHNE DICH (PAL)" "ohne_dich" :pal)
  (make-audio-player "OHNE DICH (NTSC)" "ohne_dich" :ntsc))
(make-audio-player "MARIO (PAL)" "mario" :pal)
(unless *mario-pal-only?*
  (make-audio-player "MARIO (NTSC)" "mario" :ntsc))

(format t "Done. See directory 'compiled/'.~%")
(quit)
