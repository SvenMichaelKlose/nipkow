(= *model* :vic-20)

(defvar *tv* :pal)
;(defvar *tunes*         '(:mario))
;(defvar *tv-standards*  '(:pal ))
(defvar *tunes*         '(:ohne-dich :mario))
(defvar *tv-standards*  '(:pal :ntsc))

(defvar *video?* nil) ;"video.mp4")
(defvar *video-end* "20")
(defvar *irq?* t)
(defvar *nipkow-disable-interrupts?* t)
(defvar *nipkow-fx-border?* t)
(defvar *make-test-pulses?* t)
(defvar *make-wav?* nil)
(defvar *nipkow-inversions?* t)

(defvar *nipkow-pulse-rate* 4000)
(defvar *bandwidth* 16)
(defvar frame_sync_width #x40)

(load "src/convert.lisp")
(load "src/make-nipkow-dat.lisp")

(defun tune? (x)
  (member x *tunes*))

(defun tv-standard? (x)
  (member x *tv-standards*))

(defun make-model-detection ()                                                  
  (make "obj/model-detection.bin"
        '("src/models.asm"
          "src/model-detection.asm")
        "obj/model-detection.vice.txt"))

(when *video?*
  (format t "Generating frame images of video ~A with mplayer…~%" *video?*)
  (| (file-exists? *video?*)
     (error "Couldn't find *VIDEO?* ~A." *video?*))
  (sb-ext:run-program "/usr/bin/mplayer"
                      `("-really-quiet"
                        "-ao" "dummy"
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

(when (tune? :ohne-dich)
  (make-audio "ohne_dich" (| *video?* "media/ohne_dich.mp3") "0" "-56"))
(when (tune? :mario)
  (make-audio "mario" "media/mario.flv" "3" "-56"))

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

;(defvar ohne_dich nil)
(defvar text nil)

(defun make-audio-player (name src tv)
  (= *tv* tv)
  (alet (downcase (symbol-name tv))
    (let tapname (+ "compiled/" src "." ! ".tap")
      (make-audio-player-prg src !)
      (with-output-file o tapname
        (write-tap o
            (bin2cbmtap (cddr (string-list (fetch-file (+ "obj/" src "." ! ".prg"))))
                        (+ (padded-name name)
                           (fetch-file "obj/model-detection.bin"))
                        :start #x1001))
        (when *make-test-pulses?*
          (adotimes *nipkow-pulse-rate*
            (write-byte (nipkow-average-pulse) o))
          (dotimes (j 8)
            (adotimes ((quarter *nipkow-pulse-rate*))
              (? *nipkow-inversions?*
                 (progn
                   (write-byte (+ (nipkow-shortest-pulse) j) o)
                   (write-byte (- (nipkow-longest-pulse) j) o)
                   (write-byte (+ (nipkow-shortest-pulse) j) o)
                   (write-byte (- (nipkow-longest-pulse) j) o))
                 (progn
                   (write-byte (+ (nipkow-shortest-pulse) j) o)
                   (write-byte (+ (nipkow-shortest-pulse) j) o)
                   (write-byte (- (nipkow-longest-pulse) j) o)
                   (write-byte (- (nipkow-longest-pulse) j) o))))))
        (with-input-file i (+ "obj/" src ".downsampled." ! ".wav")
          (= (stream-track-input-location? i) nil)
          (? *video?*
             (with-input-file video "obj/nipkow.dat"
               (= (stream-track-input-location? video) nil)
               (wav2pwm o i video :invert? *nipkow-inversions?*))
             (wav2pwm o i :invert? *nipkow-inversions?*))))
      (when *make-wav?*
        (with-io i tapname
                 o (+ "compiled/" src "." ! ".tap.wav")
          (tap2wav i o 44100 (cpu-cycles *tv*))))
      (sb-ext:run-program "/usr/bin/zip" (list (+ tapname ".zip") tapname)))))

(make-model-detection)
(when (tune? :ohne-dich)
  (when (tv-standard? :pal)
    (make-audio-player "OHNE DICH (PAL)" "ohne_dich" :pal))
  (when (tv-standard? :ntsc)
    (make-audio-player "OHNE DICH (NTSC)" "ohne_dich" :ntsc)))
(when (tune? :mario)
  (when (tv-standard? :pal)
    (make-audio-player "MARIO (PAL)" "mario" :pal))
  (when (tv-standard? :ntsc)
    (make-audio-player "MARIO (NTSC)" "mario" :ntsc)))

(format t "Done. See directory 'compiled/'.~%")
(quit)
