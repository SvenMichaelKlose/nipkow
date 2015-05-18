; nipkow – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defun frame-name (path x)
  (alet (format nil "~A" x)
    (list-string (+ (string-list (+ path "/"))
                    (maptimes [identity #\0] (- 8 (length !)))
                    (string-list !) (string-list ".ppm")))))

(defun frame-sync (out)
  (princ (code-char (+ audio_longest_pulse frame_sync_width)) out))

(defun make-frame (out frame)
  (alet (fetch-file frame)
    (frame-sync out)
    (let phase nil
      (dotimes (y 16)
        (dotimes (x 16)
          (let ofs (+ #x0d (* y 64 3 3) (* x 3 4))
            (let v (integer (/ (+ (elt ! ofs)
                                  (elt ! (+ ofs 1))
                                  (elt ! (+ ofs 2))
                                  (elt ! (+ ofs 3))
                                  (elt ! (+ ofs 4))
                                  (elt ! (+ ofs 5))
                                  (elt ! (+ ofs 6))
                                  (elt ! (+ ofs 7))
                                  (elt ! (+ ofs 8))
                                  (elt ! (+ ofs 9))
                                  (elt ! (+ ofs 10))
                                  (elt ! (+ ofs 11)))
                               3 4 16))
              (when (< 15 v)
                (error "Luminance of ~A.~%" v))
              (when (< v 0)
                (error "Luminance of ~A.~%" v))
              (= v (unclip v 12))
              (princ (code-char (+ audio_shortest_pulse
                                   (? phase
                                      (- 15 v)
                                      v)))
                     out)
              (toggle phase))))))))

(defun make-nipkow-dat (pathname frames-path)
  (with-output-file out pathname
    (alet 1
      (loop
        (let frame (frame-name frames-path !)
          (| (file-exists? frame)
             (return))
          (format t "Writing tape frame ~A…~%" !)
          (make-frame out frame)
          (++! !))))))
