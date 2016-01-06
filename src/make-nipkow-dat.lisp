; nipkow – Copyright (c) 2015 Sven Michael Klose <pixel@hugbox.org>

(defun frame-name (path x)
  (alet (format nil "~A" x)
    (list-string (+ (string-list (+ path "/"))
                    (maptimes [identity #\0] (- 8 (length !)))
                    (string-list !) (string-list ".ppm")))))

(defun frame-sync (out)
  (princ (code-char (+ (nipkow-longest-pulse) frame_sync_width)) out))

(defun downscale-frame (!)
  (with-queue q
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
            (enqueue q v)))))
    (queue-list q)))

(defun make-frame (out x)
  (with (min 8
         max 8)
    (dolist (i x)
      (? (< i min)
         (= min i))
      (? (< max i)
         (= max i)))
    (format t "Contrast ~A/~A~%" min max)
    (frame-sync out)
    (let phase nil
      (dolist (i x)
        (princ (code-char (alet (unclip (* 16 (/ (- i min) (- max min))) *bandwidth*)
                            (prog1 (+ (nipkow-shortest-pulse)
                                      (? phase
                                         !
                                         (- 15 !)))
                              (toggle phase))))
               out)))))

(defun make-nipkow-dat (pathname frames-path)
  (with-output-file out pathname
    (alet 1
      (loop
        (let frame (frame-name frames-path !)
          (| (file-exists? frame)
             (return))
          (format t "Writing tape frame ~A…~%" !)
          (make-frame out (downscale-frame (fetch-file frame)))
          (+! ! 3))))))

(defun make-nipkow-dat-test (pathname frames-path)
  (with-output-file out pathname
    (alet 1
      (dotimes (i 800)
        (format t "Writing tape frame ~A…~%" i)
        (make-frame out (apply #'+ (maptimes [maptimes #'identity 16] 16)))))))
