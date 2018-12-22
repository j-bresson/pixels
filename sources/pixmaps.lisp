;--------------------------------------------------
; Pixels
; Librairie pour la manipulation de Pixmaps dans OM 6.1
; (c) J. Bresson 2009
; pour Shanta Rao
;--------------------------------------------------

(in-package :om)

(defmethod! picture-size ((self picture))
   :initvals nil
   :indoc '("a picture object")
   :numouts 2
   :doc "Returns the width (first output) and height (second output) of the picture."
   :icon '(491)
   (let ((size (om-get-picture-size (thepict self))))
     (values (om-point-h size) (om-point-v size))))


(defmethod! get-RGB ((self picture))
   :initvals nil
   :indoc '("a picture object")
   :doc "Returns an array (list of lists) with all (R G B alpha) values of the pixels in <self>"
   :icon '(491)
   (om-picture-values (thepict self)))

(defmethod! get-RGB-array ((self picture))
   :initvals nil
   :indoc '("a picture object")
   :doc "Returns an array (list of lists) with all (R G B alpha) values of the pixels in <self>"
   :icon '(491)
   (om-picture-array (thepict self)))

(defmethod! get-pixel ((pixmap picture) x y)
  :initvals '(nil 0 0)
  :indoc '("picture or RGB array" "x (horizontal) index" "y (vertical) index")
  :icon '(491)
  :doc "Returns the pixel at <x>,<y> in <pixmap>."
  (get-pixel (get-RGB pixmap)))

(defmethod! get-pixel ((pixmap list) x y)
  :icon '(491)
  (nth x (nth y pixmap)))


(defmethod! pix-R ((pixel list))
  :icon 100
  (car pixel))

(defmethod! pix-G ((pixel list))
  :icon 101
  (cadr pixel))

(defmethod! pix-B ((pixel list))
  :icon 102
  (caddr pixel))

(defmethod! pix-alpha ((pixel list))
  :icon 103
  (cadddr pixel))

(defmethod! gen-pixmap (x y pixval)
     :initvals '(10 10 nil)
     :icon '(491)
     :indoc '("width" "height" "default value or function")
     :doc "Returns a pixmap"
     (loop for i from 0 to (1- y) collect
           (loop for j from 0 to (1- x) collect 
                 (if (null pixval) 0
                   (if (or (consp pixval) (numberp pixval)) 
                       pixval
                     (funcall pixval))))))
                   
(defmethod! gen-pixmap-xy (x y pixfun)
     :initvals '(10 10 nil)
     :icon '(491)
     :indoc '("width" "height" "default value or function")
     :doc "Returns a pixmap"
     (loop for i from 0 to (1- y) collect
           (loop for j from 0 to (1- x) collect 
                 (if (null pixfun) 0
                   (if (or (consp pixfun) (numberp pixfun)) 
                       pixfun
                     (funcall pixfun j i))))))


(defmethod! scale-pixmap ((pixmap list) fact)
     :initvals '(nil 1)
     :icon '(491)
     :indoc '("a picture or RGB pixmap" "a multiplicative factor (integer)")
     :doc "Returns a modified pixmap"
     (unless (integerp fact)
       (om-beep-msg "!!! multiplicative factor for scale-pixmap rounded to closest integer value.")
       (setf fact (round fact)))
     (if (plusp fact)
         (loop for line in pixmap append
           (let ((lll (loop for pix in line append 
                            (loop for i from 1 to fact do collect (if (consp pix) (copy-list pix) pix)))))
             (loop for i from 1 to fact do collect (copy-list lll))))
       (let ((h0 (length pixmap))
             (w0 (length (car pixmap)))
             (fff (- fact)))
       (loop for l from 0 to (1- (floor h0 fff)) collect
             (loop for pix from 0 to (1- (floor w0 fff)) collect 
                   (fusion-pixels 
                    (flat (mapcar #'(lambda (line) (subseq line (* pix fff) (* (1+ pix) fff)))
                                  (subseq pixmap (* l fff) (* (1+ l) fff))) 
                          1)))))
       ))
    

(defun fusion-pixels (list)
  (if (consp (car list))
      (mapcar 'om-mean (mat-trans list))
    (om-mean list)))

(defmethod! scale-pixmap ((pixmap picture) fact)
  (scale-pixmap (get-rgb pixmap) fact))


(defmethod! map-pixels ((self list) function)
     :initvals '(nil nil)
     :icon '(491)
     :indoc '("a picture or RGB pixmap" "a function to apply to each pixel")
     :doc "Returns a modified pixmap"
     (if function
         (mapcar #'(lambda (line) (mapcar #'(lambda (pixel) (funcall function pixel)) line)) self)
       self))

(defmethod! map-pixels ((self picture) function)
  (map-pixels (get-rgb self) function))       

;; marche pas bien
(defmethod! map-pixels ((self array) function)
     :initvals '(nil nil)
     :icon '(491)+
     :indoc '("a picture or RGB pixmap" "a function to apply to each pixel")
     :doc "Returns a modified pixmap"
     (when function
       (dotimes (i (array-dimension self 0))
         (dotimes (j (array-dimension self 1))
           (let* ((pixel (list (/ (aref self i j 2) 255.0) (/ (aref self i j 1) 255.0) (/ (aref self i j 0) 255.0) (/ (aref self i j 3) 255.0)))
                  (newpix (om-round (om* (funcall function pixel) 255)))
                  (pixvals (if (consp newpix) newpix (list newpix newpix newpix 1))))
             (setf  (aref self i j 0) (caddr pixvals)
                    (aref self i j 1) (cadr pixvals)
                    (aref self i j 2) (car pixvals)
                    (aref self i j 3) (or (cadddr pixvals) 1))
             ))))
     self)
     

(defmethod! map-pixels ((self picture) function)
  (oa::ensure-pict-win)
  (let* ((pict2 (copy-picture self))
         (img (oa::om-internalize-image (thepict pict2)))
         (ia (gp::make-image-access oa::*temp-pictlayout* img))
         (w (gp:image-access-width ia))
         (h (gp:image-access-height ia)))
    (setf (source pict2) nil
          (pict-pathname pict2) nil)
    (gp::image-access-transfer-from-image ia)
    (loop for j from 0 to (- h 1) collect
          (loop for i from 0 to (- w 1) 
                do 
                (let* ((pix (color::get-color-spec (color::ensure-rgb (color::unconvert-color oa::*temp-pictlayout* (gp::image-access-pixel ia i j)))))
                      (pix2 (apply function (list (list (aref pix 1) (aref pix 2) (aref pix 3) (if (> (length pix) 4) (aref pix 4) 1.0))))))
                  (setf (gp::image-access-pixel ia i j)
                        (color::convert-color oa::*temp-pictlayout* (if (consp pix2)
                                                                  (color::make-rgb (nth 0 pix2) (nth 1 pix2) (nth 2 pix2) (nth 3 pix2))
                                                                      (color::make-rgb pix2 pix2 pix2 1)))))))
    (gp::image-access-transfer-to-image ia)
    (gp::free-image-access ia)
    (setf (thepict pict2) (oa::om-externalize-image img))
    pict2
    ))



(defmethod! map-lines ((self list) function)
     :initvals '(nil nil)
     :icon '(491)
     :indoc '("a picture or RGB pixmap" "a function to apply to each pixel")
     :doc "Returns a modified pixmap"
     (if function
         (mapcar #'(lambda (line) (funcall function line)) self)
       list))

(defmethod! map-lines ((self picture) function)
  (map-lines (get-rgb self) function))      


(defmethod! pix->nb (pixel)
     :initvals '(nil)
     :icon '(491)
     :indoc '("a pixel value")
     :doc "converts to a single gray-scale value"
     (if (numberp pixel) pixel
       (/ (+ (car pixel) (cadr pixel) (caddr pixel)) 3)))

(defmethod! pix->rgb (pixel)
     :initvals '(nil)
     :icon '(491)
     :indoc '("a pixel value")
     :doc "converts to a RGB-formatted pixel"
     (if (numberp pixel) (list pixel pixel pixel 1.0) pixel))


(defmethod! pix->bit (pixel &optional seuil)
     :initvals '(nil nil)
     :icon '(491)
     :indoc '("a pixel value" "a threshold value")
     :doc "converts to a bit map (0 / 1)"
     (if seuil
         (if (< (pix->nb pixel) seuil) 0 1)
       (round (pix->nb pixel))))

(defmethod! get-bitmap (pixmap &optional seuil)
     :initvals '(nil nil)
     :icon '(491)
     :indoc '("a picture of pixmap" "a threshold value")
     :doc "converts to a bit map (0 / 1)"
     (map-pixels pixmap #'(lambda (pix) (pix->bit pix seuil))))



