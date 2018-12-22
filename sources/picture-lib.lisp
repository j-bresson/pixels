(in-package :om)

(defclass! picture-lib () 
   ((pict-list :accessor pict-list :initarg :pict-list :initform nil)
    (storemode :accessor storemode :initform nil))
   (:icon '(491)))


(defmethod omNG-save ((self picture-lib) &optional (values? nil))
  (if (storemode self) 
      `(let ((obj ,(call-next-method)))
         (setf (storemode obj) t)
         obj)
    `(make-instance 'picture-lib)))


(defmethod class-has-editor-p ((self picture-lib)) t)

(defmethod get-editor-class ((self picture-lib)) 'picturelibeditor)

(defmethod good-val-p? ((self picture-lib)) t)

(defclass picturelibeditor (editorview) 
  ((current-pict :initform 0 :accessor current-pict)
   (controlview :initform nil :accessor controlview)))

(defclass picturelibpanel (om-view) ())

(defclass picturelibcontrols (3Dborder-view) ())

(defmethod editor ((self picturelibpanel)) (om-view-container self))

(defmethod get-help-list ((self picturelibeditor)) 
  '(((tab "Next Pict"))))

(defmethod initialize-instance :after ((self picturelibeditor) &rest args)
  (om-add-subviews self
                   (setf (panel self) (om-make-view 'picturelibpanel
                                                    :position (om-make-point 0 0)
                                                    :size (om-make-point (w self) (- (h self) 40))))
                   (setf (controlview self) (om-make-view 'picturelibcontrols
                                                          :position (om-make-point 0 (- (h self) 40))
                                                          :size (om-make-point (w self) 40)
                                                          :bg-color *om-light-gray-color*))))

(defmethod update-subviews ((self picturelibeditor))
  (let ((ctrl-h 30))
  (om-set-view-size (panel self) (om-make-point (w self) (- (h self) ctrl-h)))
  (om-set-view-size (controlview self) (om-make-point (w self) ctrl-h))
  (om-set-view-position (controlview self) (om-make-point 0 (- (h self) ctrl-h)))))

(defmethod om-draw-contents ((self picturelibpanel))
  (let ((pict (when (and (object (editor self)) (numberp (current-pict (editor self))))
                (nth (current-pict (editor self)) (pict-list (object (editor self)))))))
    (om-with-focused-view self
      (when pict
        (om-draw-picture self (thepict pict) :pos (om-make-point 0 0) 
                         :size (om-view-size self))))))


(defmethod om-draw-contents ((self picturelibcontrols))
  (call-next-method)
  (let ((pict (when (and (object (om-view-container self)) (numberp (current-pict (om-view-container self))))
                (nth (current-pict (om-view-container self)) (pict-list (object (om-view-container self)))))))
      (om-with-focused-view self
        (om-with-font *om-default-font1b*
          (om-with-fg-color self *om-dark-gray-color*
        (when (and pict (name pict))
          (om-draw-string 20 20 (name pict)))
        (om-draw-string (- (w self) 100) 20 (format nil "Picture ~D" (current-pict (om-view-container self)))))))))


(defmethod handle-key-event ((self picturelibpanel) key)
  (case key 
    (#\h (show-help-window "PictureLib Editor commands..." (get-help-list (editor self))))
    (:om-key-tab 
     (when (pict-list (object (editor self)))
       (if (current-pict (editor self)) 
           (setf (current-pict (editor self)) 
                 (mod (+ (current-pict (editor self)) 1) (length (pict-list (object (editor self))))))
         (setf (current-pict (editor self)) (- (length (pict-list (object (editor self)))) 1)))
       (om-invalidate-view (controlview (editor self)))))
    (:om-key-delete nil))
  (om-invalidate-view self))



;====================================================
;BOX and frame

(defmethod object-box-specific-menu ((self picture-lib) box)
  (list (om-new-leafmenu (if (storemode self) "Do Not Save Picture-Lib Data with Patch" "Save Picture-Lib Data with Patch")
                   #'(lambda () 
                       (setf (storemode self) (not (storemode self))))
                   nil)))

