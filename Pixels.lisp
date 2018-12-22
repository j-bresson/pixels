;--------------------------------------------------
; Pixels
; Picture and pixel array manipulations in OM 6
; (c) J. Bresson 2010/2011
;--------------------------------------------------

(in-package :om)

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'(lambda (filename) 
          (compile&load (om-relative-path '("sources") filename)))
      '("pixmaps" "picture-lib"))

;--------------------------------------------------
;filling packages
;--------------------------------------------------

(fill-library  '(("From OM" nil nil (save-picture) nil)
                 ("Image" nil (picture-lib) (get-rgb picture-size gen-pixmap gen-pixmap-xy scale-pixmap get-bitmap) nil)
                 ("Pixels" nil nil (get-pixel map-pixels map-lines pix->nb pix->rgb pix->bit) nil)
                ))


(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(om::set-lib-release 1.1)
              