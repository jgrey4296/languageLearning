;; simple camera manipulation
;; functions of importance:
;; mrotate mtranslate mscale
;; set-camera-transform get-camera-transform


(show-axis 1)

;;absolute examples:
(set-camera-transform (mtranslate (vector 0 0 -10)))
(set-camera-transform (mmul (mtranslate (vector 0 0 -10))
                            (mrotate (vector 0 45 0))))

;;relative examples:
;;is able to be used in every-frame
(set-camera-transform (mmul (get-camera-transform)
                            (mrotate (vector 0 45 0))))


