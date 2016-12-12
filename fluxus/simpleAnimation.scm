;; A Simple set of rotating cubes in a grid
(clear)

;;Create the cubes
(define (create-cubes)
  (let* [( cubelist `() )
         ( numOfCubes 10)
         ( indexList (build-list numOfCubes values))
        ]
    ;;for the enumeration:        
    (for [(i indexList)]
         (let [(currentCube (build-cube))]
           ;;offset by index:
           (with-primitive currentCube
                           (translate (vector i 0 0))
                           (colour (vector (rndf) (rndf) (rndf)))
                           )
           ;;store in list:
           (set! cubelist (cons currentCube cubelist))
           )
         )
    cubelist
    )
  )

;; take a list of primitives
(define (animate cList)
  (let [( yRange 0.1 )]
    (for [(c cList)]
         (with-primitive c ;;grab each and translate vertically by time
                         (translate (vector 0 (* yRange (sin (+ (* 20 c ) (time)))) 0))
                         )
         )
    )
  )

(let [(cList (create-cubes))]
  (every-frame (animate cList))
  )
