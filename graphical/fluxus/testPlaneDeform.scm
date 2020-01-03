;; A Simple script to create a plane and deform it
(clear)
;; make a colour vector
(define (rndcvec)
  (let (
        (basevec (reverse (vector->list (rndvec))))
        )
    (list->vector (reverse (cons 1.0 basevec)))
    )
  )

;;--------------------
;; Main
(let* ( ;;vars
       (theplane (build-plane))
       )

  ;;select the plane
  (with-primitive theplane
                  ;; for each point
                  (for [(n (build-list (pdata-size) values))]
                       (begin
                         (pdata-set! "p" n (vmul (pdata-ref "p" n) (rndf)))
                         (pdata-set! "c" n (rndcvec))
                         )
                       )
                  )
  )

