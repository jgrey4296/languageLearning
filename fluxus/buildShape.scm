;;a simple build-shape script
(clear)
;;triplets of positions:
(define instructions `((-1 0 0) (-1 1 0) (-1 2 0) (-1 3 0)
                       (0 3 0)
                       (1 3 0) (1 2 0) (1 1 0) (1 0 0)))

;;for the instructions, build a cube for each position,
;;then translate each as necessary
(define (buildObj ins)
  (let ([paired (map (lambda (x) `(,(build-cube) ,x)) ins)])
    (for ([c paired])
         (let ([instruction (cadr c)])
           (with-primitive (car c)
                           (translate (vector (car instruction) (cadr instruction) (caddr instruction))))))))

;;build an object by the instructions
(buildObj instructions)

;;build a plane, rotated appropriately
(with-primitive (build-plane)
                (rotate (vector 90 0 0)))
;;todo: deform the plane size as necessary
