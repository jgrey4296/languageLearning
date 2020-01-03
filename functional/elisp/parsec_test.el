;; A File to explore using the parsec elisp library

(require 'parsec)

(defun jg-spaces ()
  (parsec-optional* (parsec-re "[[:space:]]*")))

(defun jg-par ()
  (parsec-one-of ?\( ?\)))

(defun jg-word (&optional opts)
  (let (result)
    (jg-spaces)
    (setq result (parsec-re "[[:alpha:]]+"))
    (jg-spaces)
    (if (or (null opts) (-contains? opts result))
        result)))


(defun mytest ()
  (let ((vals '()))
    (if (jg-par) (push 'a vals))
    (setq vals (append vals (parsec-many (jg-word))))
    (if (jg-par) (setq vals (append vals '(b))))
    vals
    ))

(defun mypair ()
  (let (a b)
    (setq a (mytest))
    (jg-spaces)
    (setq b (mytest))
    (-zip-pair a b)
    ))

(parsec-with-input "(this is hello) (a b c)"
  (mypair))


(parsec-with-input "this is a test."
  (let ((vals (parsec-many1 (jg-word))))
    (parsec-ch ?.)
    (seq-filter 'identity vals)))
