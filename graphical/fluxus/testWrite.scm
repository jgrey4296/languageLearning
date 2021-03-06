;; A Simple file write
;;consts
(define home (path->string (find-system-path `home-dir)))
(define writePath (string-append home "github/languageLearning/fluxus/"))

(define (writeToFile file contents)
  (let ([outFile (open-output-file (string-append writePath file) #:exists `truncate)])
    (begin 
      (for ([line contents]) (display line outFile) (display "\n" outFile))
      (close-output-port outFile))))
(writeToFile "testFile" `("this" "is" "a" "longer test blah"))

