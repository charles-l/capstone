#lang racket

(require scribble/base
         scribble/core
         (only-in srfi/1 cons*))

(provide graphviz ttt)

(define (ttt . lines)
  (list (linebreak)
        (map (lambda (x)
               (list (tt x) (linebreak)))
             (filter-not (compose char-whitespace?
                                  (curryr string-ref 0))
                         lines))))


(define (graphviz . str)
  (let* ((f (make-temporary-file "rkttmp~a.png"))
         (cmd (~a "printf \"" (string-replace (apply string-append str) "\""
                                              "\\\"")  "\" | dot -Tpng > " f)))
    (display cmd)
    (newline)
    (system cmd)
    (image (~a f) #:scale 0.5)))

(system "rm rkttmp*.png") ; do cleanup
