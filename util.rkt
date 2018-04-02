#lang racket

(require scribble/base
         scribble/core
         (only-in srfi/1 cons*)
         sha)

(provide graphviz ttt)

(define (ttt . lines)
  (list (linebreak)
        (map (lambda (x)
               (list (tt x) (linebreak)))
             (filter-not (compose char-whitespace?
                                  (curryr string-ref 0))
                         lines))))


(define (graphviz . str)
  (define filehash (bytes->hex-string (sha1 (string->bytes/locale (string-join str)))))
  (define file (format "/tmp/rkttmp~a.png" filehash))
  (unless (file-exists? file)
    (define cmd (~a "printf \"" (string-replace (apply string-append str) "\""
                                                "\\\"")  "\" | dot -Tpng > " file))
    (display cmd)
    (newline)
    (system cmd))
  (image (~a file) #:scale 0.5))
