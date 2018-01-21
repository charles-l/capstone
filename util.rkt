#lang racket

(require scribble/base
         scribble/core
         (only-in srfi/1 cons*))


(provide code)

(define (fill-groups li endi)
  (define (fill-inner l)
    (cond
      ((or (null? l) (null? (cdr l)))
       l)
      (else
        (cons*
          (car l)
          (cons
            (cdr (car l))
            (car (cadr l)))
          (fill-inner (cdr l))))))
  (append
    (if (zero? (caar li))
      '()
      `(,(cons 0 (caar li))))
    (fill-inner li)
    (let ((last-pos (cdr (last li))))
      (if (< last-pos endi)
        `(,(cons last-pos endi))
        '()))))

(define (convert-whitespace s)
  (let ((m (regexp-match-positions* #rx"[ ][ ]+" s)))
    (if (and m (not (null? m)))
      (map
        (lambda (x)
          (let ((ss (substring s (car x) (cdr x))))
            (if (eq? #\space (string-ref ss 0))
              (hspace (string-length ss))
              ss)))
        (fill-groups m (string-length s)))
      s)))

(define (code . lines)
  (nested #:style 'code-inset
          (map (lambda (x)
                 (if (equal? x "\n")
                   (linebreak)
                   (tt (convert-whitespace x)))) lines)))
