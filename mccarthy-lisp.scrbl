#lang scribble/lp2
@(require scribble-code-examples)

Lisp is extermely simple to implement in Lisp itself. A classic implementation
can be found in @cite{SICP} (Chapter 4, the Metacircular Evaluator), which
defines the interpreter elegently in terms of @tt{eval} and @tt{apply}, allowing
the language implementor to leverage internal functions in the host language to
evaluate code. Later chapters implement the interpreter in terms of a virtual
machine.

However, the original Lisp interpreter written in Lisp is only a single page
(reproduced and ported to Racket below) @cite{McCarthy Lisp}:

@code-examples[#:lang "racket" #:context #'here]{
    (define (atom? x) (not (list? x)))
    (define (assoc e a)
     (cond
      ((null? a) '())
      ((eq? e (caar a)) (car a))))
    (define (lisp-eval e a)
     (cond ((atom? e)
            (cond
             ((eq? e '()) '())
             ((eq? e 't) #t)
             ((number? e) e)
             (else
              (cdr (assoc e a)))))
      ((atom? (car e))
       (cond
        ((eq? (car e) 'quote)  (cadr e))
        ((eq? (car e) 'car)    (car (lisp-eval (cadr e) a)))
        ((eq? (car e) 'cdr)    (cdr (lisp-eval (cadr e) a)))
        ((eq? (car e) 'cadr)   (cadr (lisp-eval (cadr e) a)))
        ((eq? (car e) 'caddr)  (caddr (lisp-eval (cadr e) a)))
        ((eq? (car e) 'caar)   (caar (lisp-eval (cadr e) a)))
        ((eq? (car e) 'cadar)  (cadar (lisp-eval (cadr e) a)))
        ((eq? (car e) 'caddar) (caddar (lisp-eval (cadr e) a)))
        ((eq? (car e) 'atom?)  (atom? (lisp-eval (cadr e) a)))
        ((eq? (car e) 'null?)  (null? (lisp-eval (cadr e) a)))
        ((eq? (car e) 'cons)   (cons (lisp-eval (cadr e) a)
          (lisp-eval (caddr e) a)))
        ((eq? (car e) 'eq?)    (eq? (lisp-eval (cadr e) a)
          (lisp-eval (caddr e) a)))
        ((eq? (car e) '*)    (* (lisp-eval (cadr e) a)
          (lisp-eval (caddr e) a)))
        ((eq? (car e) '-)    (- (lisp-eval (cadr e) a)
          (lisp-eval (caddr e) a)))
        ((eq? (car e) 'cond)
         (letrec ((evcond
                   (lambda (u a)
                    (cond ((lisp-eval (caar u) a)
                           (lisp-eval (cadar u) a))
                     (else (evcond (cdr u) a))))))
          (evcond (cdr e) a)))
        (else (lisp-eval (cons (cdr (assoc (car e) a)) a)))))
        ((eq? (caar e) 'lambda)
         (lisp-eval (caddar e)
          (letrec ((ffappend (lambda (u v)
                              (cond ((null u) v)
                               (else (cons (car u)
                                      (ffappend (cdr u) v))))))
                   (pairup (lambda (u v)
                            (cond
                             ((null? u) '())
                             (else
                             (cons (cons (car u) (car v))
                                    (pairup (cdr u) (cdr v)))))))
                   (evlis (lambda (u a)
                           (cond ((null? u) '())
                            (else (cons (lisp-eval (car u) a)
                                   (evlis (cdr u) a)))))))
           (ffappend
            (pairup
             (cadar e)
             (evlis (cdr e) a))
            a))))
        ((eq? (caar e) 'label)
         (lisp-eval (cons (caddar e) (cdr e))
          (cons (cons (cadar e) (car e)) a)))))

          (lisp-eval '(car '(a)) '())
          (lisp-eval '(atom? (cdr '(a))) '())
          (lisp-eval '(null? ()) '())
          (lisp-eval '(null? '()) '())
          (lisp-eval '(eq? 1 1) '())
          (lisp-eval '1 '())
          (lisp-eval '(cond
                       ((eq? 1 1) 1)
                       (t (* 1 (- 1 1))))
           '())
          (lisp-eval '(lambda (n)
                       (cond
                        ((eq? n 1) 1)
                        (t (* n (- n 1)))))
           '())
          (lisp-eval '((lambda (n)
                        (cond
                         ((eq? n 1) 1)
                         (t (* n (- n 1))))) 7)
          '())
          (lisp-eval '((label fib (lambda (n)
                                   (cond
                                    ((eq? n 1) 1)
                                    (t (* n (fib (- n 1))))))) 7)
          '())
}

