#lang scribble/lp2
@(require scribble-code-examples)

Since @tt{yacc} and @tt{lex} are specialized domain specific languages with
their own grammars and syntaxes, so they stand out in source since they don't follow
the outer language's semantics.

To address this issue and make more readable code, compiler developers will
often use @bold{parser combinators} which are libraries made of higher-order
functions that allow them to construct parsers using functions in the semantics
of the language the compiler is being developed in.

TODO: explain examples

@code-examples[#:lang "at-exp racket" #:context #'here]|{
(parse-result $anyChar "a")
(parse-result $digit "a")
(parse-result $letter "a")
(parse-result (<or> $letter $digit) "a")
(parse-result (many $letter) "abc123")
|}

@chunk[<includes>
(require parsack)
]

A "rule" (which is really a function) to parse numbers could be
defined:

@chunk[<number>
(define $number
  (<or>
   (>>= (parser-seq (many $digit) (char #\.) (many $digit))
        (lambda (r)
          (return (string->number (list->string r)))))
   (>>= (many $digit)
        (lambda (r)
          (return (string->number (list->string r)))))))
]

In this case, the angle bracketed function @tt{<or>} is the literal function
name (not a reference to a different programming block in this book).

Parser combinators look like regular code in the language. Besides a few odd
looking functions, they're fairly self-explanatory. The rule above parses either
a series of digits followed by a dot followed by a series of digits (for
doubles) or a series of digits (for integers)

@chunk[<string>
(define $string
 (>>=
  (>> (char #\")
   (manyUntil $anyChar (char #\")))
  (lambda (p) (return (list->string p)))))]

@chunk[<char>
(define $char
	(parser-one
            (char #\')
            (~> $anyChar)
            (char #\')))]

@chunk[<var-assign>
(define $var-assign
 (>>=
  (parser-seq
   $identifier
   (~ $spaces)
   (char #\=)
   (~ $spaces)
   $expr))
 (lambda (j)
   (return j))))]

FIXME: verify that this isn't recurseive (it might be)
@chunk[<binop>
(define $binop
 (>>= (parser-seq
       $expr
       (~ $spaces)
       (oneOf "+-/*<>")
       (~ $spaces)
       $expr)
   (lambda (r) r)))]

@chunk[<funcall>
(define $funcall
  (>>= (parser-seq $identifier $spaces (between (char #\() (char #\))
                                                (sepBy (char #\,) $expr)))
       (lambda (r)
         ; FIXME put identifier in front
         r)))]

@chunk[<expr>
        (define $expr (<or> $var-assign $binop $funcall))]

@chunk[<return>
        (define $return (parser-one
                          (string "return")
                          $spaces
                          (~>
                            (>>= $expr (lambda (r)
                                         (return (list 'return r))))))]
