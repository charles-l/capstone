#lang scribble/lp2
@(require scribble-code-examples)

Since @tt{yacc} and @tt{lex} are specialized domain specific languages with
their own grammars and syntaxes, so they stand out in source since they don't follow
the outer language's semantics.

To address this issue and make more readable code, compiler developers will
often use @bold{parser combinators} which are libraries made of higher-order
functions that allow them to construct parsers using functions in the semantics
of the language the compiler is being developed in.

For instance, a "rule" (which is really a function) to parse numbers could be
defined:

@chunk[<includes>
(require parsack)
]

@chunk[<number-parser>
(define digit
  (<or>
   (>>= (parser-seq (many $digit) (char #\.) (many $digit))
        (lambda (r)
          (return (list 'DOUBLE (string->number r)))))
   (>>= (many $digit)
        (lambda (r)
          (return (list 'INT (string->number r)))))))
]

In this case, the angle bracketed function @tt{<or>} is the literal function
name (not a reference to a different programming block in this book).

Parser combinators look like regular code in the language. Besides a few odd
looking functions, they're fairly self-explanatory. The rule above parses either
a series of digits followed by a dot followed by a series of digits (for
doubles) or a series of digits (for integers)

@chunk[<>
(define c-parser
 ())
]
