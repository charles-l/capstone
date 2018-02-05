#lang scribble/lp2
@(require scribble-code-examples)

Since @tt{yacc} and @tt{lex} are specialized domain specific languages with
their own grammars and syntaxes, so they stand out in source since they don't follow
the outer language's semantics.

TODO: Talk about parsec and parasck @cite{Leijen}

To address this issue and make more readable code, compiler developers will
often use @bold{parser combinators} which are libraries made of higher-order
functions that allow them to construct parsers using functions in the semantics
of the language the compiler is being developed in.

Parser combinators are just libraries that define a structure for a successfully
parsed result, along with a few functions that can be combined in interesting
ways.

A parser function consumes input from some stream, and returns whether the
parser's rule successfully matched the result. For instance, a parser function
could match the letter "a" character.

@(define ev (make-code-eval #:lang "racket"))
@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (require parsack)
}|

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (char #\a) "a")
}|

Of course, there are more general parser functions for matching a character we
don't know.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse $letter "a")
  (parse $digit "3")
  (parse $digit "a")
  (parse (string "match this!") "match this! 1 2 3")
}|

Building a parser from these few simple functions would get tedious quickly
though. If we can match multiple characters with regular expressions, why can't
we do the same with a parser?

This is where combinators come in. Combinators are higher-order functions that
apply other functions together to build larger functions @cite{Tromp} (TODO:
move this reference to the lambda calculus section).

A basic combinator matches @tt{many} parser results (until it fails).

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (many $letter) "abcde123")
}|

We can expand our collection of combinators with conditionals:

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (many (<or> $letter $digit)) "abcde123---")
}|

Sequences of parsers are combined together with the bind function (denoted
@tt{>>=}). The bind function is a higher-order function that takes a parser
and a function. The function is expected to have the signature @tt{parse-result
-> parser}, and the bind function executes the returned parser and returns the
final result. So the full signature for @tt{>>=} is:

@tt{>>= : parser -> (result -> parser) -> result}

Essentially, the bind constructs a pipeline that passes along state so you don't
lose the result of what the previous parser parsed.

So if we want to parse an excited word, we could parse many letters followed by
an exclamation point.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (>>= (many $letter)) "woot!")
}|


@code-examples[#:lang "at-exp racket" #:context #'here]|{
(parse-result $anyChar "a")
(parse-result $digit "a")
(parse-result $letter "a")
(parse-result (<or> $letter $digit) "a")
(parse-result (many $letter) "abc123")
}|

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
   (return j)))]

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

@chunk[<expr-list>
        (define $expr-list
          (sepBy $expr (parser-seq $spaces (char ",") $spaces)))]

@chunk[<arg-list>
  (define $arg-list
   (sepBy (parser-seq $identifier (~ $spaces) $identifier)
    (parser-seq $spaces (char ",") $spaces)))]

TODO: write macro that inserts (~ $spaces) for me

@chunk[<var-decl>
        (define $var-decl
          (parser-seq $identifier $identifier))]

@chunk[<return>
        (define $return (parser-one
                          (string "return")
                          $spaces
                          (~>
                            (>>= $expr (lambda (r)
                                         (return (list 'return r)))))))]

@chunk[<for-stmt>
  (define $for
    (parser-seq
      (~ (string "for")) (~ $spaces)
      (~ (char #\()) (~ $spaces)
      $expr (~ $spaces)
      (~ (char #\;)) (~ $spaces)
      $expr (~ $spaces)
      (~ (char #\;)) (~ $spaces)
      $expr (~ $spaces)
      (~ (char #\))) (~ $spaces)
      (~ (char #\{)) (~ $spaces)
      $expr-list
      (~ (char #\})) (~ $spaces)
      ))]

@chunk[<if-stmt>
  (define $if
    (parser-seq
      (~ (string "if")) (~ (char #\()) (~ $spaces)
      $expr (~ $spaces)
      (~ (char #\))) (~ $spaces)
      (~ (char #\{))
      $stmt-list
      (~ (char #\})) (~ $spaces)
      (<or> (parser-seq
             (~ (string "else"))
             (~ (char #\{)) (~ $spaces)
             $stmt-list
             (~ (char #\})) (~ $spaces)
            (return '())))))]

@chunk[<stmt-list>
  (define $stmt-list
    (endBy $stmt (parser-seq $spaces (char #\;) $spaces)))]

@chunk[<func-def>
  (define $func-def
    (parser-seq
      $identifier
      $spaces
      $identifier
      $spaces
      (char #\()
      $spaces
      $arg-list
      $spaces
      (char #\)) (~ $spaces)
      (char #\{) (~ $spaces)
      $stmt-list
      (char #\})
    ))
]

Most of this code is fairly similar - the difference between the parser
combinator code and the @tt{yacc} parser is the parser combinator is using most
of the internal language features. While it requires more code in certain places
(particularly with parsing and ignoring spaces), it's a more intuitive way of
writing parser code for a functional programmer.

While the @tt{yacc} parser was declarative as well, it didn't expose any lambdas
or language functions to the user, instead relying on a series of sophisticated
macros (which can make debugging more difficult). All a programmer has to
understand to write a parser combinator is the monad bind operator (@tt{>>=})
and a few low-level parsers (like @tt{char}). From there, they can combine and
use any functions they want, and can even extend the functionality by adding
their own parsers or combinators.

