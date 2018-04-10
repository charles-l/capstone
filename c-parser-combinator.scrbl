#lang scribble/lp2
@(require scribble-code-examples)
@(require scriblib/footnote)
@(require "util.rkt")

Since @tt{yacc} and @tt{lex} are specialized domain specific languages with
their own grammars and syntaxes, they stand out in source since they don't
follow the outer language's semantics. For Racket, this isn't as much of
an issue since the parser DSL looks like Racket code. However, with the
original @tt{lex} and @tt{yacc} implementations for C, the @tt{lex} and
@tt{yacc} code was separated out from the rest of the source, and the
tools would generate temporary C files that weren't easy to hand-edited.

When so much code is generated, its difficult to control what gets
generated and how its written. Often languages that original have a parser
implemented in @tt{yacc} eventually deprecate the @tt{yacc} parser for one
that is hand written to address these issues.

@italic{Parser combinators} address this issue without losing the
declarative nature of a DSL. Parser combinators are parser libraries that
are used to construct parsers using functional composition of
higher-order functions. It's a natural way of thinking for functional
programmers. The resulting code is terse, provides lower-level control
than a DSL, and is easy to maintain.

Parsec is one of the first real-world parser combinators @cite{Leijen,
2001}. It is written in Haskell, but has inspired implementations in many
other languages including OCaml and Racket. We'll use the Racket
implementation (called Parsack) for our parser combinator.

A parser function consumes input from some stream, and returns a result if
parser rule successfully parsed anything. For instance, a parser function
could match the letter "a."

@(define ev (make-code-eval #:lang "racket"))
@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (require parsack)
  (parse (char #\a) "a")
}|

There are more general parser functions for matching any character or
digit. We can also match whole strings.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse $letter "a")
  (parse $digit "3")
  (parse $digit "a")
  (parse (string "match this!") "match this! 1 2 3")
}|

Building a parser with only these few simple functions would get tedious
quickly. If we can attempt to match multiple characters with regular
expressions, we should be able to do the same with a parser.

This is where combinators come in. Combinators are higher-order functions
that take other functions as arguments and apply them together to reuse
compose functionality @cite{Tromp}.

A basic combinator is @tt{many}, which parses using whatever parser it is
passed until it fails.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (many $letter) "abcde123")
}|

We can also use conditional combinators to try another parser if the first
one fails.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (many (<or> $letter $digit)) "abcde123---")
}|

Sequences of parsers are combined together with the bind function (denoted
@tt{>>=}). The bind function is a higher-order function that takes
a parser and a transformation function function. The transformation
function is expected to have the type signature @tt{parse-result ->
parser}. The bind function calls the transform function, and executes the
returned parser, returning the final result. So the full type signature
for @tt{>>=} is:

@tt{>>= : parser -> (result -> parser) -> result}

Essentially, the bind function is used to construct a pipeline of
transformations that track state so you don't lose the result of the
previous parser.

So if we want to parse an excited word, we could parse many letters followed by
an exclamation point.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (>>= (many $letter)
              (lambda (previous-letters)
                (char #\!)))
         "woot!")
}|

However, since @tt{previous-result} isn't used, @tt{>>=} only returns the
secound parse result. We need to combine the results and return it. In
the context of parser combinators the @tt{return} function doens't have
the same meaning as the return statement in most languages. It
doesn't terminate execution and return the result immediately - instead it
simply packages up whatever value was passed to it as a parse result and
passes returns it.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
(parse (return "i don't care about what needs to be parsed!") "plz parse me")
}|

Since it always succeeds, it can be used with conditions to return
a default result.

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
(parse (<or> (many1 $digit) (return "oops - no digits")) "no numbers here, dood")
}|

So in our case of trying to combine the previous results, we want to return the
combined result of two parsers

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (parse (>>= (many $letter)
              (lambda (previous-letters)
                (>>= (char #\!)
                     (lambda (exclamation-point)
                       (return (append previous-letters (list exclamation-point)))))))
         "woot!")
}|

Using the simple concept of parse results and function composition, we can
then build the rules we need for our parser. Before we start parsing,
there are a few more utility functions that will be useful.

@itemlist[@item{@tt{>>} (bind, ignoring returned result)
                        @code-examples[#:lang "racket" #:eval ev #:context #'here]|{
                        (parse (>> (string "don't care") (many1 $digit))
                               "don't care123123")
                        }|
                        When we want to ensure a keyword appears, but don't care
                        about returning the result in the parse tree, this
                        function is very useful.
                        }
                        @item{@tt{parser-seq} and @tt{parser-one}
                        (parse a sequence of parsers, combining specified
                               results)
                        @code-examples[#:lang "racket" #:eval ev #:context #'here]|{
                        (parse (parser-seq
                                 (many1 $letter)
                                 (many1 $digit)
                                 (many1 (char #\!)))
                               "awesom3!!")

                        (parse (parser-seq
                                 (~ (many1 $letter))
                                 (many1 $digit)
                                 (many1 (char #\!)))
                               "awesom3!!")

                        (parse (parser-one
                                 (many1 $letter)
                                 (~> (many1 $digit))
                                 (many1 (char #\!)))
                               "awesom3!!")
                        }|
                        Both these functions just do the bind calls internally and
                        hide the lambdas to make the code easier to read.
                        @tt{parser-seq} will combine the parse results into a
                        list, ignoring any parsers with a @tt{~} in front of it,
                        and @tt{parser-one} is the exact same, but it only
                        returns one result (whichever parser is prefixed with @tt{~>}).
                        }

                        @item{@tt{sepBy} and @tt{endBy} (parse seperators)
                        @code-examples[#:lang "racket" #:eval ev #:context
                                       #'here]|{
                        (parse (sepBy (many1 $letter) $spaces)
                               "such words many wow")
                        (parse (endBy (many1 $letter) (char #\,))
                               "expect,a,comma,bru,")
                        }|

                        These functions are fairly self-explanitory - they
                        parse the first parser until they reach the seperator
                        (the second argument) and repeat until the end.
                        @tt{sepBy} expects no seperator at the end and @tt{endBy}
                        expects the separator.
                        }
           ]

There are many more combinators that can be found in the parsack
documentation @cite{Parsack docs}, but this list contains the main
combinators we use for the parser we build.

Again, we're going to parse a C-ish language, since it has enough non-trivial
syntax rules to be interesting.

@chunk[<includes>
(require parsack)
]

A parse "rule" could be defined as a function in a parser combinator. For
instance, a number parser would be written,

@chunk[<number>
(define $number
  (<or>
    (>>= (try (parser-seq (many $digit) (char #\.) (many $digit)))
        (lambda (r)
          (return (string->number (list->string (append (car r) (list (cadr r)) (caddr r)))))))
    (>>= (many $digit)
        (lambda (r)
          (return (string->number (list->string r)))))))
]@note{In this case, the angle bracketed function @tt{<or>} is the actual function
name (not a literate programming reference to a different block).}

Parser combinators look like regular function calls in language. Besides
a few odd function names, they're fairly self-explanatory. The rule above
parses either a series of digits followed by a dot followed by a series of
digits (for doubles) or a series of digits (for integers)

Likewise, parsing strings and characters is straightforward.

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

The @tt{$identifier} rule is built into Parsack and is convenient
for parsing variable and function identifiers. However, it returns an
array of characters, when it would be nicer to return a string. We'll
write a function that does this conversion for us.

@chunk[<sidentifier>
        (define $sidentifier (>>= $identifier
                                  (lambda (v)
                                    (return (list->string v)))))]

Now, a rule for parsing variable assignment.

@chunk[<var-assign-ugly>
(define $var-assign-ugly
 (>>=
  (parser-seq
   $sidentifier
   (~ $spaces)
   (char #\=)
   (~ $spaces)
   $expr))
 (lambda (j)
   (return j)))]

It's going to get annoying annoying having to parse out the @tt{$spaces}
after every word in the expression. We'll write a macro to insert optional
spaces between every parser for us.

@chunk[<intersperse-spaces>
(require (for-syntax racket/match))
(require (for-syntax (except-in racket string)))
(define-syntax (intersperse-spaces stx)
  (letrec ((intersperse
	     (match-lambda**
	       (('() _) '())
	       (((list x) _) `(,x))
	       (((cons a b) sep) (cons a (cons sep (intersperse b sep)))))))
    (let ((l (syntax->datum stx)))
      (match l
	     ((list _ f args ...)
	       (datum->syntax stx (cons f (intersperse args '(~ $spaces)))))
	     (else
	       (error 'intersperse-spaces
                      "expecting function and arguments to intersperse"))))))
]

Now we can use the @tt{intersperse-spaces} macro to rewrite the variable
assignment rule.

@chunk[<var-assign>
(>>=
  (intersperse-spaces
    parser-seq
    $sidentifier
    (char #\=)
    $expr)
  (lambda (j)
    (return (append (list '= (car j)) (caddr j)))))]

Function calls are simply:

@chunk[<funcall>
(intersperse-spaces
  parser-seq
  $sidentifier
  (between (char #\() (char #\)) (sepBy $expr (char #\,))))]

Infix operators (also known as binary operators) are trickier to implement
in a parser combinator than in @tt{yacc}. The following rule will not
work.

@chunk[<binop-bad>
(parser-compose
  (lhs <- $expr)
  $spaces
  (op <- (oneOf "+-/*<>"))
  $spaces
  (rhs <- $expr)
  (return (list op lhs rhs)))
  ]

If we try to do this, we will run into a problem. The @tt{lhs} references
an @tt{$expr}, which is the rule we're currently defining. We're allowed
to recursively reference the current rule later in the rule (when we use
the @tt{parser-seq} or @tt{parser-compose} macros, since they expand to
lambda functions which delay the evaluation of reference @tt{$expr}).
However, we cannot reference it as the first term in the rule. Recursively
referencing the current rule as the first term isn't allowed since it is
a form of left-recursion. To eliminate left-recursion, we'll use the
algorithm from the @cite{Dragon book} (Section 4.3.3.).

@ttt|{
for each production in the grammar:
  if the production has the form A -> AX | Y:
    (where X, Y are any number of terminals or nonterminals that do not begin with A)
    replace the production with the following:
      A -> YA'
      A' -> XAA' | ε
}|

Without left recursion, our infix operator rule looks like:

@chunk[<binop>
        (define $binop
          (parser-compose
            (op <- (oneOf "+/*<>"))
            $spaces
            (rhs <- $expr)
            (return (cons (string->symbol (list->string (list op))) rhs))))
        ]

Our full expression rule looks like this:

@chunk[<expr>
        (define $expr-binopable
          (<or>
            (try <funcall>)
            $char
            $number
            $sidentifier))

        (define $expr (<or>
                        (try <var-assign>)
                        $string
                        (>>=
                          (parser-seq
                            $expr-binopable
                            (<or> (try $binop) (return '())))
                            (lambda (v)
                              (return (apply cons v))))
                        ))]

The @tt{$expr-binopable} parser defines expressions that @italic{might}
have a binary operation after parsing the term. This is to prevent the
possiblity of binary operations after something like a variable assignment
which doesn't make much sense.

In the @tt{$expr} rule, we try to parse a binary operation, and return an
empty list if it fails to parse. We've successfully eliminated
left-recursion!

When two rules reference each other recursively (like @tt{$binop} and
@tt{$expr} in this case) we say they're mutually recursive. Mutual
recursion can become an issue in some situations. For instance, it can
lead to indirect left-recursion which is more subtle than the direct
left-recursion we've seen. But often when building parsers, mutual
recursion is useful for expressing certain parse rules.

Now we'll define expression and argument lists.

@chunk[<expr-list>
        (define $expr-list
          (sepBy $expr (parser-seq $spaces (char #\,) $spaces)))]

@chunk[<arg-list>
  (define $arg-list
   (sepBy (parser-seq $sidentifier (~ $spaces) $sidentifier)
    (parser-seq $spaces (char #\,) $spaces)))]

We'll also include variable declarations.

@chunk[<var-decl>
        (define $var-decl
          (>>=
            (intersperse-spaces parser-seq $sidentifier $sidentifier)
            (lambda (v)
              (return (cons 'def v)))))]

Similar to @tt{$identifier}, Parsack's built-in @tt{string} function
returns a list of characters rather than a string. Rather than converting
back to a string every time we call the function, we'll write another
helper function to parse this way for us.

@chunk[<sstring>
(define (sstring s)
  (>>=
    (string s)
    (compose return list->string)))]

We'll use the @tt{sstring} function for our return statement.

@chunk[<return>
        (define $return (parser-one
                          (sstring "return")
                          $spaces
                          (~>
                            (>>= $expr (lambda (r)
                                         (return (list 'return r)))))))]

If statements require a bit more code, but are fairly simple to understand
as well.

@chunk[<if-stmt>
        (define $if
          (intersperse-spaces
            parser-seq
            (>>= (sstring "if") (lambda _ (return 'if)))
            (between (char #\() (char #\)) $expr)
            (~ (char #\{)) $stmt-list (~ (char #\}))
            (<or> (intersperse-spaces
                    parser-seq
                    (>>= (sstring "else") (lambda _ (return 'else)))
                    (~ (char #\{)) $stmt-list (~ (char #\})))
                  (return '()))
            (~ $spaces)))]

We'll use statement lists inside of our blocks.

@chunk[<stmt-list>
  (define $stmt (<or> (try $if)
                      (parser-one
                        (~> (<or> (try $return) (try $var-decl) $expr))
                        $spaces
                        (char #\;)
                        $spaces)))
  (define $stmt-list
    (many1 $stmt))]

And finally, our top-level function definition rule is written like so:

@chunk[<func-def>
  (define $func-def
    (>>=
      (intersperse-spaces
        parser-seq
        $sidentifier
        $sidentifier
        (~ (char #\()) $arg-list (~ (char #\)))
        (~ (char #\{))
        $stmt-list
        (~ (char #\})))
      (lambda (r) (return (cons 'fun r))))) ]


We'll combine it into a file with the following structure.

@chunk[<*>
(provide parse-c)
<includes>
<intersperse-spaces>
<sidentifier>
<sstring>

<number>
<string>
<char>
<binop>
<expr>
<expr-list>
<arg-list>
<var-decl>
<return>
<if-stmt>
<stmt-list>
<func-def>

(define (parse-c s)
  (parse-result $func-def s))
]

Most of this code is similar to the @tt{yacc} parser code - the difference
between the two is the parser combinator is using more "Racket-isms" and
language features. While it requires more code in certain places, it's an
intuitive way of writing parser code for a functional programmer, while
providing more control than @tt{yacc}.

While the @tt{yacc} parser was declarative as well, it didn't expose any
lambdas or language functions to the user, instead relying on a series of
sophisticated macros (which can make debugging more difficult). After
learning about the low-level bind operator (@tt{>>=}) and some parsers
(like @tt{char}) a programmer can reason about a parser combinator and
build whatever abstractions they need. They can combine and use any
functions they want, and can easily extend the functionality with their
own combinators.

The parser combinator is capable of parsing a complex program like:

@code-examples[#:lang "racket" #:eval ev #:context #'here]|{
  (require "c-parser-combinator.scrbl")
  (parse-c "int main(int a){
              long x;
              x = 2;
              if(x){
                return a;
              } else {
                x = 3;
              }
              return f(1+a*x);
            }")
}|
