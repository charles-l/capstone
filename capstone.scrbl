#lang scribble/book

@(require scribble-code-examples)
@(require scribble/lp-include)
@(require scriblib/footnote)
@(require scriblib/figure)
@(require "util.rkt")

@title{Programming Language Implementation}
@author{Charles Saternos}

@(table-of-contents)

@section{Background}

TODO: quick racket guide

TODO: regular expressions

Like spoken languages, every programming language has its own set of features and
quarks. Many programming languages vary wildly in syntax (older and newer
languages). Newer languages almost all have C-style or ALGOL-style syntax, while
many older languages had hideous syntax that was difficult to read.

However, syntax is simply a surface level detail of a programming language.
Well formatted code isn't necessarily good code, and it's not really where the
differences lie in how programming languages operate. The real differences are
in in the languages semantics.

Semantics are a broad topic, but there are a few important distinctions to keep
in mind.

@itemlist[
@item{Functional vs imperative vs object oriented}
@item{Strong typing vs weak typing}
@item{Static typing vs dynamic typing}
@item{Lazy vs strict evaluation}
]

@subsection{Programming paradigm}

While there are many programming paradigms besides functional, object-oriented
(OO), and imperative (e.g. dataflow, logic, or concatenative), and there's a great
deal of overlap even between paradigms (e.g. a logic programming library could
be embedded in a functional language), functional, OO, and imperative are the
most common paradigms. Newer programming languages, tend to be
distinguished by one of these three paradigms. While there's certainly some dataflow
code running somewhere, if you pick a random sample of industry applications
you're more likely to find Java code than Oz code.

Imperative (or procedural) programming languages rely on mutation and
side-effects. Reassigning variables and destructively updating data structures
are examples of mutation, and are expected in imperative languages. Imperative
programming is popular in systems languages, since it maps closely to how modern
CPU's are designed (which makes performance tuning fairly transparent).
It's also often paired with object-oriented programming, since most
object-oriented languages rely on a web of mutating objects.

@figure[
    "Imperative code example"
    "Racket tends to discourage this type of imperative programming, more often opting for the
    functional paradigm which is why this code is a bit awkward"]{
    @code-examples[#:lang "at-exp racket" #:context #'here]|{
        (define x 0)

            (define (add-to-x! i)
             (set! x (+ x i)))

            (add-to-x! 2)

            (print x)
    }|
}

Object oriented code focuses on defining functionality and state data together, in an attempt
to mimic objects in the real world (I have a coffee machine that is turned off
(state) and I can turn in it on to and myself a delicious cup of brew
(functionality)). While the object oriented model was popular in the 1990s, some
of its limitations have come to light recently. In particular, object-oriented
programming tends to lead to tightly-coupled components, that don't encourage reuse
and it is difficult to parallelize a programs that are object-oriented but don't
use the actor model.

@code-examples[#:lang "at-exp racket" #:context #'here]|{
    (define coffee-maker-class%
      (class object%
        (field (cups-made 0) (status 'off))
        (define/public (turn-on!)
          (set! status 'on))
        (define/public (make-coffee!)
          (when (eq? status 'off)
            (error "Coffee maker must be turned on to make coffee"))
          (set! cups-made (+ 1 cups-made)))
        (define/public (turn-off!)
          (set! status 'off))
        (define/public (get-cups-made)
          cups-made)
        (super-new)))

    (define my-coffee-maker (make-object coffee-maker-class%))

    (send my-coffee-maker make-coffee!)

    (send my-coffee-maker turn-on!)
    (send my-coffee-maker make-coffee!)
    (send my-coffee-maker make-coffee!)
    (send my-coffee-maker make-coffee!)
    (send my-coffee-maker turn-off!)

    (send my-coffee-maker get-cups-made)
}|

The final paradigm is called functional programming, and is the paradigm that is
focused on the most in this book. In functional programming, functions are
defined in the mathematical sense - they are pure in the sense that they only
have inputs and outputs without mutation. For instance, a function to append to
a list would be implemented so it constructed an entirely @italic{new} list from
scratch with the new element appended onto the end. Often, functional languages
have immutable values, meaning it's invalid to reassign a value. This tends to
lead to code that's easy to reason about since state isn't a factor that has to
be considered. Functional programming is superior to object-oriented programming
from a code reuse standpoint, since functional composition is encouraged which
leads to small, general functions that work well together.

@code-examples[#:lang "at-exp racket" #:context #'here]|{
    (define l '(a b c d e f))
    (append l 'x)
    l

    (define j '(1 2 3 4 5 6))
    (map add1 j)
    j
}|

Functional programming does have its faults. For instance, state-heavy code
tends to be more confusing for beginner programmers since state updates might be hidden in
recursion or a monad. However, since many programs don't really need to track
state and can be written as a series of functions chained together in a
pipeline, functional programming is an important paradigm to understand.

@subsection{Strong vs weak typing}
@subsection{Static vs dynamic typing}
@subsection{Lazy vs strict evaluation}

TODO: lambda calculus

@section{Parsing and Semantic Analysis}

TODO: give analogy

The first step taken by any interpretor or compiler is parsing. The
parsing step converts the plain-text source code into IR (intermediate
representation) - usually a tree of some sort. The tree can then be passed
onto another compilation pass to do semantic analysis, interpretation,
code generation or some other transformation.

Of course the parser needs a valid source file to be able to convert into
a tree, so in translating the source to an IR, it checks the syntax of the
code. If the syntax is invalid, it can emit a syntax error, and either
fail or attempt to continue parsing the rest of the file (if it's
possible).

There are a few ways to implement a parser, each with its own advantages
and disadvantages. The methods covered in this paper are:

@itemlist[
@item{A @tt{lex}/@tt{yacc} parser}
@item{A parser combinator}
]

@subsection{Formal languages}

@subsection{Lexing}

Lexing is a step that "tokenizes" the input source code. The tokenisation
process just splits the source up into a set of words, each with an
associated type (i.e. keyword, number, identifier, etc.). One of the
advantages of lexing is it removes non-semantic whitespace from the code,
so any spurious spaces, tabs or newlines are removed. This can simplify
the parser and make the logic in the later compiler passes simpler.

For instance, consider a string such as:

"The quick brown fox jumps over the lazy dog"

We could use spaces as a delimiter for words, and we could assign the
nouns that are animals with a special `ANIMAL` tag.

@code-examples[#:lang "at-exp racket" #:context #'here]|{
    (define string-to-lex "The quick brown fox jumps over the lazy dog")
    (define animals '("dog" "cat" "fox"))
    (define (animal? a)
     (ormap
      (lambda (q) (equal? q a))
            animals))
    (map (lambda (s)
          (match s
           ((? animal? s) `(ANIMAL ,s))
           ((? string? s) `(TOKEN ,s))))
     (string-split string-to-lex))
}|

This token list could then be passed on the parser to build the parse tree@note{
    A @bold{parse tree} is a tree constructed during the parsing phase. It's
    an early IR that is converted into a modified format later during semantic
    analysis.
}.

In Racket (and other languages that support pattern matching), lexing is
very easy to do without any library support, but since @tt{lex} (and tools
like it) are popular in other languages, it's worth looking at how to
write a lexer using @tt{lex}.

Typically, @tt{lex} uses regular expressions to match the incoming text to
make pattern matching simpler.

We'll now define a lexer for a subset of C's grammar which is implemented
using a Racket package called @tt{parser-tools}.

@code-examples[#:lang "at-exp racket" #:context #'here]|{
    (require br-parser-tools/lex
     (prefix-in : br-parser-tools/lex-sre))

    ; we'll call these value-tokens, since they reference a literal
    ; value in the source code
    (define-tokens value-tokens (INT DOUBLE CHAR STRING ID))

    ; these tokens don't need to hold a value since they're keywords, parts of
    ; syntax, or operators so the name defines what value they originally held
    (define-empty-tokens keyword-tokens
        (if else for * / + - < > = += -= return
         IF
         COMMA POUND SEMI LBRACE RBRACE LPAREN RPAREN LSQUARE RSQUARE))

    (define c-lexer
      (lexer
        ((eof) 'EOF)

        ; this rule allows us to skip tabs, spaces, and newlines
        ; by recursively calling c-lexer on the rest of the input
        ((:or #\tab #\space #\newline) (c-lexer input-port))

        ; lets us match keywords that we then convert into their symbolic
        ; name so they're recognized as tokens
        ((:or "else" "for" "*" "/" "+" "-" "<" ">" "=" "+=" "-=" "return")
         ; lexeme is an implicit variable that is created created
         ; for whatever the rule matched
         (string->symbol lexeme))

        ("{" 'LBRACE)
        ("}" 'RBRACE)
        ("[" 'LSQUARE)
        ("]" 'RSQUARE)
        ("(" 'LPAREN)
        (")" 'RPAREN)
        (";" 'SEMI)
        ("#" 'POUND)
        ("," 'COMMA)

        ; equivalent to the regexp [0-9]+
        ((:+ (:/ "0" "9"))
         (token-INT (string->number lexeme)))

        ; equivalent to the regexp [0-9]+\.[0-9]+
        ((:seq (:+ (:/ "0" "9")) #\. (:+ (:/ "0" "9")))
         (token-DOUBLE (string->number lexeme)))

        ((:seq (:+ (:/ "a" "z") (:or (:/ "a" "z") (:/ "0" "9"))))
         (token-ID lexeme))

        ((:seq "'" any-char "'") (token-CHAR (string-ref lexeme 1)))

        ; equivalent to the regexp ["][^"]*["]
        ((:seq #\" (:* (char-complement #\")) #\")
         (token-STRING
           (substring lexeme 1 (- (string-length lexeme) 2))))))

    (define test-input (open-input-string "int main {\nprintf(\"hello world!\n\");\nreturn 0;\n}"))

    ; now we'll lex the whole string
    (let lex-loop ()
        ; lex until we get EOF
        (let ((tok (c-lexer test-input)))
         (if (eq? tok 'EOF)
            '()
            (append (list tok) (lex-loop)))))

}|

Racket uses a macro to generate the lexer code at runtime, but
in older languages with the original @tt{lex} implementation
a developer would embed the lexer code in a specially annotated block in
a C program. Then, they could call @tt{lex} on the file and it would
generate a lexer in pure C code.

Packrat parsers don't utilize lexers (since they rely on matching strings
with the parser), nor do parser combinators, usually (though it is
possible). Nevertheless, @tt{lex} and @tt{yacc} are a common combo for
building high-quality, fast parsers.

Now that we've had a look at lexers, it's time to move onto the next step:
parsers.

While lexers and parsers certainly seem similar, the key difference between
the two is that parsers are more sophisticated than lexers.
Where a lexer simply matches and splits the input string, a parser builds a tree
with semantic structure. A lexer takes flat data as an input (a string) and returns
flat data (an array of tokens), whereas a parser takes an array of tokens
and constructs a parse tree.

@subsection{A @tt{yacc} parser}

@(lp-include "c-parser.scrbl")

@subsection{A parser combinator }

@(lp-include "c-parser-combinator.scrbl")

@section{Interpreters}

@subsection{Graph walking}

@subsection{Virtual Machine}

@subsubsection{Stack based}

@subsubsection{Register based}

@subsubsection{JIT compilation}

@section{Compilers}

@subsection{Optimization passes}

@subsection{Code generation}
