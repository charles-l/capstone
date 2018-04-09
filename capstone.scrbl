#lang scribble/book

@(require scribble-code-examples)
@(require scribble/lp-include)
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@(require "util.rkt")
@(require plot/pict)
@(require scribble-math)

@title{Programming Language Implementation}
@author{Charles Saternos}

@(table-of-contents)

@section{Introduction}

From the outside, programming languages seem like mystical entities that
somehow translate arcane instructions to tangible outputs. The inner
workings of these magical beasts are best left to those who have years of
research under their belt and advanced knowledge of computers.

While programming language implementations do draw on many areas of
computer science knowledge, simple programming language can easily be
developed by any hacker with moderate interest and some free time. In
fact, a simple meta-circular interpreter developed in Racket
(the language used for implementations in this book), could be written in
one sitting.

This book is meant to be an introduction to some of the concepts and
techniques used to build programming languages. We'll be looking at simple
implementations, with runnable example code throughout the book. Most code
examples can be run on their own, and the reader is encouraged to run
them, particularly when they are confused. Tinkering with example code is
one of the best ways to learn new programming techniques.

For larger code examples that require multiple chunks of code, the book is
written using literate programming. Literate programming provides
a textual version of the code, meant for reading, as well as the option to
stitch the code chunks together into a runnable source file. This allows
the reader to check out the exact code they read in the chapter, change
it, and run it for themselves.

@subsection{Book source}

The source for this whole book is available on GitHub
@url{https://github.com/charles-l/capstone}.
The reader is encouraged to download the source and run it for themself.

@subsection{Why bother with programming language implementation?}

There are many reasons for learning how programming languages work under
the hood, but some of the most useful are gaining a deeper understanding
of what programming languages do, how to implement your own domain
specific languages, and building your own general purpose programming
languages for fun.

Gaining a comprehensive knowledge of compilers and interpreters gives you
the ability to reason about error messages, and tune the performance of
programs written in your your favorite programming language. For instance,
knowledge of how the C compiler works, gives a programmer the ability to
determine the difference between compile time and link time errors. When
they get a linker error, they'll realize they forgot to include a library
or object file in their compilation command.

Performance tuning is also easier once a programmer knows how a programing
language is optimizing. For instance, in Lua, strings are interned
- they're inserted into a large hash-table, so string comparisons are
cheap. This means the programmer doesn't have to intern the strings in
a hash table by hand. Or in Python, knowing that the bytecode compiler
generates an opcode to create a dictionary directly for an inline
declaration (i.e. @tt{{}}), and the @tt{dict()} function does a function
call generates the opcode, meaning an inline @tt{{}} is faster than
calling @tt{dict()}.

@section{Background}

This chapter will introduce the basics of the Racket programming language,
and will discuss some theory that will be used later during
implementation.

@subsection{Racket}

@graphviz{
    digraph {
        "Lisp" -> "Scheme";
        "Scheme" -> "Racket";
        "Lisp" -> "Common Lisp";
        "Lisp" -> "Clojure";
    }
}

We'll use the Racket programming language for the implementations
throughout the book. Racket is a descendent of Lisp, the second high-level
programming language created (after Fortran). Despite its age, Lisp
dialects are still popular today.

Racket is designed to aid programming language developers in prototyping
and experimenting with new programming language paradigms. It is a test
bed, with many tools and libraries that make implementations simple and
concise. Instead of tracking memory usage in C, or writing dozens of
classes in a complicated hierarchy in Java, we will focus on developing
compilers and interpreters using domain-specific languages provided by
Racket.

Even without the libraries and extra resources, Racket is still an
excellent choice for developing programming languages. With built-in
functional pattern matching, idiomatic use of recursion, which is useful
for tree-navigation, and a simple, easy-to-implement kernel
(which makes building Lisp interpreters trivial), Racket is a great choice
for language development.

Racket is a simple language that you can learned quickly. You're
encouraged to enter the simple programs throughout this book to see the
results yourself. Fidget with them in a REPL
(the interactive prompt for Racket programs). Tweak
them and observe what changes in the output. You're encouraged to copy the
code throughout the book, but it's especially vital to understand this
chapter since concepts we introduce here are referenced throughout the book.

Lisp's@note{We'll talk generally about Lisp and Scheme since the features
are relevant to Racket} syntax is extremely minimal. Expressions are
written in prefix notation, with parentheses to denote function calls.

@code-examples[#:lang "racket" #:context #'here]|{
    (println "Hello world")
}|

Literal values (i.e. strings, numbers, or booleans) and variables are
written without parentheses.

@code-examples[#:lang "racket" #:context #'here]|{
"reeeee"
2

(define the-number-three 3)
the-number-three
}|

The number of parentheses is significant in Lisp, unlike other languages
where they're used for redefining order of operations. It's nonsense to
write extra parentheses in many contexts.

For instance, the variable we defined in the previous example in another
pair of parentheses will attempt to execute the bound value (@tt{3}) as
a function. This doesn't make sense, so it's important to match
parentheses carefully (a good editor is invaluable!).

@code-examples[#:lang "racket" #:context #'here]|{
(the-number-three)
}|

The DrRacet IDE that ships with Racket @cite{DrScheme} is a common choice
for newcomers to the language. DrRacket features a REPL
(read-eval-print-loop - a prompt for interactive program development),
a graphical debugger, and macro expander. It highlights and indents Racket
code properly, and generally makes life easier for users who aren't
familiar with Lisp syntax and notation.

Lisp doesn't (by default) have infix notation. Standard numeric operations
are written in prefix form. For example, 3 * (4 + 2) is written:

@code-examples[#:lang "racket" #:context #'here]|{
    (* 3 (+ 4 2))
}|

This format seems awkward at first, but has the advantage of simple syntax
that is straightforward to parse which makes user defined macros possible
(which we'll look at later).

Racket has standard data types (strings, numbers, and characters):

@code-examples[#:lang "racket" #:context #'here]|{
    "reeeee"
    42
    #\z
}|

And it has built-in data-types that are typically seen in Lisps. Namely
symbols, @tt{cons} cells, lists, and vectors.

Symbols are similar to strings. The only distinction is the symbol's value
is "interned" (i.e. inserted into an internal hash table), to make lookups
fast. This means symbols are cheap to compare, so they can be used in
contexts where enums would be used in other languages.

Symbols are created with the quote keyword:

@code-examples[#:lang "racket" #:context #'here]|{
(quote symbols-are-cooler-than-strings)
(println (quote just-a-symbol))
}|

Since it's awkward to keep write @tt{quote} every time we create a symbol,
we can use the shorthand @tt{'}.

@code-examples[#:lang "racket" #:context #'here]|{
'symbols-are-cooler-than-strings
(println 'just-a-symbol)
}|

Admittedly, the quote syntax is shocking when seen by programmers who use
languages that use single quotes to denote characters or strings. But
quote shorthand is terser than the @tt{quote} keyword, so we will use it.

Lisp's primary container structure is the cons cell. A cons cell is a pair
of two values. The first and second elements of the pair are accessed with
the @tt{car} and @tt{cdr} functions respectively.

@code-examples[#:lang "racket" #:context #'here]|{
    (cons 'a 'b)
    (cons 2 "cool")
    (cons #\m #\e)

    (car (cons 'a 'b))
    (cdr (cons 'my-other-car-is 'a-cdr))
}|

Cons cells can hold other cons cells in them, to become a linked list of
values.

@code-examples[#:lang "racket" #:context #'here]|{
    (cons 'a (cons 'little (cons 'dotted 'list)))
}|

Rather than storing a value in the last cons cell, it's standard to store
the null list, @tt{'()}, which helps simplify list navigation.

@code-examples[#:lang "racket" #:context #'here]|{
    (cons 'a (cons 'proper (cons 'list '())))
}|

Since @tt{cons}ing a bunch of @tt{cons} cells together is tedious, the
helper function, @tt{list} will create a linked-list of cons cells
(terminated by the empty list).

@code-examples[#:lang "racket" #:context #'here]|{
    (list 'a 'b 'c)
}|

Furthermore, quoting each individual element can become an annoyance, so
we can use the quote keyword to build a list of symbols.

@code-examples[#:lang "racket" #:context #'here]|{
    (quote (a b c))
}|

Or using the shorthand @tt{'}:

@code-examples[#:lang "racket" #:context #'here]|{
    '(a b c)
}|

The quote symbol can construct lists, sublists, and literal values.

@code-examples[#:lang "racket" #:context #'here]|{
    '((a b c) d e f 1 2 3 "strings too!")
    '(working with lists and symbols is quite nice)
}|

And if you want to quote a list, but insert an expression or bound value
(i.e. a variable) instead of a literal quote, you can use the quasiquote
and unquote syntax.

@code-examples[#:lang "racket" #:context #'here]|{
    `(1 plus 2 is ,(+ 1 2))
    `((+ 1 2) is ,(+ 1 2))
    (define the-sum (+ 1 2))
    `((+ 1 2) is ,the-sum)
}|

Variable assignment (called variable binding in functional languages) is
possible in Racket with two mechanisms. The first is using the @tt{define}
keyword (as shown before):

@code-examples[#:lang "racket" #:context #'here]|{
    (define my-awesome-list (list 'a 'b 'c))
    (car my-awesome-list)
    (cdr my-awesome-list)
}|

Racket's @tt{define} keyword operates like assignment in most
languages. The variable exists in the block or function it is defined in,
and is deleted once it goes out of scope.

The second way of defining variables is with a @tt{let}
binding@note{@tt{let} bindings are more idiomatic in functional languages,
but Racket code usually uses the @tt{define} keyword since it compiles to
a @tt{let} binding anyway.}. With a @tt{let} binding the scope of the
variable can be explicitly defined with a block.

@code-examples[#:lang "racket" #:context #'here]|{
    (let ((x 2) (y 3))
	 (+ x y))
}|

After the ending parenthesis for the @tt{let} block, the variable no
longer exists. Attempting to reference it results in an error:

@code-examples[#:lang "racket" #:context #'here]|{
    (let ((x 2) (y 3))
	 (+ x y))
    x
}|

Variations of @tt{let} allow the programmer to specify whether bindings
can reference each other (@tt{let*}), or reference themselves recursively
@tt{letrec} (a useful feature for constructing recursive inner functions).

@code-examples[#:lang "racket" #:context #'here]|{
    (let* ((x 2) (y (+ x 2)))
     (+ x y))

    (letrec ((y 2) (c (cons 'a y)))
     c)
}|

Racket encourages the use of unnamed functions@note{Called
@italic{anonymous functions}} with the @tt{lambda} keyword. A @tt{lambda}
requires arguments and a body. To call the newly constructed @tt{lambda},
wrap it in another set of parenthesis to apply it to the arguments you
pass.

@code-examples[#:lang "racket" #:context #'here]|{
    (lambda (x y)
      (+ x y))

    ((lambda (x y)
      (+ x y)) 2 4)
}|

Functions are first class values in Lisp, which means they can be passed
to another function like any primitive value or bound to a variable.

@code-examples[#:lang "racket" #:context #'here]|{
    (define f (lambda (a)
                (+ a 2)))
    (f 3)
	(letrec ((make-bacon
			  (lambda (n)
			   (cond
				((zero? n) '())
				(else
				 (cons 'bacon (make-bacon (- n 1))))))))
	 (make-bacon 5))
}|

In the previous example, you've seen the use of the @tt{cond} keyword,
which operates like an bunch of @tt{else if} statements. It starts with
the first condition @tt{(zero? n)}, checks if the condition is true, and
executes the associated expression if the condition passes (i.e. it
returns @tt{'()}). We can have more of these conditions, but in this code
we only ensure that @tt{n} isn't zero.

A larger @tt{cond} example with boolean operators:

@code-examples[#:lang "racket" #:context #'here]|{
	(define categorize-character
	 (lambda (has-staff has-beard has-hairy-feet)
	  (cond
	   ((and has-staff has-beard) 'wizard)
	   ((and has-hairy-feet has-beard) 'dwarf)
	   (has-hairy-feet 'hobbit)
	   (else 'unknown))))

	(categorize-character #t #t #f)
	(categorize-character #f #f #t)
}|

@tt{if} statements are in Racket, but require terse syntax. The first
expression in the if statement will run if the condition passes, otherwise
the second condition will execute.

@code-examples[#:lang "racket" #:context #'here]|{
	(if (> 8 (+ 2 1))
	  (println "8 is greater than (+ 2 1)")
	  (println "8 is less than (+ 2 1) which obviously isn't true"))
}|

Lisp heavily encourages recursion. Lisp can be defined without any looping
mechanisms built into the language since recursion can be used in place of it.

Cons cells are elegant when paired with recursion. Using the underlying
concept of pairs, its possible to recursively build and navigate complex
data structures like trees and lists with nothing but @tt{car}, @tt{car},
@tt{lambda}s and @tt{if} statements.

@code-examples[#:lang "racket" #:context #'here]|{
    (define append
     (lambda (a b)
        (if (null? a)
            b
            (cons (car a) (append (cdr a) b)))))

    (define flatten-list
     (lambda (tree)
      (if (null? tree)
       '()
       (if (list? (car tree))
        (append (flatten-list (car tree)) (flatten-list (cdr tree)))
        (cons (car tree) (flatten-list (cdr tree)))))))

    (flatten-list '((a b) (c) (e f)))
}|

We will not reimplement every utility function from scratch, but
remember these simple concepts since we will use cons-cells and lists
throughout this book. We will use libraries that come with Racket. The
documentation is readily available in the @tt{Help > Documentation} menu
in DrRacket.

@subsubsection{Macros}

Consider the following @italic{list}.

@code-examples[#:lang "racket" #:context #'here]|{
	'(+ 2 3)
}|

It's data. It doesn't actually calculate the value of 2 + 3, but it looks
eerily similar to the expression @tt{(+ 1 2)} which will run.

Using the built in @tt{eval} function, we can execute this data as code:

@code-examples[#:lang "racket" #:context #'here]|{
	(eval '(+ 2 3))
}|

Using an unquote, we can cheekily capture surrounding variables.

@code-examples[#:lang "racket" #:context #'here]|{
	(define x 34)
	(eval `(+ ,x 3))
}|

We can write code that generates code then evaluates it:

@code-examples[#:lang "racket" #:context #'here]|{
	(define (list-downfrom i)
	 (cond
	  ((zero? i) '(0))
	  (else (cons i (list-downfrom (- i 1))))))

	(cons '+ (list-downfrom 10))

	(eval (cons '+ (list-downfrom 10)))
}|

Using @tt{eval} for demonstration purposes is fine, but its not meant for
regular use in code since it's extremely unsafe. Besides security
vulnerabilities, @tt{eval}s can capture surrounding bindings implicitly
leading to subtle bugs that are difficult to understand.

Don't use @tt{eval}.

But surrendering the powerful feature of code that generates code and
evaluates it would be regrettable. Consider how much boilerplate we might
eliminate, or the many interesting domain specific languages we might
create.

A key feature of Lisp is that it is @italic{homoiconicity}. Its syntax
@italic{is} a built-in data structure that can be easily manipulated. This
is why the data @tt{'(+ 1 2)} looks like code that can be run.

Racket provides a @italic{hygienic macro system}@note{Some Lisp
implementations implement unhygienic macros that allow the expanded syntax
to capture a variable bound in the surrounding context, which is unsafe
but sometimes useful.} feature as a safe alternative to @tt{eval}. Macros
match user defined syntax and expand it into an new form specified by the
programmer before running the code (i.e. at "compile-time").

A contrived example might be @tt{inverse-if}, a feeble attempt at code obfuscation:

@code-examples[#:lang "racket" #:context #'here]|{
	(define-syntax (inverse-if stx)
	  (syntax-case stx ()
	    ((inverse-if condition else-expr then-expr)
		#'(if condition then-expr else-expr))))

	(inverse-if #f "IS THIS TRUE?" "IS THIS FALSE?")
}|


@subsubsection{Functional pattern @tt{match}ing}

A common feature in functional languages is functional pattern matching.
Since a significant portion of conditional code navigates and operates on
data structures, functional languages implement a matching system that
allows the programmer to bind values that are nested within a structure.

Consider an association list, a Lisp data structure that stores key-value
pairs in @tt{cons} cells and lists (similar to a hash table, albeit with poorer performance).
Association lists are slow to traverse if they're large, but are
performant enough when they don't store much data.

@(define ev (make-code-eval #:lang "racket"))
@code-examples[#:lang "racket" #:context #'here #:eval ev]|{
    (define tasty-food '((crunchy . peanut-butter)
                         (pulpy . orange-juice)
                         (whole-grain . bread)))
}|

If we wanted to write a search function to traverse the association list,
we could use @tt{car}s and @tt{cdr}s to access data in the list.

@code-examples[#:lang "racket" #:context #'here #:eval ev]|{
    (define (search term alist)
     (if (null? alist)
      #f
      (if (eq? (car (car alist)) term)
       (cdr (car alist))
       (search term (cdr alist)))))
    (search 'whole-grain tasty-food)
    (search 'pulpy tasty-food)
    (search 'chocolate tasty-food)
}|

But wouldn't it be nicer if the code looked like the data structure we
traverse?

@code-examples[#:lang "racket" #:context #'here #:eval ev]|{
    (define (matchy-search term alist)
        (match alist
            ('() #f)
            (`((,key . ,value) ,rest ...)
             (if (eq? key term)
               value
               (matchy-search term rest)))))
    (matchy-search 'whole-grain tasty-food)
    (matchy-search 'caramel tasty-food)
}|

Functional pattern matching makes code easier to read since it cleanly
breaks up the code into cases. Each case plainly details the
structure of the data it is able to process. Code written with functional
pattern matching is easier to read and write, since the programmer can
instantly determine which cases are handled. Without functional pattern
matching, the programmer must mentally interpret the code's execution to
discern which cases are handled.

@subsection{Language semantics}

Every programming language has useful features and odd quarks, just like
spoken languages. Historically, programming languages have varied wildly
in syntax. Newer languages tend toward C-style or ALGOL-style syntax, but
older languages experimented with strange and arcane syntax that goes
against modern sensibilities.

However, syntax a surface level detail. Well formatted or "pretty looking"
code isn't necessarily good code. While syntax may vary wildly between
languages, the @italic{semantics} (i.e. the "meaning" of the program) is
more importantly what differentiates programming languages.

Some important differences in semantics are discussed next.

@subsubsection{The paradigms: functional vs imperative vs object-oriented}

The major paradigms for programming languages are functional,
object-oriented (OO), and imperative. While other paradigms, such as
dataflow, logic, and concatenative exist, these smaller paradigms overlap
with others (for instance, a logic programming
library could be embedded in a functional language).

Imperative (or procedural) programming languages rely on mutation and
side-effects. Reassigning variables and destructively updating data structures
are examples of mutation, and are expected in imperative languages.
Imperative programming is popular for systems languages (e.g. C, C++,
Go and Rust), since it maps closely to how modern CPUs work. This makes
performance tuning more transparent. These types of languages tend to win
in programming languages benchmarks since they can easily be optimized for
real hardware.

The runtime speed does come at a cost. It takes more programmer time to
develop applications in imperative languages, and they tend to have more
bugs (since imperative code is harder to verify than functional). Recently,
imperative langauages have dropped in popularity since parallelizing
imperative programs can be difficult. As single-threaded performance for
CPUs plateaus (because of physical limitiations), parallelized
applications become more important.

Object-oriented programming utilizes imperative constructs inside objects.
Most object-oriented systems are graphs of objects that rely on mutation
and encapsulated state changes.

@figure[
    "Imperative code example"
    "Racket tends to discourage imperative programming, more often opting for the
    functional paradigm. That's why this code is a bit awkward"]{
    @code-examples[#:lang "at-exp racket" #:context #'here]|{
        (define x 0)

            (define (add-to-x! i)
             (set! x (+ x i)))

            (add-to-x! 2)

            (print x)
    }|
}@note{Racket encourages the convention of postfixing an exclamation mark onto
code that does mutation}

Object-oriented code focuses on keeping state data and functions that
change it together, in an attempt to mimic objects in the real world.
Perhaps we want to model a coffee machine with an object. Some relevant
state to track is whether it is turned on, how much water is in the water
chamber, or when the coffee was brewed. For functionality we'll add the
ability to turn on the coffee maker, and use it to brew a pot of
coffee.

@code-examples[#:lang "at-exp racket" #:context #'here]|{
    (define coffeemaker-class%
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

    (define my-coffeemaker (make-object coffeemaker-class%))

    (send my-coffeemaker make-coffee!)

    (send my-coffeemaker turn-on!)
    (send my-coffeemaker make-coffee!)
    (send my-coffeemaker make-coffee!)
    (send my-coffeemaker make-coffee!)
    (send my-coffeemaker turn-off!)

    (send my-coffeemaker get-cups-made)
}|

While the object-oriented model was popular in the 1990s, some of its
limitations have come to light recently. In particular, object-oriented
programming tends to lead to tightly-coupled components, that don't
encourage reuse. Object oriented code is also notoriously difficult to
parallelize since it's hard to reason about when variables will be
mutated.

Object-oriented programming might be useful sometimes, but it is a limited
model. It seems attractive at first but quickly becomes difficult to use.

The final paradigm we'll discuss is functional programming. It is the
paradigm that will be used most in this book. In functional programming,
functions are defined in the mathematical sense. They're @italic{pure},
which means they only have inputs and outputs without ever mutating
values.

For instance, a function that appends an element onto a list would be
written so it constructed an entirely @italic{new} list from scratch with
the new element appended onto the end. Purely functional languages force
immutability, meaning it's invalid to ever mutate a value. This tends to
result in code that is easy to reason about, since implicit state isn't
a factor to consider. Functional programming is superior to
object-oriented programming from a code reuse standpoint, since functional
composition is encouraged and tends to work better than complex class
hierarchies. It results in small, general functions that work well
together.

@code-examples[#:lang "at-exp racket" #:context #'here]|{
    (define l '(a b c d e f))
    (append l 'x)
    l

    (define j '(1 2 3 4 5 6))
    (map add1 j)
    j
}|

Functional programming does have limitations. For instance, functional
code that manages state can be confusing for a beginner since state
updates often simulated with recursion and monads. However, many
programs don't need implicit state, and can be rewritten as a series of
functions chained together in a pipeline. Pipelines are easy to
understand and reuse.

@subsubsection{When are types enforced? Static vs dynamic typing}

@figure[
"Code continuum"
"Continuum of dynamic/static, weak/strong type systems"
@(parameterize ([plot-width    400]
            [plot-height   230]
            [plot-font-face "Times New Roman"]
            [plot-x-label  "Weak ↔ Strong"]
            [plot-y-label  "Dynamic ↔ Static"])
            (define xs (build-list 20 (λ _ (random))))
            (define ys (build-list 20 (λ _ (random))))
            (plot (list
                   (point-label (vector -1 0.5) "C")
                   (point-label (vector -0.5 0.7) "C++")
                   (point-label (vector 0.8 -1) "Racket")
                   (point-label (vector -0.8 -1) "JavaScript")
                   (point-label (vector 1 1) "SML"))
                #:x-min -1.1
                #:x-max 1.3
                #:y-min -1.1
                #:y-max 1.1))
]


Choosing between static and dynamic typing is a decision between safety
and flexibility. With dynamic type systems, variables point to values that
track their own type information. This means variables can be
reassigned to different types, and functions aren't guaranteed to return
a specific type. This makes code easier to write quickly, but harder to
reason about (especially in a formal manner). Since it's difficult or
sometimes impossible to determine types statically (i.e. at compile time),
the programmer can't always guarantee that the program won't crash on
a type mismatch error at runtime.

In the following example, types are @italic{not} checked at compile time.
The predicates @tt{number?} and @tt{string?} operate on tagged runtime
values.

@code-examples[#:lang "racket" #:context #'here #:show-lang-line #t]|{
    (define (f x)
      (cond
        ((number? x)
         (add1 x))
        ((string? x)
         (string-append x " (≥∇≤)/"))
        (else x)))

    (f 2)
    (f "dynamic typing is freedom!")
    (f '(no one expects a LIST!))
}|

Given an arbitrary @tt{x} of an unknown type, we're unsure of the type of
the return value of @tt{f}. For quick prototyping and non-mission-critical
code, dynamic types are convenient and easy to work with. However, once
a system grows, the uncertainty from dynamic types makes debugging and
maintenance hard. There's no easy way to prove the system works without
running massive test suites (and still there are no guarantees).

In comparison, static typing @italic{requires} the programmer to notate type
information for functions and variables. Then, the type system can prove
correctness about types @italic{at compile time} and ensure that types are
handled consistently even for edge cases.

In this @tt{typed/racket} example, we force the type of @tt{x} to be
a real number. Attempting to pass a string instead, will result in
a compile-time error.

@code-examples[#:lang "typed/racket" #:context #'here #:show-lang-line #t]|{
    (: f (-> Real Real))
    (define (f x)
      (add1 x))

    (f 2)
    (f "static typing makes code safe!")
}|

Since the types are explicitly defined by the programmer, there's little
room for ambiguity. In cases where a programmer wants to define a function
that can operate on multiple data-types more generally, polymorphic types
can be used.

For instance, if we define an append function (named @tt{tappend} since
@tt{append} is already defined by Racket) to operate on a list, we
must define the type of the list.

@code-examples[#:lang "typed/racket" #:context #'here #:show-lang-line #t]|{
    (: tappend (-> (Listof Integer) Integer (Listof Integer)))
    (define (tappend l i)
      (cond
        ((null? l) (list i))
	(else (cons (car l) (tappend (cdr l) i)))))

    (tappend '(1 2 3) 4)
}|

This function only operates on lists of numbers, so we'd have to redefine
it for every possible datatype and duplicate the code. Instead of
specifying the exact type the list's contents, we can specify
a polymorphic function that accepts lists that contain any type.

@code-examples[#:lang "typed/racket" #:context #'here #:show-lang-line #t]|{
    (: tappend (All (A) (-> (Listof A) A (Listof A))))
    (define (tappend l i)
      (cond
        ((null? l) (list i))
	(else (cons (car l) (tappend (cdr l) i)))))

    (tappend '(1 2 3) 4)
    (tappend '(a b c) 'd)
    (tappend '("d" "o" "o") "d")
}|

Besides safety, static typing allows the compiler to perform more
optimizations since it can knows the types of the values and doesn't need
to check them at runtime. The resulting code can then has less overhead
due to type checks.

With Racket, we can have the best of both worlds, since code that is
written in @tt{racket/static} is compatible with regular Racket code. We
can quickly prototype, then tighten up our code with types later.

@subsubsection{Should the compiler typecast for you? Strong vs weak
typing}

The other axis that must be considered is strong vs weak types. Weak
typing is more "forgiving" in the sense that the language will
automatically cast values to prevent type errors. This may be considered
a convenience, but it can also lead to subtle bugs where unexpected type
coercion results in an incorrect type.

Most modern languages, particularly static languages, tend towards
a stronger type system. Arguably, the most popular language that is weakly
typed is C. It was originally designed for operating systems, which
require unsafe code. In C, any type can be cast to any other, and often
types are implicitly cast silently or with just a warning. There aren't
many rules for type casts in C. For instance, a floating point value could
be cast to a string pointer, then dereferenced. This series of
ridiculously unsafe operations may be correct in some context, but it is
difficult to prove the correctness.

Most scripting languages are more stringent about type coercion (e.g. they
won't allow addition to be performed on a number and a string). Some allow
implicit casting between integer and floating point numbers, but don't
take it farther than that. A notable deviation from this is
JavaScript. For instance, it will attempt to cast anything (even a function!)
to a string if a concatenation operation is attempted between a string and
any other value.

@subsubsection{What's the order of evaluation? Lazy vs strict evaluation}

@italic{Strict evaluation} evaluates all sub-expressions in an expression
before evaluating the top-level expression itself. For instance, with
strict evaluation the expression @tt{(a (b c) (d e))} would evaluate
@tt{(b c)} and @tt{(d e)}, then @tt{(a <the result of (b c)> <the result of (d e)>)}.
With lazy evaluation, the expressions @tt{(b c)} and @tt{(d e)} would only
be evaluated if @tt{a} referenced the value at some point.

Lazy evaluation waits until the last possible moment to execute
sub-expressions. If the value is never used, it is never evaluated. Most
languages have strict evaluation, but certain features, such as boolean
operators, are often lazily evaluated
(though usually its called short-circuiting in the context of boolean operators).

@code-examples[#:lang "racket" #:context #'here #:show-lang-line #t]|{
    (define (false-fun)
      (print "YOU'VE BEEN TERMINATED")
      (newline)
      #f)
    (define (true-fun)
      (print "continue...")
      (newline)
      #t)
    (and (false-fun) (true-fun) (true-fun))
    (and (true-fun) (false-fun) (true-fun))
    (and (true-fun) (true-fun) (false-fun))
}|

As soon as a condition fails, the @tt{and} stops evaluating, which is a lazy way
of checking results. In languages with lazy evaluation, @italic{every} function operates this
way (i.e. it doesn't compute the argument until it needs the value), which
allows for some interesting constructs that aren't possible with strict
evaluation.

For instance, with strict evaluation, the following recursive function
would loop forever since there's no base condition to terminate it.

@code-examples[#:lang "racket" #:context #'here #:show-lang-line #t]|{
    (define (dont-stop-believing)
      (cons 'hold-on-to-that-feeeeeeling (dont-stop-believing)))
}|

However, with Racket's @tt{lazy} language implementation (that does lazy
							       evaluation),
this code will operate as a lazy stream
(which doesn't compute the whole array, and only computes each value as it
       is needed). Therefore, we can represent infinite data structures.

@code-examples[#:lang "lazy" #:context #'here #:show-lang-line #t]|{
    (define (dont-stop-believing)
      (cons 'hold-on-to-that-feeeeeeling (dont-stop-believing)))

    (car (dont-stop-believing))

    (car (cdr (dont-stop-believing)))

    (cdr (dont-stop-believing))
}|

Looking at the last result, we can see it creates
a @italic{promise}, which delays the evaluation of the results until the
value is "forced" (i.e. evaluated) once its value is needed.

Lazy evaluation doesn't cooperate with side-effects, since an argument
might change part way through a functions evaluation. Lazy evaluation
isn't seen often besides an occasional purely functional language like
Haskell, or as an addon for languages that heavily encourage the
functional paradigm (like Scheme and OCaml).

@subsection{Lambda calculus}

When discussing programming language theory, its useful to define
a minimal model that can perform any computation. The first and most
common model used is the Turing machine, which is a useful model because
it maps closely to real hardware.

A Turing machine consists of a finite state machine@note{Which is a graph
of the control flow of the program, much like the IR for a compiler.}, and
a tape reader that can read, write and shift around an infinite tape. Real
CPUs execute lists of instructions with jumps (that a finite state machine
can model), and memory is similar to a tape that doesn't require shifting
to cells.

Using this minimal model for computability can lead to some interesting
designs. For instance, HTML5 and CSS3 (without JavaScript),
C++ templates (which were made Turing complete by accident), Magic: The
Gathering (the card game), Minecraft, Excel formulas, and a plethora of
other systems are accidentally Turing complete @note{That is to say, they
meet the requirements for a Turing machine}. This means that @italic{any}
computable problem
(i.e. any problem that can be solved with a real computer and a general
      purpose programming language), can be solved with any of these
Turing complete systems. You could calculate the Fibonacci sequence with
Magic the Gathering, or compute a frame of Doom with an Excel spreadsheet.
You could emulate an NES game with just C++ templates, and do matrix
multiplication with CSS3 rules.

Obviously, these examples are contrived, and it wouldn't make sense to use
these systems for a real computation. They're not meant for
general purpose computing so they're slow and difficult to use to solve
these problems. However, it is useful to know the minimum requirements to
make a system Turing complete, since you can develop a way of computing
a desired value from first principles.

The Turing machine model is useful for modeling simple, real-world
computers. However, it isn't good for representing functional programming
languages since they don't focus on memory allocation in a tape, and often
represent function as first-class values which is awkward to represent in
a Turing machine. Instead of Turing machines, we will be using Lambda (λ)
Calculus (a model invented by Alonzo Church in the 1930s), to reason about
computation@cite{Church, 1932}. Lisp was based on λ-calculus, so we will
use Lisp examples to demonstrate certain λ-calculus concepts.

In λ-calculus notation, function definitions are represented with the
λ symbol and function applications are written in prefix notation (like
Lisp). For example, the Lisp function:

@code-examples[#:lang "racket" #:context #'here]|{
(lambda (i)
  (+ 32 i))
}|

Would be notated in λ-calculus as,

@$${\lambda i \; . \; + \; 32 \; i}

Parentheses are included for to clearly delimit expressions.
In particular, they're useful for clearly notating applications@note{We
will use the term @italic{function application} and @italic{function call}
interchangeably}. For instance:

@$${(\lambda i \; . \; + \; 32 \; i) \; 2}

Which @italic{reduces} (or evaluates) to:

@$${(\lambda i \; . \; + \; 32 \; i) \; 2 \rightarrow \; (+ \; 32 \; 2) \rightarrow 34}

The @${\rightarrow} denotes a single-step evaluation

To make our notation easier to understand, we'll allow function
definitions through the @tt{=} symbol. It is possible to represent
recursion with just anonymous functions using the Y-combinator
(a great description of which can be found in @cite{The Little Schemer}),
but we'll allow function definitions to simplify our notation
(since it's clearer).

In our previous examples, we've used operators (like @${+}), and data
types like numbers in our functions, but λ-calculus doesn't necessarily
require numbers, standard mathematical operators, lists, or any kind of
data types besides functions. Functions can encode data with Church
Encoding @cite{Types and PL}. Once again, for pragmatic reasons, we will
extend λ-calculus to include common data types like numbers, strings, and
booleans. We will also assume common operations on these data types.

@subsubsection{Multiple arguments and currying}

We can represent multiple arguments in λ-calculus by returning a new
function for every argument. For instance, if we wanted to write
a function to add three arguments together, we might write:

@$${\lambda a \; . \; \lambda b \; . \; \lambda c \; . \; (+ \; a \; b \; c)}

And to evaluate it, we evaluate each intermediate function until we the
final third function that adds the arguments.

@$${((((\lambda a \; . \; \lambda b \; . \; \lambda c \; . \; (+ \; a \; b \; c)) 1) 2) 3) \rightarrow}
@$${((\lambda b \; . \; \lambda c \; . \; (+ \; 1 \; b \; c) 2) 3) \rightarrow}
@$${(\lambda c \; . \; (+ \; 1 \; 2 \; c) 3) \rightarrow}
@$${(+ \; 1 \; 2 \; 3) \rightarrow 6}

After each of the first three steps of evaluation, the function reduces to
a new function with the innermost reference to the variable bound to the
argument that was passed. We can take advantage of these "partial
applications" to generate functions that store part of a computation, but
still require more arguments to fully compute the final value.

For instance, consider a string concatenate function (called @tt{concat}).

@$${\texttt{concat} \; \texttt{"a"} \; \texttt{"b"} \rightarrow \texttt{"ab"}}

If we wanted to create a function called @tt{suffix}, that concatenates
a string (@tt{s}) onto the suffix (@tt{u}).

@$${\texttt{suffix} = \lambda u \; . \; \lambda s \; . \; \texttt{concat} \; s \; u}

We can partially apply this function to create an @tt{excited} function
that concatenates an exclamation point onto a string it is passed.

@$${\texttt{excited} = ((\texttt{suffix} \; \texttt{"!"}) \rightarrow (\lambda u \; . \; \lambda s \; . \;
\texttt{concat} \; s \; u) \; \texttt{"!"} \rightarrow \lambda s \; . \; \texttt{concat} \; s \; \texttt{"!"})}

We can apply @tt{excited} to any string and the function will append the
explanation point.

@$${\texttt{excited} \; \texttt{"currying is cool"} \rightarrow}
@$${(\lambda s \; . \; \texttt{concat} \; s \; \texttt{"!"}) \; \texttt{"currying is cool"} \rightarrow}
@$${\texttt{concat} \; \texttt{"currying is cool"} \; \texttt{"!"} \rightarrow \texttt{"currying is cool!"}}

Currying is possible in Racket using the @tt{curry} function.

@code-examples[#:lang "racket" #:eval ev2 #:context #'here]|{
	(define (suffix suf str)
	  (string-append str suf))

	(define excited (curry suffix "!"))
	(excited "currying is cool")
}|

@subsubsection{Function composition}

Nested function calls are prevalent in functional programming, which can
lead to busy-looking code.

@(define ev2 (make-code-eval #:lang "racket"))
@(ev2 '(define f identity))
@(ev2 '(define g identity))
@(ev2 '(define h identity))
@(ev2 '(define i identity))
@(ev2 '(define (j _) 'some-transformed-value))

@code-examples[#:lang "racket" #:eval ev2 #:context #'here]|{
    (f (g (h (i (j 'a-value)))))
}|

To simplify the notation for this type of nesting, we'll use the compose
operator to create a new function that applies each of the functions from
right to left (because of the order that results from nesting). So the
above code is equivalent to the following:

@code-examples[#:lang "racket" #:eval ev2 #:context #'here]|{
    ((compose f g h i j) 'a-value)
}|

The compose operator originates in math, and is notated like so:

@$${f \circ g \circ h \circ i \circ j}

Programming purely through function composition is called
@italic{point-free programming}, and has the advantage of fewer named
variables which can make code easier to read and understand. Naming
variables is a difficult task, and a poorly named variable can confuse
a reader, so removing intermediate variables eliminates the issue.

@section{Parsing}

Like spoken languages, programming languages have a grammar. However,
human languages aren't strict in their rules and are full of exceptions
and contextual cues. This is why natural language processing is a hard
problem, and why computers aren't programmed in English.

For instance, the following English sentence is ambigious:

"Time flies like an arrow; fruit flies like a banana."

Does the phrase "fruit flies" refer to a collection of insects? If so,
then we're saying that a specific family of insects enjoy a banana.
However, if "fruit flies like a banana" means pieces of fruit move through
the air similarly to a thrown banana, that's an entirely different
meaning.

The reason a programming language is "understood" and executed by the
mindless machine is because it is described in a (usually
context-free) formal grammar. The formal grammars that language designers
use to describe the syntax for a grammar can unambiguously be "parsed."

The first step taken by any interpretor or compiler is the parsing step.
Parsing converts the plain-text source code into IR (intermediate
representation) - a tree (referred to as the @italic{parse tree}). The
parse tree is passed on to later compilation passes for semantic analysis,
interpretation or code generation, and other transformations.

Of course the parser needs a grammatically valid source file, so in
translating the source to a parse tree, it checks the syntax of the input.
If the syntax is invalid, it can emit a syntax error and fail, or attempt
to continue parsing the rest of the file if it's possible.

There are many ways to implement a parser, but in this book we'll cover
two:

@itemlist[
@item{A @tt{lex}/@tt{yacc} parser}
@item{A parser combinator}
]

@subsection{Formal languages}

Formal languages are sets of strings that are made of words that are
generated by an alphabet @cite{Appel, 1998}. A language has an
associated grammar that defines the rules for constructing a valid (or
@italic{well-formed}) string in the language. Formal languages are
concerned purely with syntax (i.e. how sentences are constructed) not
semantics (i.e. what the sentences mean).

A grammar can be defined through a series of rules, which can then be
translated to a parser implementation. In the Chomsky hierarchy
@cite{Chomsky, 1956} the sophistication of the language and its
implementation increases with every level (@(figure-ref "ch")).

@figure[
"ch"
"The Chomsky hierarchy"
@graphviz{
digraph {
    subgraph cluster_c2 {label="Recursively-enumerable languages"
    subgraph cluster_c1 {label="Context sensitive language"
	subgraph cluster_c0 {label="Context free language"; "Regular Language" [shape = "record"];}
    }
    }
}
}
]

The simplest type of language, the regular language (also known as
a regular expression) can be defined with a restricted grammar that can be
modeled as a finite state machine. A finite state machine is simple to
implement which is appealing, but regular expressions cannot parse most
programming languages. They're usually used for simple pattern matching on
strings (which makes them useful for parts of a parser like the lexing stage).

The next level, a context-free language, has more complex grammar and can
be modeled with a finite-state machine and a stack. Programming language
grammars are defined to be context-free, so syntax can be more expressive
without requiring the more sophisticated parsing algorithms for
the languages higher in the Chomsky hierarchy.

Context-sensitive languages can be parsed with a restricted Turing machine
and a recursively-enumerable language can be parsed with a full Turing
machine. Recursively-enumerable grammars can model human language, since
they have no restrictions on how grammars are constructed.

A context free grammar can be defined using a series of rules called
productions. Each production @italic{derives} a @italic{terminal} or
a @italic{non-terminal}. Terminals are the leafs of the derived tree, and
cannot be derived any further, while non-terminals have an associated
production rule allows them to be further derived. When notating grammer,
terminals are lowercase, and non-terminals are capitalized.

The symbol ε is used to notate an empty word, and the vertical bar means
either the rule on the left of the bar or the rule on the right of the bar
can be applied. The derivation starts with the starting symbol @tt{S}.

Using these rules, we can create a grammar that generates palindromes
in the alphabet @tt{{a, b}}.

@ttt{
    S -> aSa | bSb | a | b | ε
}

Using this grammar, if we wanted to generate the string @tt{aba}, we would
derive it as,

@tt{S -> aSa -> aba}

To generate @tt{aabbaa},

@tt{S -> aSa -> aaSaa -> aabSbaa -> aabεbaa = aabbaa}

A more sophisticated example might be to define a language with an
alphabet @tt{{a, b, c}}. Each string our language must have an equal
number of @tt{a}s and @tt{b}s followed by any number of @tt{c}s.

@ttt{
    S -> AB
    A -> aAb | ε
    B -> Bc | ε
}

To generate @tt{aaabbbc} we could derive it as follows,

@tt{S -> AB -> aAbB -> aaAbbB -> aaaAbbbB -> aaaεbbbB = aaabbbB -> aaabbbBc -> aaabbbεc = aaabbbc}

@subsection{Lexing}

@(lp-include "c-lexer.scrbl")

@subsection{A @tt{yacc} parser}

@(lp-include "c-parser.scrbl")

@subsection{A parser combinator }

@(lp-include "c-parser-combinator.scrbl")

No book on language implementation is complete without a brief look into
parsing techniques. However, for the rest of the book, we will be using
s-expressions to represent our language since they're a convenient
format to use to represent ASTs (especially in Racket). Writing an
s-expression parser is trivial, but since Racket has a built in @tt{read}
function (which does all necessary parsing into an s-expression), we'll be
freed from having to write any parsing code for the rest of the
implemlentations.

@section{Interpreters}

We will begin with a brief look at interpreters. Simple interpreters are
easier to implement than simpler compilers, so they're often used for
prototyping language implementations.

We'll start with the simplest type of interpreter, a graph walking interpreter.

@(lp-include "mccarthy-lisp.scrbl")

@subsection{Graph walking}

Graph walking interpreters are straightforward to implement. A few minor
transformations are made on the IR, then the interpreter executes the
graph directly. Since the source closely represents the runtime structure
of the code, runtime errors are easier debug. Finally, metaprogramming and
reflection are simple since no extra metadata has to be stored to ensure
the runtime environment has access to the information it needs.

The downside of graph walking interpreters is their poor performance.
Traversing a graph generally entails following pointers from node to node,
which is far slower to execute on modern CPU's than sequences of (mostly)
contiguous instructions. Many interpreted programming language
implementations start out as graph walking interpreters, since they're
quick to implement. Eventually, most compile to a bytecode that is
interpreted on a virtual machine (VM) to improve performance.

Essentially, we've already implemented a graph walking interpreter with the
Lisp implementation on the last page. Lisp code is already a tree once
@tt{(read)} in, and we simply traverse the tree to execute our program.

@subsection{Virtual Machine}

Once a language has a graph interpreter, the language implementer will
want to improve its performance. The next step is to design a virtual
machine (VM) for the language that maps closely to real hardware, without
drifting far from the semantics of the interpreted language. The goal of
the bytecode is to be an intermediate target between the source and real
machine code so the from source to the VM's instructions (called bytecode)
is a simple and fast.

@graphviz{
    digraph G {
        rankdir = LR;
        node [shape = box];
        subgraph cluster_0 {
            label = "VM";
            color = black;
            bytecode [label = "Interpret bytecode"];
            bytecode -> bytecode;
        }
        "Parse source" -> "Compile to bytecode" -> bytecode;
    }
}

While this strategy requires initially processing to convert the
source to bytecode, it can improve interpretation speed tremendously. To
prevent slow startup times caused by bytecode compilation, some languages
require a separate compile step to generate a bytecode file. This bytecode
file is then directly interpreted by the virtual machine implementation.
One example of this is Java, which requires a compile step with @tt{javac}
(the Java compiler) to generate a @tt{.java} file which can then be
interpreted by the JVM implementation installed on the machine. Even
languages like Python will generate bytecode files (@tt{.pyc}) and cache
them for installed libraries to improve import speed.

Some languages like Ruby and Lua don't bother with caching the bytecode
and re-parse the file every time it is loaded. This decision is made
because performance isn't a focus (in Ruby's case), or because the
language is meant for embedded purposes with small source files (in Lua's
case).

It's important to realize that the compilation step happens. The term
"interpreter" can be misleading when most language "interpreters" are
actually bytecode interpreters that require a compilation step to convert
source into VM bytecode. While the bytecode interpreter is a proper
interpreter (usually), modern interpreters can be better described as
naive compilers that feed the result into a bytecode interpreter.

@subsubsection{Stack-based}

The simplest virtual machine model is a stack-based machine. This
design of virtual machine has no registers, instead relying on pushing and
popping from the runtime stack. All operations affect the top elements in
the stack, and new computations are pushed onto the stack when completed.

A simple stack-based bytecode might look like:

@ttt{
push 3
push 7
add
push 2
div
}

Which equates to the operations,

@graphviz{
    digraph G {
        nodesep=.05;
        rankdir=LR;
        node [shape=record,width=.4,height=.1];

        node00 [label = "",shape=none];
        node0 [label = "3 | |  |  |  |  |  | ",height=2.0];
        node1 [label = "7 |3 |  |  |  |  |  | ",height=2.0];
        node2 [label = "10 | |  |  |  |  |  | ",height=2.0];
        node3 [label = "2 |10 |  |  |  |  |  | ",height=2.0];
        node4 [label = "5 | |  |  |  |  |  | ",height=2.0];

        node00 -> node0 [label = "push 3"];
        node0 -> node1 [label = "push 7"];
        node1 -> node2 [label = "3 + 7"];
        node2 -> node3 [label = "push 2"];
        node3 -> node4 [label = "10 / 2"];
    }
}

After each result from an operation is computed, it placed on the top of
the stack, so the next operation can access it. Stack machines are popular
because the simple design is appealing. Register allocation is not an
issue, and it is easy to reason about stack operations. Stack-based VMs do
have a slight performance loss and require more instructions than
register-based VMs.

@subsubsection{Register-based}

Register-based VMs require fewer significantly fewer instruction for
programs, and are faster than stack-based machines @cite{Yunhe, 2005} (due
to the fact that a register-based VM reflects the architecture of a real
machine more than a stack-based VM does).

Register-based VMs are less common than stack-based VMs. The most notable
register-based VMs implementations are LuaJIT and the V8 JavaScript
implementation in Chrome.

@subsubsection{JIT compilation}

The disadvantage of a naive stack or register-based interpreter written
another language is that interpretation of virtual machine bytecode is
inherently slower than native code. Machine code written specifically for
the target CPU's architecture will run faster the machine code that is
interpreting some other bytecode. To mitigate the problem, the fastest
interpreters compile portions of the interpreted bytecode directly to
machine code at runtime.

This technique (known as just-in-time compilation), is what makes many modern
interpreters fast. LuaJit, the V8 JavaScript interpreter, the Java Virtual Machine,
and PyPy (a JITted Python implementation) are some examples of popular
JIT implementations.

Since the code is compiled during runtime, the interpreters are very
sophisticated and can be difficult to debug (since part of the interpreted
code is VM bytecode and part is be machine code). While simplicity is
lost, JITs can perform optimizations that are impossible for conventional
compilers.

Tracing JITs analyze the program as it is interpreted as bytecode. When it
finds a "hot spot" (i.e. a sequence of instructions that repeatedly
executed), it compiles the bytecode instructions to native instructions so
they execute faster.

If a module was dynamically loaded at runtime, and a function was
frequently called in the bytecode, the tracing JIT could
inline@note{Inlining is the process of copying the code inside a function
to where the function is called, so there's less overhead from a function
call and certain other optimizations are possible} the bytecode
instructions and compile the whole sequence to
machine code, which is something a conventional ahead-of-time (AOT)
compiler simply couldn't do. It is impossible for an AOT compiler to
determine which module will be dynamically loaded.

The dynamism of JITs is quite fascinating, and its interesting how the
interpreter can "learn" to make code faster over time. But JIT's are a
sophisticated and advanced subject, requiring a book of their own.

JITs attempt to take advantage of the best aspects of compliers (speed)
and interpreters (flexibility), with a tradeoff of complexity. With
a basic understand of interpretation, we will move onto the final portion
of this book which focuses on compilers.

@section{Compilers}

@(lp-include "compiler.scrbl")

@section{Conclusions}

In the last few chapters, we've learned to use Racket to build parsers
using traditional parser tools like @tt{lex} and @tt{yacc}. We've looked
into the more advanced approach of using parser combinators, and seen how
they compare to @tt{lex} and @tt{yacc}.

We've breifly looked at interpreter implementation, and how modern
"interpreters" use bytecode virtual machines to improve performance. Some
even perform more compilation steps in the bytecode interpreter to JIT
code for native speeds.

Finally, we looked at how compilers work, and how the passes for common
transformations could be implemented in a nanopass compiler. The full
source is available at
@url{https://github.com/charles-l/comp/tree/master/fir} for reader
experimentation.

@subsection{Where to go from here}

Given the constraints, this introduction only scratches the surface of
programming language implementations. Many resources exist for those who
are more interested in this area of computer science.

As mentioned many times throughout this book, Structure and Interpretation
of Computer programs @cite{SICP} is a great introduction to many
interpreter and compiler concepts, and is a good place to start before
delving into more advanced literature.

The Tiger Book @cite{Appel, 1998} is more advanced, and goes into more
detail about many of the details of a compiler. The @cite{Dragon Book} is
even more advanced, and is considered a classic in the compiler community.
It is primarily focused on imperative compilers, but most of the
information is useful to functional languages.

For jitted interpreters, there aren't really any books on the subject.
There are a smattering of blog posts and academic papers on the subject,
but no central source of knowledge. The best way to learn about JITs is to
study real implementations for LuaJIT or V8, and play with the source
code.

There is a good chance this book will continue to receive updates into the
future. If it does, the source code and book contents at
@url{https://github.com/charles-l/capstone} will be updated.

@section{References}

@(include-section "bib.scrbl")

