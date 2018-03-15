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

@section{Background}

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
language created (after Fortran). Despite its age, Lisp dialects are in
popular today (particularly in the programming language theory community).
Racket was designed to aid programming language developers in prototyping
and experimenting with new programming language paradigms. It is a test
bed, with many tools and libraries make implementations simple and
concise. Instead of tracking memory usage in C or writing dozens of
classes in a complicated hierarchy in Java, we will focus on developing
compilers and interpreters using domain-specific languages provided by
Racket.

Even without the libraries and extra resources, Racket is still an
excellent choice for developing programming languages. With built-in
functional pattern matching, idiomatic use of recursion, which is useful
for tree-navigation, and a simple, easy-to-implement kernel (which makes
building Lisp interpreters trivial).

Racket is a simple language that you can learned quickly. You're
encouraged to enter the simple programs throughout this book to see the
results yourself. Fidget with them in a REPL. Tweak them and observe what
changes in the output. You're encouraged to copy the code throughout the
book, but it's especially vital to understand this chapter since concepts
from it will be referenced throughout the book.

Lisp's@note{We'll be using Racket and Lisp interchangeably when talking
about basic concepts that apply to Lisp implementations} syntax is
extremely minimal. Expressions are written in prefix notation, with
parentheses to denote expressions.

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
write extra parentheses in many contexts. For instance, the variable we
defined in the previous example in another pair of parentheses will
attempt to execute the bound value (the number 3) as a function. This
doesn't make sense, so it's important to match parentheses carefully (a
good editor is invaluable!).

@code-examples[#:lang "racket" #:context #'here]|{
(the-number-three)
}|

The DrRacet IDE that ships with Racket @cite{DrS} is a common choice for
newcomers to the language. DrRacket features a REPL, a graphical debugger,
and macro expander. It highlights and indents Racket code properly, and
generally makes life easier for users who aren't familiar with Lisp syntax
and notation.

Lisp doesn't (by default) have infix notation. Standard numeric operations
are written in prefix form. For example, 3 * (4 + 2) is written:

@code-examples[#:lang "racket" #:context #'here]|{
    (* 3 (+ 4 2))
}|

This format seems awkward at first, but has the advantage of simple syntax
that is straightforward to parse and makes user defined macros possible
(which we'll look at later).

Racket has standard data types (strings, numbers, and characters):

@code-examples[#:lang "racket" #:context #'here]|{
    "reeeee"
    42
    #\z
}|

And it has built-in data-types that are typically seen in Lisps. Namely symbols,
cons cells, lists, and vectors.

Symbols are similar to strings. The only distinction is the symbol's value
is "interned" (i.e. inserted into an internal hash table), to make lookups
fast. This means symbols are cheap to compare in contexts where enums
would be used in other languages.

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
quotes are terse, so we will use them.

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

Cons cells can hold other cons cells in them, to a linked list of
values.

@code-examples[#:lang "racket" #:context #'here]|{
    (cons 'a (cons 'little (cons 'dotted 'list)))
}|

Rather than storing a value in the last cons cell, it's standard to store
the null list, @tt{'()}, which simplifies list navigation.

@code-examples[#:lang "racket" #:context #'here]|{
    (cons 'a (cons 'proper (cons 'list '())))
}|

Since consing a bunch of conses together is tedious, the helper function,
@tt{list} will create a linked-list of cons cells (terminated by the empty list).

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
@tt{letrec} (a useful construct for constructing recursive inner functions).

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

Cons cells are elegant. Using the underlying concept of pairs, its
possible to recursively build and navigate complex data structures like
trees and lists with nothing but @tt{car}, @tt{car}, @tt{lambda}s and
@tt{if} statements.

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

We're not about to reimplement every utility function from scratch since,
but remember these simple concepts since we will use cons-cells and lists
throughout this book. We will use libraries that come with Racket, but the
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

But surrendering the power of code that generates code and evaluates it
would be regrettable. Consider how much boilerplate we might eliminate, or
the many interesting domain specific languages we might create. A key
feature of Lisp is its @italic{homoiconicity}. It's syntax @italic{is}
a data structure it can manipulate. It would be wasteful to lose a feature
that powerful.

Racket provides a @italic{hygienic macro}@note{Some Lisp implementations
implement unhygienic macros that allow the expanded syntax to capture
a variable bound in the surrounding context, which is unsafe but sometimes
useful.} feature as a safe alternative to @tt{eval}. Macros match user
defined syntax and expand it into an new form specified by the programmer
at compile-time.

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
pairs in cons cells and lists when the speed of a hash table isn't
necessary. Association lists are slow to traverse if they're large, but
are performant enough when they don't store much data.

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
matching, the programmer must mentally interpret the codes execution to
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
what truely differentiates programming languages.

Semantics are a broad topic, but some important categories are discussed next.

@subsubsection{The paradigms: functional vs imperative vs object-oriented}

The major paradigms for programming languages are functional,
object-oriented (OO), and imperative. While other paradigms, such as
dataflow, logic, and concatenative exist, these smaller paradigms overlap
with each other and the major paradigms (for instance, a logic programming
library could be embedded in a functional language).

Imperative (or procedural) programming languages rely on mutation and
side-effects. Reassigning variables and destructively updating data structures
are examples of mutation, and are expected in imperative languages.
Imperative programming is popular for systems languages (e.g. C, C++,
Goand Rust), since it maps closely to how modern CPU's works which makes
performance tuning more transparent. These types of languages tend to win
in programming languages benchmarks since they can easily be optimized for
real hardware.

The runtime speed does come at a cost. It takes more programmer time to
develop applications in imperative languages, and they tend to have more
bugs (since its harder to verify that code works as expected). Recently,
imperative langauages have dropped in popularity since parallelizing
imperative programs can be difficult. As single-threaded performance for
CPUs plateaus (because of physical limitiations), parallelized
applications become more important.

Object-oriented programming utilizes imperative constructs inside objects,
since object-oriented systems are usually mutating graphs of objects.

@figure[
    "Imperative code example"
    "Racket tends to discourage this type of imperative programming, more often opting for the
    functional paradigm. That's why this code is a bit awkward"]{
    @code-examples[#:lang "at-exp racket" #:context #'here]|{
        (define x 0)

            (define (add-to-x! i)
             (set! x (+ x i)))

            (add-to-x! 2)

            (print x)
    }|
}

Object-oriented code focuses on storing functionality and state data
together, in an attempt to mimic objects in the real world. Perhaps I want
to model my coffee machine with objects. Some relevant state I might model
is whether it is turned on, how much water is in the water chamber, or
when the coffee was brewed. To model functionality, I'll add the ability
to turn on the coffee maker or use it to brew a pot of coffee.

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
encourage reuse and it is difficult to parallelize a programs that are
object-oriented but don't use the actor model.

Object-oriented programming might be useful sometimes, but it is a limited
model. It seems attractive at first but quickly becomes difficult to use.

The final paradigm functional programming, and is the paradigm that will
be used most in this book. In functional programming, functions are
defined in the mathematical sense. They're @italic{pure} which means
they only have inputs and outputs without mutation.

For instance, a function to append to a list would be written so it
constructed an entirely @italic{new} list from scratch with the new
element appended onto the end. Purely functional languages force
immutability, meaning it's invalid to ever reassign a value. This tends to
result in that is easy to reason about, since implicit state isn't
a factor to consider. Functional programming is superior to
object-oriented programming from a code reuse standpoint, since functional
composition is encouraged. It results in small, general functions that
work well together.

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
updates are done with recursion and monads. However, many programs don't
need implicit state, and can be rewritten as a series of functions chained
together in a pipeline that is easy to understand and reuse.

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
                   (point-label (vector 0.7 -1) "Python")
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
impossible to check types at compile time (i.e. statically), the
programmer can't always guarantee that the program won't crash at runtime.

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
running the whole system.

In comparison, static typing @italic{requires} the programmer to notate type
information for functions and variables. Then, the type system can prove
correctness about types @italic{at compile time} and ensure that every
edge case is accounted for.

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
that can operate on multiple data-types generally, polymorphic types can
be used.

For instance, if we define an append function (named @tt{tappend} since
@tt{append} is already defined by Racket) to operate on a list, we have
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
specifying the exact type of what the list contains, we can specify
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
optimizations since it can make assumptions about types. The resulting
code can execute faster.

With Racket, we can have the best of both worlds, since code that is
written in @tt{racket/static} is compatible with regular Racket code. We
can quickly prototype, then add types later.

@subsubsection{Should the compiler typecast for you? Strong vs weak
typing}

The other axis that must be considered is strong vs weak types. Weak typing is
more "forgiving" in the sense that the language will attempt to cast the
value to what it thinks the programmer meant. This may be considered
a convenience, but it can also lead to subtle bugs where unexpected type
coercion results in an incorrect type.

Most modern languages, particularly static languages, tend towards
a stronger type system. Arguably, the most popular language that is weakly
typed is C. It was originally designed for operating systems, which
require unsafe code. In C, any type can be cast to any other. An integer
can be converted to a string pointer, then dereferenced. This series of
ridiculously unsafe operations may be correct in some context, but it is
difficult to prove correctness.

Most scripting languages are more stringent about type coercion (e.g. they
won't let you add a number and a string). Some allow implicit casting
between integer and floating point numbers, don't go much farther than
that. A notable deviation from this is JavaScript. For instance, it
will attempt to cast anything (even a function!) to a string if
a concatenation operation is attempted between two values of different
types.

@subsubsection{What's the order of evaluation? Lazy vs strict evaluation}

Strict evaluation evaluates all sub-expressions in an expression before
calling the main expression itself. For instance, with strict evaluation
the expression @tt{(a (b c) (d e))} would evaluate @tt{(b c)} and @tt{(d
e)}, then @tt{(a <the result of (b c)> <the result of (d e)>)}. With lazy
evaluation, the expressions @tt{(b c)} and @tt{(d e)} would only be
evaluated if @tt{a} required the value at some point.

Lazy evaluation waits until the last possible moment to execute the
expressions for its arguments. If the value is never used, it is never
evaluated. Most languages have strict evaluation, but certain keywords,
such as boolean operators, are often lazily evaluated (though usually its
called short-circuiting in the context of boolean operators).

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

For instance, in strict evaluation, the following recursive function would loop forever
since there's no base condition.

@code-examples[#:lang "racket" #:context #'here #:show-lang-line #t]|{
    (define (dont-stop-believing)
      (cons 'hold-on-to-that-feeeeeeling (dont-stop-believing)))
}|

However, with Racket's @tt{lazy} language implementation, this code will
operate as a lazy stream (which doesn't compute the next value in the
stream until it is needed). Therefore, we can have an infinite data
structure.

@code-examples[#:lang "lazy" #:context #'here #:show-lang-line #t]|{
    (define (dont-stop-believing)
      (cons 'hold-on-to-that-feeeeeeling (dont-stop-believing)))

    (car (dont-stop-believing))

    (car (cdr (dont-stop-believing)))

    (cdr (dont-stop-believing))
}|

Looking at the last result, we can see it creates
a @italic{promise}@note{A @bold{promise} is simple way of delaying an
evaluation (by wrapping it in an thunk (an anonymous function)), and
@bold{forcing} the value to be computed when it is needed.}, which delays
the evaluation of the results until the value is "forced."

Lazy evaluation doesn't cooperate with side-effects, since an argument
might change part way through a functions evaluation. Lazy-evaluation
isn't found in most programming languages besides an occasional purely
functional language like Haskell, and as an addon for languages that
heavily encourage the functional paradigm (like Scheme and OCaml).

@subsection{Lambda calculus}

When discussing programming language theory, its useful to have a minimal
model that can theoretically do any computation. The original and most
common model presented is the Turing machine, which is useful model
because it maps closely to real hardware.

A Turing machine consists of a finite state machine@note{Which is a graph
of the control flow of the program, much like the IR for a compiler.}, and
a tape reader that can read, write and shift around an infinite tape. Real
CPUs execute lists of instructions with jumps (that a finite state machine
can model), and memory is similar to a tape that doesn't require shifting
to cells.

Understanding the minimal requirements for computability can lead to some
interesting, but accidental results. For instance, HTML5 and CSS3 (without
JavaScript), C++ templates (which were made Turing complete by accident),
Magic: The Gathering (the card game), Minecraft, Excel formulas, and
a plethora of other systems are accidentally Turing complete @note{That is
to say, they meet the requirements for a Turing machine}. This means that
@italic{any} computable problem (i.e. any problem that can be solved with
a real computer and a general purpose programming language), can be solved
with any of these Turing complete systems. You could calculate the
Fibonacci sequence with Magic the Gathering, or compute a frame of Doom
with an Excel spreadsheet. You could emulate an NES game with just C++
templates, and do matrix multiplication with CSS3 rules.

Obviously, these examples are contrived, and it wouldn't make sense to use
them for a real computation. These systems are not meant for general
purpose computing so they're slow and difficult to use to solve
these problems. However, it is useful to know the minimum requirements to
make a system Turing complete, since you can develop a realistic solution
from first principles.

The Turing machine model is essential for building simple, real-world
computers. However, it isn't good for representing functional programming
languages since they don't focus on memory allocation in a tape, and often
represent function as first-class values which is awkward to represent in
a Turing machine. Instead of Turing machines, we will be using Lambda
(λ) Calculus (a model invented by Alonzo Church in the 1930s), to reason
about computation@cite{Church, 1932}. Lisp was based on λ-calculus, so we
will use Lisp examples to demonstrate some λ-calculus concepts.

In λ-calculus notation, function definitions are represented with the
λ symbol and function applications are written in prefix notation (like
Lisp). For example, the Lisp function:

@code-examples[#:lang "racket" #:context #'here]|{
(lambda (i)
  (+ 32 i))
}|

Would be notated in λ-calculus as,

@$${\lambda i \; . \; + \; 32 \; i}

Parentheses are included for clarity when multiple expressions are
written. In particular, they're useful for clearly notating
applications@note{We will use the term @italic{function application} and
@italic{function call} interchangeably}. For instance:

@$${(\lambda i \; . \; + \; 32 \; i) \; 2}

Which @italic{reduces} (or evaluates) to:

@$${(\lambda i \; . \; + \; 32 \; i) \; 2 \rightarrow \; (+ \; 32 \; 2) \rightarrow 34}

The @${\rightarrow} denotes a single-step evaluation

To make our notation easier to understand, we'll allow function
definitions through the @tt{=} symbol. It is possible to represent
recursion with just anonymous functions using the Y-combinator, but we'll
allow definitions to simplify our notation (since it's clearer).

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
still require more to fully compute the final value.

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
result in many nested function calls.

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
a reader, so removing named variables eliminates the issue.

@section{Parsing and Semantic Analysis}

Like spoken languages, programming languages have a grammar. However,
human languages aren't strict in their rules and are full of exceptions
and contextual cues. This is why natural language processing is a hard
problem, and why computers aren't programmed in English.

The reason a programming language is "understood" and executed by the
mindless machine is because it is described in a (usually
context-free) formal grammar. The formal grammars that language designers
use to describe the syntax for a grammar can unambiguously be "parsed."

The first step taken by any interpretor or compiler is the parsing step.
Parsing converts the plain-text source code into IR (intermediate
representation) - a tree of some sort (referred to as the @italic{parse
tree}). The tree is passed onto later compilation passes for semantic
analysis, interpretation, code generation and other transformations.

Of course the parser needs a grammatically valid source file, so in
translating the source to a parse tree, it checks the syntax of the input.
If the syntax is invalid, it can emit a syntax error and fail, or attempt
to continue parsing the rest of the file if it's possible.

There are many ways to implement a parser, each with its own advantage and
disadvantage. The methods covered in this book are:

@itemlist[
@item{A @tt{lex}/@tt{yacc} parser}
@item{A parser combinator}
]

@subsection{Formal languages}

TODO: write, use dragon book as reference

@graphviz{
digraph {
    subgraph cluster_c2 {label="Recursively-enumerable languages"
    subgraph cluster_c1 {label="Context sensitive language"
	subgraph cluster_c0 {label="Context free language"; "Regular Language" [shape = "record"];}
    }
    }
}
}

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
function (which parses our syntax for us), we'll be using it in
conjunction with simple pattern matching to parse our languages.

@section{Interpreters}

We will begin with a brief look at interpreters. Simple interpreters are easier
to implement than simpler compilers, so they're generally a first step for
programming language implementations.

We'll start with the simplest type of interpreter, a graph walking interpreter.

@(lp-include "mccarthy-lisp.scrbl")

@subsection{Graph walking}

Graph walking interpreters are straightforward to implement. A few minor
transformations are made on the IR, then the interpreter executes the
graph directly. Since the source closely represents the runtime structure
of the code, runtime errors are easier debug. Finally, metaprogramming and
reflection are simpler since no extra metadata has to be stored to ensure
the runtime environment has access to the information it needs.

The downside of graph walking interpreters is their poor performance.
Traversing a graph generally entails following pointers from node to node,
which is far slower to execute on modern CPU's than sequences of (mostly)
contiguous instructions. Many interpreted programming language
implementations languages start out as graph walking interpreters, since
they're quick to implement. Eventually, most implement a virtual machine
(VM) to improve performance.

Essentially, we've already implemented a graph walking interpreter with the
Lisp implementation on the last page. Lisp code is already a tree once
@tt{(read)} in, and we simply traverse the tree to execute our program.

@subsection{Virtual Machine}

Once a language has a graph interpreter, the language implementer will
want to improve its performance. The next step is to design a virtual
machine (VM) for the language that maps closely to real hardware, without
drifting far from the semantics of the interpreted language. A close
mapping ensure the conversion from source to the VM's instructions (called
bytecode) is a simple task.

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

While this strategy requires more processing to initially convert the
source to bytecode, it can improve interpretation speed tremendously. To
prevent slow startup times caused by bytecode compilation, some languages
require a separate compile step to generate a bytecode file. This bytecode
file is then directly interpreted by the virtual machine implementation.
One example of this is Java, which requires a compile step with @tt{javac}
(the Java compiler) to generate a @tt{.java} file which can then be
interpreted by the JVM implementation installed on the machine. Even
languages like Python will implicitely generate cached bytecode files
(@tt{.pyc}) for installed libraries to make imports faster.

Some languages like Ruby and Lua don't bother with caching the bytecode
and re-parse the file every time it is loaded. This may be because
performance isn't a focus (in Ruby's case), or because the language is
meant for embedded purposes with small source files (in Lua's case).

It's important to realize that the compilation step happens. The term
"interpreter" can be misleading when most language "interpreters" are
actually bytecode interpreters that require a compilation step to convert
source into VM bytecode. While the bytecode interpreter is a proper
interpreter (usually), modern interpreters can be better described as
naive compilers that feed the result into a bytecode interpreter.

@subsubsection{Stack based}

The simplest virtual machine model is a stack-based machine. This
design of virtual machine has no registers, instead relying on pushing and
popping from the stack. All operations affect the top elements in the
stack, and new computations are pushed onto the stack when completed.

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

Each after each operation or function "returns", it places the result on the top
of the stack, so the next operation can access it. Stack machines are
popular because the simple design is appealing. Register allocation is not
an issue, and it is simple to reason about stack operations. Stack-based
VMs do have a slight performance loss and require more instructions than
register-based VMs.

@subsubsection{Register based}

Register based machines require fewer significantly fewer instruction for programs,
and are faster than stack-based machines @cite{Yunhe} (due to the fact
that a register-based vm reflects the architecture of a real machine more
than a stack-based vm does).

@subsubsection{JIT compilation}

The disadvantage of a naive stack or register-based interpreter written
another language is that interpretation of virtual machine bytecode is
inherently slower than native code. Machine code written specifically for
the target CPU's architecture will run faster the machine code that is
interpreting some other bytecode. To mitigate the problem, fast
interpreters will compile portions of the interpreted bytecode directly to
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
instructions and compile the whole sequence of instructions to
machine code, which is something a conventional ahead-of-time (AOT)
compiler simply couldn't do since it is impossible for it to determine
which module will be dynamically loaded.

The dynamism of JITs is quite fascinating, and its interesting how the
interpreter can "learn" to make code faster over time. But JIT's are a
sophisticated and advanced subject, requiring a book of their own, so this
section will just be concluded with a brief JIT example.

TODO: example

@section{Compilers}

This final portion of the book will focus on compilers. Compilers convert the
source language to a target language (generally assembly or VM bytecode). The
process happens in a series of passes, starting with parsing, semantic
analysis, and a series of optimization passes.

The later passes in a compiler (called the backend) are where a compiler really
diverges from an interpreter. Both an interpreter and a compiler may share a
front-end (i.e. the parsing/semantic analysis passes), but after that point they
diverge in what they do.

Since compilers read and compile code once, they can do sophisticated and
computationally expensive optimization passes that aren't usually available to
interpreters for performance reasons. Fast compilers are important, but slower
compilers are acceptable when the final binary has a faster execution time.

An overview of the compiler pipeline we for our compiler implementation is as
follow:

@graphviz{
    digraph {
        node [shape = box];
        "Source" -> "Parse tree" [label="Parse"];
        "Parse tree" -> "Annotated parse tree" [label="Semantic analysis"];
        "Annotated parse tree" -> "IR";
        "IR" -> "Optimized IR" [label="Optimization passes"]
        "Optimized IR" -> "A-normal form";
        "A-normal form" -> "Target source" [label="Code generation"]
    }
}


@subsection{The IR}

After semantically checking the parse tree, the compiler converts the
parse tree into an intermediate representation (IR). The IR is designed to
make optimization and analysis easier. After the optimization and analysis
passes, the compiler generates machine code from the IR. Depending on the
complexity of the IR, there may be passes to convert the IR into
a lower-level IR that maps more closely to machine code.

Functional compilers regularly use continuation-passing style (CPS) as an
intermediate representation.

@subsubsection{Continuation passing style}
@subsubsection{A-normal form}

@subsection{Optimization passes}

Most of the passes in a real-world compiler's backend are dedicated to code
optimization. The term code optimization is a bit misleading, since it technically
doesn't result in optimal code, just code that may be more performant because of
transformations based on heuristics. However, these heuristics can be valuable for
improving the runtime performance of the binaries produced by the compiler.

@subsubsection{Inlining}

The most fundamental optimization is inlining, which is the process of
copying the code for a function directly to the call site. This lowers the overhead
for a function call, and makes other optimizations possible since the function
is no longer a black box.

@graphviz{
    digraph {
        subgraph cluster_0 {
            label = "before";
            node [shape = box];
            g [label = "(let ((a 2) (b 3))\l (my-super-add a b))"];
            h [label = "(define (my-super-add m n)\l (+ m n))"];
            g -> h;
        }
        subgraph cluster_1 {
            label = "after";
            node [shape = box];
            i [label = "(let ((a 2) (b 3))\l (+ a b))"];
        }
    }
}

@subsubsection{Partial evaluation}

If we look at the following code, we can hand optimize a few things.

@code-examples[#:lang "racket" #:context #'here]|{
    (define +widget-price+ 3)
    (define (widget-price-with-tax)
      (+ (* +widget-price+ 0.06) +widget-price+))

    (widget-price-with-tax)
}|

Given @tt{+widget-price+} stays constant throughout the program, calculating the
@tt{(widget-price-with-tax)} will be the same number in every case. Rather than recalculating
it ever time we call the function, we can determine statically (i.e. at compile time) that
the value of the widget price is 3.18.

@code-examples[#:lang "racket" #:context #'here]|{
    (define (widget-price-with-tax)
      3.18)

    (widget-price-with-tax)
}|

In this case, we were just dealing with constants, but partial evaluation is even more
powerful than that. For instance, an expression comprised of pure function could be
evaluated at runtime so as little code is run at runtime as possible.

@code-examples[#:lang "racket" #:context #'here]|{
    (define +widget-price+ 3)
    (define (widget-price-with-tax)
      (+ (* +widget-price+ 0.06) +widget-price+))

    (cons 'my-new-widget (cons 'price (cons (widget-price-with-tax) '())))
}|

Since all the necessary information for this program is available to the compiler at runtime
it could partially evaluate this example to:

@code-examples[#:lang "racket" #:context #'here]|{
    '(my-new-widget price 3.18)
}|

@subsubsection{Deforestation}

In functional programming, loops are generally discouraged since they're hard to write and
difficult to reason about. Instead, functional languages encourage the use of @tt{map},
@tt{filter}, and @tt{reduce} (or @tt{foldl}).

For instance, to list all the hobbits with names that end with 'r':

@(define defor-eval (make-code-eval #:lang "racket"))
@code-examples[#:lang "racket" #:context #'here #:eval defor-eval]|{
    (require srfi/13)
    (define *names* '("Thorin" "Fili" "Kili" "Balin" "Dwalin"
                      "Oin" "Gloin" "Dori" "Nori" "Ori"
                      "Bifur" "Bofur" "Bombur"))
    (map string-reverse
     (filter (λ (s) (eq? (string-ref s 0) #\r))
      (map string-reverse
       (map string-downcase *names*))))
}|

Besides the fact that using a @tt{string-reverse} is an extremely wasteful way to
check the first letter of a string, we're also constructing many intermediate lists that
we never even use. Every @tt{map} call generates a brand new list, and throws away the
previous. We can optimize these functions with deforestation.

@code-examples[#:lang "racket" #:context #'here #:eval defor-eval]|{
    (filter-map
     (λ (s)
      (and (eq? (string-ref ((compose string-reverse string-downcase) s) 0) #\r)
       (string-downcase s)))
     *names*)
}|

While it is difficult to understand this code, it only loops through one list, and only
constructs one new list. In the previous example, it looped through the same list 4 times,
and constructed 4 intermediate lists that it threw away. This is a hugely valuable
optimization for functional languages since it has the potential to massively cut down on
computation time for processing large lists.

@subsubsection{Various other optimization techniques}

There are a plethora of other optimization techniques utilized
by standard imperative compilers. Since they tend to be smaller, and less
specific to functional languages, we'll only briefly touch on them.

@subsubsection{Dead code elimination}

TODO define flow analysis

After doing flow analysis, we can determine the control flow for the whole program,
allowing us to see which expression are executed. If we mark every line of code that
has the potential to be executed, we're left with all the "dead code" (i.e. code that
cannot ever be executed in our program).

Dead code elimination cuts down wasted resource usage that is the result of lazy
programming. It can even be exposed to the programmer as a linting or code quality
metric so they can determine how much of the project is wasted.

Consider the following code:

@(define dc (make-code-eval #:lang "racket"))
@(dc '(define (a-pure-function f) f))
@code-examples[#:lang "racket" #:context #'here #:eval dc]|{
    (define (f x)
      (let ((g (a-pure-function x)))
        (+ 2 x)))
}|

Assuming @tt{(a-pure-function)} doesn't have any side-effects, the calculation it
does is wasted, since @tt{g} is never used. Therefore, the code will still be correct
if we remove the binding.

@code-examples[#:lang "racket" #:context #'here #:eval dc]|{
    (define (f x)
      (let ()
        (+ 2 x)))
}|

This reduces the size of our code and makes further optimizations simpler and faster since
we don't waste time reasoning about code that is never run.

@subsubsection{Redundant assignment removal}

A similar pass related to dead-code elimination is redundant assignment elimination.
If we reassign to a value twice, before reassigning it (again assuming no side effects),
we can delete the first assignment.

We could then transform this code:

@code-examples[#:lang "racket" #:context #'here]|{
    (define x (* 30 20))
    (set! x 30)
    (println x)
}|

into this code:

@code-examples[#:lang "racket" #:context #'here]|{
    (define x 30)
    (println x)
}|

@subsubsection{Loop invariant detection}

If code that is executed repeatedly is tuned for performance, the overall speed of a program can
improve drastically. When possible, optimizing compilers will often move code from inside
a loop to the scope above, if the value isn't determined by the loop. For instance,

@code-examples[#:lang "racket" #:context #'here]|{
    (require math/number-theory)
    (for ((i (in-range 0 200)))
     (let ((j (* i 2)) (k (nth-prime 20)))
      (+ j k)))
}|

Since @tt{j} relies on @tt{i}, a variable computed in the loop, we must leave it where it is.
However, @tt{k} recalculates the 20th prime in every iteration, despite the fact that the 20th prime
never changes. Therefore, we can "hoist" it out of the loop, and only calculate its value once.

@code-examples[#:lang "racket" #:context #'here]|{
    (require math/number-theory)
    (let ((k (nth-prime 20)))
     (for ((i (in-range 0 200)))
      (let ((j (* i 2)))
       (+ j k))))
}|

Loops constructs permeate imperative code, but are not as popular in functional languages,
so this type of optimization is less useful to us. However, it could still be used for hoisting
variables outside of recursive named-@tt{let}s or recursive inner functions.

@subsubsection{Loop unrolling}

Another optimization for loops is loop unrolling which lowers the cost of short loop
expressions. Since the loop expressions will regularly branch, it will be expensive to finish
an iteration, check whether the loop is finished, then jump to the beginning of the loop again.

Sometimes (if the loop is small enough), loop unrolling can completely eliminate the loop,
but in cases with many iterations, it can still partially unroll the loop to make the loop
execute the same sequence of multiple times in an iteration. For instance,

@code-examples[#:lang "racket" #:context #'here]|{
    (for ((i (in-range 0 20)))
     (print "hello world!"))
}|

Could be rewritten as,

@code-examples[#:lang "racket" #:context #'here]|{
    (for ((i (in-range 0 20 5)))
     (print "hello world!")
     (print "hello world!")
     (print "hello world!")
     (print "hello world!")
     (print "hello world!"))
}|

Or in a more sophisticated example,

@code-examples[#:lang "racket" #:context #'here]|{
    (define position #(1 3 2))
    (define translation #(2 2 1))
    (for/vector ((p position) (t translation))
      (+ p t))
}|

Could be rewritten,

@code-examples[#:lang "racket" #:context #'here]|{
    (define position #(1 3 2))
    (define translation #(2 2 1))
    (vector
      (+ (vector-ref position 0) (vector-ref translation 0))
      (+ (vector-ref position 1) (vector-ref translation 1))
      (+ (vector-ref position 2) (vector-ref translation 2)))
}|

Which is far more performant since we've fully eliminated branching.

@subsection{Code generation}

Finally, after transforming, tweaking, and pruning our original source code, we've now
reached the point in the process where we can start generating actual machine code for
our target architecture.

@subsubsection{From A-normal form to assembly}

@subsubsection{Peephole Optimizations}

@section{References}

@(include-section "bib.scrbl")

