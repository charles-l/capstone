#lang scribble/book

@(require scribble-code-examples)
@(require scribble/lp-include)
@(require scriblib/footnote)
@(require scriblib/figure)
@(require "util.rkt")
@(require plot/pict)

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

@subsection{Static vs dynamic typing}

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


Choosing between static and dynamic typing is a decision of whether you value safety or
flexibility more. With dynamic type systems, variables just point at values that
track type information. This means that variables can be reassigned to different
types, and functions aren't guaranteed to return any specific type. This makes code easier
to write, but harder to reason about (especially in a formal manner).

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

Given an arbitrary @tt{x} with an unknown type, we're unsure of what our return
value will be. If we call @tt{f} later in a process and are unsure if its value
the uncertainty of our result only increases. For quick prototyping, dynamic
types are convenient and stay out of the way. However, once a system grows, they
can make debugging and maintenance harder (since there are few assurances about
types).

In comparison, static typing @italic{requires} the programmer to notate type
information for functions and variables. Then, the type system can prove
correctness about types and ensure that every edge case is accounted for.

@code-examples[#:lang "typed/racket" #:context #'here #:show-lang-line #t]|{
    (define (f (x : Real))
      (add1 x))

    (f 2)
    (f "static typing makes code safe!")
}|

Since the types are defined by the programmer and there's no room for
ambiguities, the types of variables can be narrowed at compile time. Most of
the time, the type of every variable is known (with only a few exceptions to
allow for polymorphic functions that encourage code reuse).

Storing and checking type information at runtime uses more resources than
pre-computing the information at compile time. Static typing tends to result in
faster code at execution time.

@subsection{Strong vs weak typing}

The other axis that must be considered is strong vs weak types. Weak typing is
more "forgiving" in the sense that the language will attempt to cast the value
to be what the programmer "meant." This may be considered a convenience by some,
but it can also lead to subtle bugs where unexpected typecasts result in
incorrect types.

Most modern languages tend towards a stronger type system (and static languages
rarely allow strange casts). The biggest exception is C, which is a language
that was designed to write dangerous and unsafe code. Most scripting languages
are somewhat stringent about types (e.g. they won't let you add a number and a
string) and are usually allow implicit casting between floating point and decimal
numbers. A notable deviation from this is JavaScript which will attempt to cast
anything (even a function!) to a string if a concatenation operation is
attempted with a variable of a different type.

@subsection{Lazy vs strict evaluation}

Strict evaluation evaluates all the expressions in a statement before calling
the statement itself. Lazy evaluation waits until the last possible moment to
execute the arguments. If the argument is never used, it never gets evaluated.
Most languages use strict evaluation, with the usual exception being conditional
statements.

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
allows for some interesting abilities.

For instance, in strict evaluation, the following recursive function would loop forever
since there's no base condition.

@code-examples[#:lang "racket" #:context #'here #:show-lang-line #t]|{
    (define (dont-stop-believing)
      (cons 'hold-on-to-that-feeeeeeling (dont-stop-believing)))
}|

However, with Racket's lazy language implementation, this code will operate as a
lazy stream (which doesn't compute the next value in the stream until it is
needed). Therefore, we can have an infinite data structure.

@code-examples[#:lang "lazy" #:context #'here #:show-lang-line #t]|{
    (define (dont-stop-believing)
      (cons 'hold-on-to-that-feeeeeeling (dont-stop-believing)))

    (car (dont-stop-believing))

    (car (cdr (dont-stop-believing)))

    (cdr (dont-stop-believing))
}|

Looking at the last result, we can see it creates a promise@note{A
@bold{promise} is simple way of delaying an evaluation (by wrapping
it in an thunk (an anonymous function)), and @bold{forcing} the value to be
computed when it is needed.}, which is how strictly evaluated languages
like Racket can allow lazy languages.

Lazy evaluation doesn't work properly with side-effects, (since an argument might
change while a function is running). Lazy-evaluation isn't found in most
programming languages besides a few purely functional languages like Haskell,
and in a few languages that encourage the functional paradigm (like Scheme and
OCaml).

@subsection{Lambda Calculus}

@section{Parsing and Semantic Analysis}

Like spoken languages, programming languages have a grammar. However human
languages aren't extremely strict in their rules and are full of weak rules and
squishy contextual sentences. This is why natural language processing is so
hard, and why we can't program computers in English.

The reason a programming language is understandable to the stupid metal
machine is because it is described in a (usually context-free) formal grammar.
The formal grammars that language designers use to describe the syntax for a
grammar can easily be "parsed." The parser for a language will read a text file
as input, and will convert that code into a "parse tree" which is a tree
structure that is more convenient for a compiler or interpreter to understand.

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

@(lp-include "c-lexer.scrbl")

@subsection{A @tt{yacc} parser}

@(lp-include "c-parser.scrbl")

@subsection{A parser combinator }

@(lp-include "c-parser-combinator.scrbl")

No book on language implementation is complete without a brief look into parsing
techniques. However, from here on out, we will be using s-expressions to
represent our language since it is a convenient format to use to represtent ASTs
(especially in Racket). Writing an s-expression parser is trivial, but since
Racket has a built in @tt{read} function (which parses our syntax for us), we'll
be using it in conjunction with simple pattern matching to parse our languages.

@section{Interpreters}

@(lp-include "mccarthy-lisp.scrbl")

@subsection{Graph walking}

Graph walking interpreters are straightforward to implement, since once the
source has been parsed into a parse tree and a few minor transormatinos have
happened, the interpreter executes the graph directly. Since the source closely
represents the runtime structure of the code, runtime errors are easier to catch
and debug. Finally, metaprogramming and reflection are simpler since no extra
metadata has to be stored to ensure the runtime environment has access to the
information it needs.

The downside of graph walking interpreters is that they're very slow. Traversing
a graph generally entails following pointers from node to node, which is far
slower to execute on modern CPU's than sets of (mostly) contigious instructions.
Most programming language implementations for intpreteted languages start out as
graph walking interpreters, since they're simpler to implement, before
eventually implementing a virtual machine (VM) to improve performance.

Essentially, we've already implemented a graph walking interpeter with the
Lisp implementation on the last page. Lisp code is already a tree once
@tt{(read)} in, and we simply traverse the tree to execute our program.

@subsection{Virtual Machine}

So you've got a graph intepreter for a language and now you want to speed it up.
The next step is to design a virtual machine (VM) for your language that is
similar to real hardware but close enough to the semantics of the language that
it's not hard to translate beetween source code and VM bytecode.

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

While this strategy requires more processing to initially convert the source to
bytecode, it can speed up interpretation tremendously. To mitigate this issue,
some languages require a separate compile step to generate the bytecode. This
bytecode file is then directly interpreted by the virtual machine
implementation. An example of this is Java, which requires a compile step with
@tt{javac} (the Java compiler) to generate a @tt{.java} file which can then be
run by the JVM implementation installed on the machine. Even languages like
Python will implicitely generate @tt{.pyc} (python bytecode) files for libraries
to speed up imports.

Some languages like Ruby and Lua don't bother with caching the bytecode and
re-parse the file every time it is loaded because performance isn't a focus, or
because the language is meant for embedded purposes (with small source files
that are fast to parse).

It's important to realize that this compilation step is happening. The
term "interpreter" can be misleading when most language "interpreters" are
actually bytecode interpreters that require a compilation step to the VM
bytecode. While the bytecode interpreter is a proper interpreter (usually),
modern interpreters are more often naive compilers than true graph-walking
interpreters.

@subsubsection{Stack based}

Probably the simplest virtual machine model is a stack-based machine. This
design of virtual machine has no registers, instead relying on pushing and
popping from the stack, and operating on the top elements in the stack
to perform computations.

A simple stackbased bytecode might look like:

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
of the stack, so the next operation can access it. Stack machines are popular
because the simple design is appealing. Issues related to register allocation
are gone because of design at a @italic{slight} cost of performance.

@subsubsection{Register based}

@subsubsection{JIT compilation}

@section{Compilers}

@subsection{Optimization passes}

@subsection{Code generation}

@(include-section "bib.scrbl")

