#lang scribble/lp2
@(require scribble-code-examples)
@(require scribble-code-examples)

This final portion of the book will focus on compilers. Compilers convert the
source language to a target language (generally assembly or VM bytecode). The
process happens in a series of passes, starting with parsing, semantic
analysis, and a series of optimization passes.

The later passes in a compiler (called the backend) are where a compiler really
diverges from an interpreter. Both an interpreter and a compiler may share a
front-end (i.e. the parsing/semantic analysis passes), but after that
point they diverge in what they do. An interpreter will take the
high-level IR from the from the front-end and either comiple it to
a byte-code after some simple optimization passes, or it will begin
interpreting the IR directly.

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
        "Parse tree" -> "Simplified parse tree" [label="Desugar"];
        "Simplified parse tree" -> "Annotated parse tree" [label="Semantic analysis"];
        "Annotated parse tree" -> "IR";
        "IR" -> "Optimized IR" [label="Optimization passes"]
        "Optimized IR" -> "A-normal form";
        "A-normal form" -> "Target source" [label="Code generation"]
    }
}

@subsection{Desugaring}

Most languages have shorthand syntax for common operations, like creating
literal arrays or hashes, referencing elements in a data structure with
square brackets, or using operations like @tt{+=} or @tt{++} (which are
just shorthand for incrementing a value by an amount and reassigning the
variable with the new value).

The shorthand syntax (known as @italic{syntactic sugar}), creates
extraneous expressions that must be handled by passes further down the
pipeline. These expressions can be simplified to a semantic equivalent in
a smaller core language with the same meaning (but more verbose syntax).

For instance, the creation of a @tt{vector} might be converted from:

@code-examples[#:lang "racket" #:context #'here]|{
    (vector 'a 'b 'c 'd)
}|

to:

@code-examples[#:lang "racket" #:context #'here]|{
    (define tmpvec (make-vector 4))
    (vector-set! tmpvec 0 'a)
    (vector-set! tmpvec 1 'b)
    (vector-set! tmpvec 2 'c)
    (vector-set! tmpvec 3 'd)
    tmpvec
}|

For Racket, it is a bit contrived to have the compiler perform desugaring,
since most language features are implemented as macros. In Racket, macros
allow the programmer do define their own syntactic sugar. However, in most
languages where the syntax isn't as customizable, desugaring is a useful
step that simplifies the structure of later passes.

@subsection{The IR}

After semantically checking the parse tree, the compiler converts the
parse tree into an intermediate representation (IR). The IR is designed to
make optimization and analysis easier. After the optimization and analysis
passes, the compiler generates machine code from the IR. Depending on the
complexity of the IR, there may be passes to convert the IR into
a lower-level IR that maps more closely to machine code.

Functional compilers regularly use continuation-passing style (CPS) or
A-normal form (ANF) as an intermediate representation.

@subsubsection{Continuation Passing Style}

@subsubsection{A-normal form}

A-normal form was introduced in @cite{Flanagan, 1993}, as an alternative
to CPS since CPS has clunky syntax and encodes some redundant information.
ANF is equivalent to CPS, and requires simpler transformation passes.

In ANF, every subexpression is reduced and lifted into its own temporary
variable. This means that function call arguments only reference
variables, and cannot be expressions themselves. When every subexpression
is an immediate value, translation to machine code is trivial.

Consider the following program:

@code-examples[#:lang "racket" #:context #'here]|{
    (define (fib n)
      (if (or (eq? n 1) (eq? n 2))
       1
       (+ (fib (- n 1)) (fib (- n 2)))))
    (fib 10)
}|

If we were to rewrite it in ANF:

@code-examples[#:lang "racket" #:context #'here]|{
    (define (anf-fib n)
     (let ((t1 (eq? n 1)) (t2 (eq? n 2)))
      (let ((t3 (or t1 t2)))
       (if t3
	1
	(let ((t4 (- n 1)) (t5 (- n 2)))
	 (let ((t6 (fib t4)) (t7 (fib t5)))
	  (+ t6 t7)))))))
    (anf-fib 10)
}|

In this example the variables are named with temporaries since a human
isn't expected to write in ANF since it's an automated transformation in
the compiler.

In the compiler implementation we combine desugaring with

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

