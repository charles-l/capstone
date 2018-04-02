#lang scribble/lp2
@(require scribble-code-examples)
@(require scribble-code-examples)
@(require scribble-math)
@(require "util.rkt")

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
	 (let ((t6 (anf-fib t4)) (t7 (anf-fib t5)))
	  (+ t6 t7)))))))
    (anf-fib 10)
}|

In this example the variables are named with temporaries since a human
isn't expected to write in ANF since it's an automated transformation in
the compiler.

The Scheme compiler we implement will include a pass that converts the
program into ANF form. It is implemented recursively by ensuring that
subexpressions are eliminated and replaced with temporary variables (these
temporary variable names are generated with @tt{(gensym)}, a built-in
Racket function that generates fresh variable names).

@chunk[<convert-to-anf>
(define-pass convert-to-anf : L1 (ir) -> L1.1 ()
 (definitions
  (define (value? m)
   (match m
    ((? constant?) #t)
    ((? variable?) #t)
    (else #f)))
  (with-output-language (L1.1 Expr)
   (define (maybe-normalize e gen-body)
    (if (value? e)
     (gen-body e)
     (let ((tmp-var (gensym)))
      `(let ,tmp-var ,(Expr e)
	  ,(gen-body tmp-var)))))
   (define (maybe-normalize* e* gen-body)
    (match e*
     ('() (gen-body '()))
     (`(,e . ,rest)
      (maybe-normalize e
       (λ (t)
	(maybe-normalize* rest
	 (λ (t*)
	  (gen-body (cons t t*)))))))))))
    (Expr : Expr (ir) -> Expr ()
     ((if ,e0 ,[e1] ,[e2])
      (maybe-normalize e0
       (λ (v)
	`(if ,v ,e1 ,e2))))
     ((,e0 ,e1 ...)
      (maybe-normalize e0 (λ (t)
			   (maybe-normalize* e1
			    (λ (t*)
			     `(,t ,t* ...))))))))
]

@subsection{α-conversion}

In λ-calculus, two programs may be equivalent, despite having different
binding names. For instance, the identify function, is the identify
function no matter what the argument is named.

@$${\lambda i \; . \; i}
@$${\lambda j \; . \; j}

These functions are α-equivalent (i.e. they're semantically equivalent),
and we can make them syntactically equivalent by transforming them using
α-conversion (alpha conversion). For instance, if we rename @${j} to
@${i} in the second example,

@$${\lambda j \; . \; j \Rightarrow \lambda i \; . \; i}

it is equivalent to the first equation.

α-conversion is not only useful for checking equivalence. It can also be
used to rename variables. When variables are shadowed, the order in which
they appear is significant.

@$${\lambda x, y \; . \; ((\lambda x \; . \; x) \; y) + x}

To prevent later passes from having to track environment information to
properly shadow variables, α-conversion is done to ensure each variable
name is unique. For instance, the previous expression could be converted
into the following to differentiate between shadowed variables:

@$${\lambda x.0, y \; . \; ((\lambda x.1 \; . \; x.1) \; y) + x.0}

The pass could be implemented as follows (this pass also does basic
desugaring):

@chunk[<desugar-and-alpha-conversion>
(define-pass parse-and-desugar : * (e) -> L0 ()
 (definitions
  (define (in-env? env e)
   (hash-has-key? env e))
  (define (extend-env env e)
   (if (in-env? env e)
    (hash-set env e (add1 (hash-ref env e)))
    (hash-set env e 0)))
  (define (var-name env e)
   (if (%name? e)
    e
    (string->symbol
     (string-append
      (symbol->string e)
      "."
      (number->string (hash-ref env e))))))
  (define (maybe-beginify e)
   (if (= (length e) 1)
    (car e)
    (cons 'begin e)))
  (define (Expr* e* env)
   (map (curryr Expr env) e*))
    (with-output-language (L0 Expr)
     (define (expand-let let-e env)
      (match let-e
       (`(let ,bindings ,body ...)
	(let ((env* (foldl
		     (lambda (n e) (extend-env e (car n)))
		     env
		     bindings)))
	 (define (expand-bindings b)
	  (match b
	   ('() (Expr (maybe-beginify body) env*))
	   (`((,x : ,t ,e) . ,rest)
	    (unless (or (variable? x) (type? t))
	     (error "let binding is invalid" `(,x : ,t)))
	    `(let ,(var-name env* x) ,t ,(Expr e env)
		,(expand-bindings rest)))))
	 (expand-bindings bindings)))
       (else
	(error "invalid let form" let-e))))))
(Expr : * (e env) -> Expr ()
 (match e
  ((? constant?) e)
  ((? variable?) (var-name env e))
  (`(begin ,exprs ...)
   (let ((e (Expr* exprs env)))
    `(begin ,(drop-right e 1) ... ,(last e))))
  (`(if ,a ,b ,c)
   `(if ,(Expr a env) ,(Expr b env) ,(Expr c env)))
  (`(,(or 'lambda 'λ) ,type ,args ,body ...)
   (when (check-duplicates args)
    (error "duplicate arg names"))
   (let ((env* (foldl
		(lambda (n e) (extend-env e n))
		env
		args)))
    `(λ ,type (,args ...) ,(Expr (maybe-beginify body) env*))))
  (`(let ,_ ...)
   (expand-let e env))
  (`(,f ,args ...)
   `(,(Expr f env) ,(Expr* args env) ...))))
    (Expr e (hash)))]


@subsection{Type checking}

Since the Scheme we implement includes type information in the source code
we will include a pass that does basic type-checking, and discards type
information for later passes (since it's irrelevent in later stages in the
pipeline). Type-checking can be implemented as a sort of
pseudo-interpreter that evaluates and checks type information in the
program.

Type checking will track types in a symbol-table, which is passed along as
the @tt{env}.

@chunk[<type-check>
(define-pass type-check-and-discard-type-info : L0 (ir) -> L1 ()
 (definitions
  (define (check env x t)
   (let ((xt (infer x env)))
    (unless (equal? xt t)
     (error "expected" x "to be of type" t "but was" xt))
    xt)))
 (infer : Expr (ir env) -> * (type)
  (,x (env-lookup-type env x))
  (,c (match c
       ((? number?) 'int)
       ((? boolean?) 'bool)
       ((? char?) 'char)))
  ((begin ,e* ... ,e) (infer e env))
  ((if ,e0 ,e1 ,e2)
   (check env e0 'bool)
   (let ((t1 (infer e1 env)) (t2 (infer e2 env)))
    (unless (equal? t1 t2)
     (error "if statement paths must return same type, but got" t1 "and" t2))
    t1))
  ((λ ,t (,x* ...) ,body)
   (let ((env* (foldl
		(lambda (c e) (env-add e (car c) (cdr c)))
		env
		(map (lambda (n t) (cons n (binding n t)))
		 x*
		 (drop-right (cdr t) 1)))))
    (check env* body (last t))
    t))
    ((let ,x ,t ,e ,body)
     (let ((env* (env-add env x (binding x t))))
      (check env* e t)
      (infer body env*)))
    ((,e0 ,e1 ...)
     (let* ((fty (infer e0 env)) (rargsty (map (curryr infer env) e1))
	    (argsty (drop-right (cdr fty) 1)))
      (unless (equal? argsty rargsty)
       (error "function has incorrect type - expecting args of type"
	argsty
	"but got"
	rargsty
	"for"
	(cons e0 e1)))
      (last fty))))
    (Expr : Expr (ir env) -> Expr ()
     ((let ,x ,t ,[e] ,body)
      `(let ,x ,e
	  ,(Expr body (env-add env x (binding x t)))))
     ((λ ,t (,x* ...) ,[body])
      `(λ (,x* ...) ,body)))
    (infer ir (hash))
    (Expr ir (hash)))]

@subsection{Functional compiler optimizations}

Most of the passes in a real-world compiler's backend are dedicated to code
optimization. The term code optimization is a bit misleading, since it
doesn't result in optimal code. It generates code that may be more performant because of
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

@subsection{Various other optimization techniques}

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

TODO lambda lifting

@subsection{Code generation for x86}

Finally, after transforming, tweaking, and pruning our original source code, we've now
reached the point in the process where we can start generating actual machine code for
our target architecture.

@subsubsection{A brief review of assembly}

Assembly is the lowest level series of an instructions a programmer can
provide to a computer. Most CPU's have hundreds (or thousands) of
instructions for performing mathematical, boolean algebra, memory
manipulation, and code branching operations. For the x86 architecture,
most of these instructions can be safely ignored as they're only used in
rare cases, or are kept for backwards compatibility.

Despite the scores of instructions available, CPU's are fundamentally the
same as pocket calculators, just with more memory, and more conditional
logic. CPU's have a set of @italic{registers} which are each capable of
holding a few dozen bits. For instance, 32-bit machines primarily have
32-bit registers, 64-bit machines have 64-bit registers.

Variables in assembly are just memory locations. There is no name
assocated with a variable binding, so once a program is compiled to
machine code, variable names are lost. Global variables are stored in
static locations in memory (generally precomputed before compilation), and
local variables are stored on the runtime stack.

The instructions that a program is comprised of are stored in memory in
the "text" segment. An instruction pointer points at the current machine
code instruction to execute, and can be moved to an arbitrary instruction
to perform a jump (also known as a branch). Jumps are how loops and
conditional expressions are implemented. A loop jumps back to a prior
instruction and executes the same instructions again, and conditionals are
implemented by choosing which set of instructions to execute depending on
a condition.

CPU operations are performed on registers, and must load data from memory
into a register before using it. Once an operation is completed and the
data needs to be stored for later use, it will be put back into a memory
location. Some registers are special purpose, and must be used when using
certain instructions, while the rest are general purpose registers for
storing and transforming temporary values with most instructions.

32-bit register names are prefixed with an "e". In our compiler, the
immediate value (i.e. the current value we return from each expression) is
stored in @tt{%eax}. @tt{%esp} (stack pointer) and @tt{%ebp} (base
pointer) are important registers since they track memory address relevant
to the current stack frame. @tt{%eip} is the instruction pointer register,
and can be manipulated to perform jumps.

The necessary instructions for our compiler are a small subset of the
instructions available on an x86 machine.

The @tt{mov} instruction can move 4-bytes constants and values between
registers and memory locations. For instance, @tt{movl $4, %eax} means
"move the long (i.e. 4-byte) constant 4 into the @tt{%eax} register" and
@tt{movl -4(%ebp), %ebx} means "move the long value stored at the memory
location @tt{%ebp - 4} to @tt{%ebx}."

Conditionals can't be implemented without comparisons, which is when the
comparison instruction @tt{cmp} is used. @tt{cmp} compares its two
arguments and sets a flag in the CPU depending on whether the first value
is equal to, less than, or greater than the second value.

However, the @tt{cmp} instruction doesn't change @tt{%eip} to branch. The
instruction directly after determines the jump. @tt{jz} will jump if the
@tt{cmp} results were equal. @tt{jne} jumps if they're not equal, @tt{jg}
jumps if greater than. Unconditional jumps that don't rely on the result
of a @tt{cmp} can be made with the @tt{jmp} instruction.

@tt{push} and @tt{pop} operate on the runtime stack, and can be used to
save a registers value to, or restore it from the stack. Underneath the
hood they actually juggle register values and keep track of the top of the
stack with @tt{%esp}.

@tt{call} and @tt{ret} enforce C-calling convention style function calls
and function returns. Some extra boilerplate is required to save and
restore the @tt{%ebp} and @tt{%esp} registers, but @tt{call} and @tt{ret}
track return addresses and ensure the proper value is returned from the
function when it completes.

We'll be targeting the x86 architecture, since it is one of the most
prevalent architectures for desktop computers. x86-64 is similar, but has
larger 8-byte registers and passes arguments through registers rather than
pushing them onto the stack when calling functions. However, x86-64 is
backwards compatible with x86 (32-bit), so we'll use 32-bit conventions.
It isn't necessary to use C calling conventions, and a high-performance
functional compiler likely would use a custom convention. However, by
using C calling conventions, we're able to use the @tt{call} instruction,
and interop with C code.

The code generation rules are trivial because we've transformed the code
into a format that is simple to translate. The structure of the code is
consistently linear, meaning each expression clearly translates to a few
assembly instructions.

@graphviz{
    digraph G {
	nodesep=.05;
	rankdir=LR;
	node [shape=record,width=.4,height=.1];
	node0 [label = "<a3>...|argument 3|argument 2|argument 1|return address|<a0>ebp|local 1|local 2|<a1>local 3|<a4>...",height=2.0];
	ebp [shape="plaintext"];
	esp [shape="plaintext"];
	"higher addresses" [shape="plaintext"];
	ebp -> node0:a0;
	esp -> node0:a1;
	"higher addresses" -> node0:a3:nw;
	"lower addresses (stack grows down)" [shape="plaintext"];
	"lower addresses (stack grows down)" -> node0:a4:sw;
    }
}

Local variables are pushed onto the stack and can be accessed by adding
offsets to @tt{ebp} (of 4 byte intervals for 32-bit x86). For instance, to
access @tt{local 1} using pseudo C syntax, @tt{*(%ebp - 4)} (i.e. we
dereference the memory location stored in %ebp minus an offset of
4 bytes).

Arguments are stored in a different portion of the stack and must be
accessed by adding an offset to @tt{%ebp}. Of course, an extra 4 bytes
must be added to step over the stack location that stores the return
address.

Function calls require some boilerplate to keep the stack in this
structure.

@chunk[<codegen-x86>
(define-pass emit-asm : L2 (ir) -> * ()
 (definitions
  (define (frame-locals frame)
   (filter (compose positive? cdr) frame))

  (define (slot-index frame e)
   (cond ((findf (lambda (x) (eq? (car x) e)) frame) => cdr)
    (else (error "failed to get index for" e))))

  (define (emit-immediate frame e)
   (match e
    ((? variable?)
     (if (%name? e)
      (format "$~a" (clean-%name e))
      (format "~a(%ebp)" (- (* +word-size+ (slot-index frame e))))))
    ((? number?) (format "$~a" e))
    ((? boolean?) (format "$~a" (if e 1 0))))))

 (Expr : Expr (ir (frame '())) -> * ()
  (,x (printf "movl ~a, %eax\n" (emit-immediate frame x)))
  (,c (printf "movl ~a, %eax\n" (emit-immediate frame c)))
  ((begin ,e* ... ,e) (begin
      (map (curryr Expr frame) e*)
      (Expr e frame)))
  ((if ,e0 ,e1 ,e2)
   (let ((else-label (gensym 'else)) (end-label 'end))
    (Expr e0 frame)
    (printf "cmp $0, %eax\n")
    (printf "jz ~a\n" else-label)
    (Expr e1 frame)
    (printf "jmp ~a\n" end-label)
    (printf "~a:\n" else-label)
    (Expr e2 frame)
    (printf "~a:\n" end-label)))
  ((let ,x ,e ,body)
   (printf "subl $4, %esp\n")
   (Expr e frame)
   (let ((slot (add1 (length (frame-locals frame)))))
    (printf "movl %eax, ~a(%ebp)\n" (- (* +word-size+ slot)))
    (Expr body (cons (cons x slot) frame))))
  ((lref ,x)
   (printf "movl $~a, %eax\n" x))
  ((,e0 ,e1 ...)
   (Expr e0 frame)
   (for-each (lambda (v)
	    (printf "pushl ~a\n" (emit-immediate frame v)))
    e1)
   (printf "call *%eax\n")))
  (Func : Func (l) -> * ()
   ((label ,x (,x* ...) ,body)
    (printf ".global ~a\n" x)
    (printf ".type ~a @function\n" x)
    (printf ".align 8\n")
    (printf "~a:\n" x)
    (printf "pushl %ebp\n")
    (printf "movl %esp, %ebp\n")
    (Expr body
     (map
    (lambda (a i)
     (cons a (- i)))
    x*
    (iota (length x*) 2)))
    (printf "movl %ebp, %esp\n")
    (printf "popl %ebp\n")
    (printf "ret\n")))
  (Expr ir '())
  (Func ir))
]
