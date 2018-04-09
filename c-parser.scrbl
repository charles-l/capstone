#lang scribble/lp2
@(require scribble-code-examples)
@(require scribble-code-examples)
@(require scriblib/footnote)

@tt{yacc} is a tool for generating LALR@note{Look ahead, left-to-right,
rightmost deriviation} parsers.  The original tool was written in C,
and generated C source code, but we will be using a port of @tt{yacc} implemented
in the racket @tt{br-parser-tools} library.

With classic @tt{yacc} implementations, the generated code
is convoluted and difficult to understand (as most generated code is).
Since we're once using a Racket implementation, the parser generation step will be
hidden in a macro, so our parser source is more readable.

Let's build a parser to continue parsing our C grammar example from before with
@tt{yacc}. We'll assume a lexer with tokens like the ones we defined in the @tt{lex}
example.

@tt{yacc} grammars are constructed out of "rules." Unlike regular expression
rules, @tt{yacc} rules can reference themselves recursively. This
allows for more powerful parsing rules.

Let's start with adding a simple rule to our parser called @tt{number} that
parses numeric values.

First we'll include the necessary libraries

@chunk[<includes>
(require
 br-parser-tools/yacc
 br-parser-tools/lex
 (prefix-in : br-parser-tools/lex-sre))
(require "c-lexer.scrbl")
]

Then, we'll write the @tt{numbers} rule.

@(define ev (make-code-eval #:lang "racket" #:allow-for-require `(,(string->path "c-lexer.scrbl"))))
@(ev '(require br-parser-tools/yacc))
@(ev '(require br-parser-tools/lex))
@(ev '(require "c-lexer.scrbl"))
@(ev '(require "c-parser.scrbl"))
@code-examples[#:lang "racket" #:context #'here #:eval ev]{
    (define num-parser
     (parser
      (error
       (lambda (tok-ok? tok-name tok-value)
        (error "parsing failed, unexpected " tok-name tok-value)))
      (start number)
      (tokens keyword-tokens value-tokens)
      (end EOF)
      (grammar
       (number ((INT) $1)
        ((DOUBLE) $1)))))}

This is a fully functioning parser! It doesn't parse much, and it only
constructs a tree of one node, but it is capable of parsing valid "programs."

An example to demonstrate what it can do:

@code-examples[#:lang "at-exp racket" #:context #'here #:eval ev]|{
    (define test-input (open-input-string "2"))
    (num-parser (lambda () (c-lexer test-input)))

    (define test-input (open-input-string "a string of text to make it fail"))
    (num-parser (lambda () (c-lexer test-input)))
}|

A @tt{number} can be an @tt{INT} token or a @tt{DOUBLE} token. Since we have
both @tt{INT} and @tt{DOUBLE} listed as numbers, the rule will match either token
when it appears in the token stream.

The variables with the '$' prefix (@tt{$1} in this case) reference the i@superscript{th}
variable in the match list. For instance, @tt{((A B C) (list $1 $3))}, would
return a list with the @tt{A} and @tt{C} tokens (i.e. the first and third).

We'll use these simple patterns to build the rest of our parser.

For a full-fledged parser we have to define a few things first. The overall
structure is shown below:

@chunk[<*>
<includes>
(provide c-parser)
(define c-parser
 (parser
  (tokens keyword-tokens value-tokens)
  (error
   (lambda (tok-ok? tok-name tok-value)
    (error "parsing failed, unexpected" tok-name tok-value)))
  (start func-def)
  (end EOF)
  <precs>
  (grammar
   <literal>
   <expr-list>
   <expr>
   <var-decl>
   <func-def>
   <arg-list>
   <stmt>
   <maybe-else>
   <stmt-list>)))]

To avoid ambiguities when parsing infix operators, we'll define operator precedence next.

@chunk[<precs>
(precs
 (right =)
 (left - +)
 (left * /)) ]

Now we can write the individual grammar rules. String, character, numeric,
and other hardcoded values in a program are often called "literals." Since
they're easy to parse, we'll start with them.

@chunk[<literal>
  (literal
   ((INT) $1)
   ((DOUBLE) $1)
   ((CHAR) $1)
   ((STRING) $1))]

Since the token already contains the literal value, we don't need to do
any manipulation and can return the token directly with @tt{$1}.

Not every language differentiates between expressions and statements
as C does. Scheme for instance has no grammatical structure for
statements. Since C has statments, our parser must differentiate between
expressions and statements, so if a program attempts to use a statement
in the context of an expression, the parser will fail.

In our parser, we'll handle variable declaration statements, return
statements, and if statements. We will define these rules later.

Now we'll write a rule to parse expressions.

@chunk[<expr>
  (expr
   ((ID = expr) (list 'assign $1 $3))
   ((literal) $1)
   ((ID) $1)
   <binops>
   <function-call>)]

The assignment rule is slightly more sophisticated than the rules shown
before. First, it's recursive which allows us to write more powerful
rules.  Second, when we construct the returned results, we do a small
transformation to make it a list where the first element is the symbol
"assign." We tag the first element in the list to make differentiation between
different node types easier.

Next we define our infix operations (once again using simple tagged lists to make the
nodes differentiable).

@chunk[<binops>
   ((expr + expr) (list '+ $1 $3))
   ((expr - expr) (list '- $1 $3))
   ((expr * expr) (list '* $1 $3))
   ((expr / expr) (list '/ $1 $3))
   ((expr < expr) (list '< $1 $3))
   ((expr > expr) (list '> $1 $3))
   ((expr += expr) (list '+= $1 $3))
   ((expr -= expr) (list '-= $1 $3))
]

These grammar rules employ left-recursion, since it is allowed in @tt{yacc}.
This is because @tt{yacc} is a bottom-up parser, so left-recursion doesn't
need to be eliminated as it does in a top-down parser.

To finish our expression rules, we add a rule for parsing function calls.

@chunk[<function-call>
((ID LPAREN expr-list RPAREN) (cons $1 $3))
]

Since we don't know how many expressions will show up in an expression
list (for a function call, for instance), we must parse until the end of the
list.

@chunk[<expr-list>
  (expr-list
   ((expr COMMA expr-list) (cons $1 $3))
   ((expr) (list $1))
   (() (list)))]

By matching an expression @italic{and} a comma, followed by a recursive
match of the expression list again, we can determine when to stop. When
the token after the next expression is @italic{not} a comma, the parser
terminates with one of the last two rules.

Now we must define rules for to parse statements. Note that an expression
followed by a semi-colon is also a statement.

@chunk[<stmt>
    (stmt
     <if-stmt>
     <return-stmt>
     ((var-decl SEMI) $1)
     ((expr SEMI) $1)
     )]

Variable declaration simply parses two identifiers. The first is the type
of the variable and the second is its name. We'll use this @tt{var-decl}
later on for parameter definitions in our @tt{func-def} rule, so it has
its own rule.

@chunk[<var-decl>
  (var-decl
   ((ID ID) (list 'decl $1 $2)))]

Return statements are straightforward as well - they're simply the return
keyword followed by an expression and a semicolon to delimit the end of the statement.

@chunk[<return-stmt>
((return expr SEMI) (list 'return $2))
]

@tt{if} statements are composed of many tokens, and have an optional else statement.

@chunk[<if-stmt>
     ((if LPAREN expr RPAREN LBRACE stmt-list RBRACE maybe-else)
      (append (list 'if $3 $6) $8))]

We'll include a rule called @tt{maybe-else} which will return an empty list if the
else statement doesn't exist.

@chunk[<maybe-else>
    (maybe-else
     ((else LBRACE stmt-list RBRACE) (list 'else $3))
     (() '()))]

When @tt{maybe-else} rule returns an empty list the @tt{if} parser
rule will have nothing to @tt{append}. When there is, an else statement, the
@tt{'else} will be tagged on the end, along with the block of statements
(from the @tt{stmt-list} rule).

@chunk[<stmt-list>
    (stmt-list
     (() (list))
     ((stmt stmt-list) (cons $1 $2)))]

The last significant rule is the top-level rule that utilizes all the
above rules to construct function definitions. To parse a full-scale
C program, our starting rule would need to be a list of function
definitions, we'll keep it simple for now by just parsing one.

@chunk[<func-def>
  (func-def
   ((ID ID LPAREN arg-list RPAREN LBRACE stmt-list RBRACE)
    (list 'func-def $1 $2 $4 $7)))]

The final rule is the argument list helper which expects a list of @tt{var-decl}s that
we defined earlier.

@chunk[<arg-list>
  (arg-list
   ((var-decl COMMA arg-list) (cons $1 $3))
   ((var-decl) (list $1))
   (() (list)))]

Now we can parse C-ish looking programs! For example,

@code-examples[#:lang "at-exp racket" #:context #'here #:eval ev]|{
	(with-input-from-string
	  "int main() {
        int x;
        x = rand(0, 3);
        int i;
        i = 2;
        if(i < x) {
            printf(\"my number is %d\", x);
        }
      }"
	  (lambda ()
		(c-parser (lambda () (c-lexer (current-input-port))))))
}|

@code-examples[#:lang "at-exp racket" #:context #'here #:eval ev]|{
	(with-input-from-string
	  "int add(int a, int b) {
	    return a + b;
      }"
	  (lambda ()
		(c-parser (lambda () (c-lexer (current-input-port))))))
}|
