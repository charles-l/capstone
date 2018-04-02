#lang scribble/lp2
@(require scribble-code-examples)

@tt{yacc} is a classic LALR parser that generates parser code that can be used to
parse source programs into parse trees.

With classic @tt{yacc} implementations, the generated code
is convoluted and difficult to understand, but is reasonably performant. Since
we're once again using a Racket implementation, the code will be generated
through a macro, so our parser source is still readable.

Let's build a parser to continue parsing our C grammar example from before with
@tt{yacc}. We'll assume a lexer with tokens like the ones we defined in the @tt{lex}
example.

@tt{yacc} grammars are constructed out of "rules." Unlike regular expression
rules, @tt{yacc} rules can be used to reference themselves recursively. This
provides a new degree of power.

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

Next we'll take a look at an extremely simple parser, one that simply parses
digits.

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
        (error "parsing failed" tok-name tok-value)))
      (start number)
      (tokens keyword-tokens value-tokens)
      (end EOF)
      (grammar
       (number ((INT) $1)
        ((DOUBLE) $1)))))}

This is a fully functioning parser! It doesn't parse much, and it only
constructs a tree of one node, but it will refuse to parse an invalid "program,"
so it meets the definition of a parser.

An example to demonstrate what it can do:

@code-examples[#:lang "at-exp racket" #:context #'here #:eval ev]|{
    (define test-input (open-input-string "2"))
    (num-parser (lambda () (c-lexer test-input)))

    (define test-input (open-input-string "a string which will make it fail"))
    (num-parser (lambda () (c-lexer test-input)))
}|

A @tt{number} can be an @tt{INT} token or a @tt{DOUBLE} token. Since we have
both @tt{INT} and @tt{DOUBLE} listed as numbers, the rule will match both tokens
in the token stream.

The variables with the '$' prefix (@tt{$1} in this case) reference the ith
variable in the match list. For instance, @tt{((A B C) (list $1 $3))}, would
return a list with the @tt{A} and @tt{C} tokens (i.e. the first and third).

We'll use these simple patterns to build the rest of our parser.

For a full-fledged parser we have to define a few things first. The overall
structure is seen below (we'll be defining individual rules next to make things
a bit clearer):

@chunk[<precs>
(precs
 (right =)
 (left - +)
 (left * /)) ]

@chunk[<*>
<includes>
(provide c-parser)
(define c-parser
 (parser
  (tokens keyword-tokens value-tokens)
  (error
   (lambda (tok-ok? tok-name tok-value)
    (error "parsing failed" tok-name tok-value)))
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

String, character, numeric, and other hardcoded values in a program are often
called "literals." Since they're easy to parse, we'll start with them.

@chunk[<literal>
  (literal
   ((INT) $1)
   ((DOUBLE) $1)
   ((CHAR) $1)
   ((STRING) $1))]

Since the token already contains enough information about the literal value, we
don't need to do much manipulation and can simply return the token directly with
@tt{$1}.

The next small unit worth defining is an expression. While not all languages
differentiate between expressions and statements (like Scheme), C does,
so our parser rules must reflect this so the parser fails if a statement is used in the
wrong place.

Expressions are a subset of statements (i.e. an expression is a statement but a
statement isn't an expression). Basically, a statement is something that has
it's own line to itself (e.g. if statements, variable declarations, and return
statements) whereas an expression may make up some portion of a statement (e.g.
variable assignment, binary operators, and function calls).

We'll start by defining variable assignment.

@chunk[<expr>
  (expr
   ((ID = expr) (list 'assign $1 $3))
   <binops>
   <function-call>)]

The assignment rule is slightly more sophisticated than the rules shown before.
First, it's self-referencing and recursive. Note that the recursion is on the
right side of the equals sign so we're not at risk of left-recursion here. When
we construct the returned results, we make a list with the leading cons cell
containing "assign". We'll use these tagged lists to different between nodes in
the parse tree.

Next we define our binops (once again using simple tagged lists to make the
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

This grammar employs left-recursion, since it is allowed in @tt{yacc}.
This is because @tt{yacc} is a bottom-up parser, so left-recursion doesn't
need to be eliminated.

And finally, we add a function call rule which references a rule we'll define
next for expression lists.

@chunk[<function-call>
((ID LPAREN expr-list RPAREN) (cons $1 $3))
]

That's the last rule for parsing expressions, so we'll look at expression lists
next. Since we don't know how many expressions will show up in an expression
list (for a function call, for instance), we must parse until the end of the
list.

@chunk[<expr-list>
  (expr-list
   ((expr COMMA expr-list) (cons $1 $3))
   ((expr) (list $1))
   (() (list)))]

By matching an expression @italic{and} a comma, followed by a recursive match of
the expression list again, we can determine when to stop. When the token after
the next expression is @italic{not} a comma, we can terminate our parsing.

Now we must define our rules for statements - rules for parsing instructions that must
exist on their own semi-colon-segmented line.

@chunk[<stmt>
    (stmt
     ((var-decl) $1)
     <return-stmt>
     <if-stmt>
     <for-loop>
     )]

Variable declaration simply parses two types, the first being the type and the
second being the identifier. We'll use this rule later on, so it's separated out
into its own rule (which is why it's referenced above, unlike the other rules
which are inserted inline).

@chunk[<var-decl>
  (var-decl
   ((ID ID) (list 'decl $1 $2)))]

Return statements are straightforward as well - they're simply the return
keyword followed by an expression.

@chunk[<return-stmt>
((return expr) (list 'return $2))
]

@tt{for} and @tt{if} statements are mostly straightforward despite being made up
of more tokens than any most other rules.

@chunk[<for-loop>
     ((for LPAREN expr SEMI expr SEMI expr LSQUARE stmt-list RSQUARE)
      (list 'for $3 $5 $7 $9))]

@chunk[<if-stmt>
     ((if LPAREN expr RPAREN LBRACE stmt-list RBRACE maybe-else)
      (append (list 'if $3 $6) $8))]

@tt{if} statements are odd from the perspective that they @italic{might} have an
else statement, which is why the rule to parse them is called "maybe-else."

@chunk[<maybe-else>
    (maybe-else
     ((else LBRACE stmt-list RBRACE) (list 'else $3))
     (() '()))]

The @tt{maybe-else} rule will return an empty list if no else statement is
parsed, so the @tt{if} parser rule will append nothing if there is no else
statement. When there is, an extra symbol @tt{'else} will be tagged on the end,
along with the block of statements (from the @tt{stmt-list} rule).

@chunk[<stmt-list>
    (stmt-list
     ((stmt SEMI) (list $1))
     ((stmt SEMI stmt-list) (cons $1 $3))
	 (() (list)))]

This rule is actually where whey parse the semi colons. We expect
statements to be followed by a semicolon, and if the next token is anything
other than a statement, the rule will terminate parsing.

The last significant rule is the top-level rule that the above rules build up
to: a function definition. While our starting rule should actually be a list of
function definitions, we'll keep it simple for now by just parsing one.

This rule is also simple (it just looks sophisticated because it matches more
tokens than the other rules) and also returns a tagged list.

@chunk[<func-def>
  (func-def
   ((ID ID LPAREN arg-list RPAREN LBRACE stmt-list RBRACE)
    (list $1 $2 $4 $7)))]

The final rule is the argument list helper which expects a @tt{var-decl} pair
like we defined early.

@chunk[<arg-list>
  (arg-list
   ((var-decl COMMA arg-list) (cons $1 $3))
   ((var-decl) (list $1))
   (() (list)))]


@code-examples[#:lang "at-exp racket" #:context #'here #:eval ev]|{
	(with-input-from-string
	  "int main() {
	  	int x = rand(0, 3);
	  	printf(\"my number is %d\", x);
	  }"
	  (lambda ()
		(c-parser (lambda () (c-lexer (current-input-port))))))
}|
