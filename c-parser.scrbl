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
]

Next we'll take a look at an extremely simple parser, one that simply parses
digits.

@chunk[<num-parser>
    (define num-parser
     (parser
      (tokens keyword-tokens value-tokens)
      (end EOF)
      (grammar
       (number ((INT) $1)
        ((DOUBLE) $1)))))]

This is a fully functioning parser! It doesn't parse much, and it only
constructs a tree of one node, but it will refuse to parse an invalid "program,"
so it meets the formal definition for a parser.

An example to demonstrate what it can do:

@code-examples[#:lang "at-exp racket" #:context #'here]|{
    (load "c-parser.scrbl")

    (define test-input (open-input-string "2"))
    (num-parser test-input)

    (define test-input (open-input-string "a string which will make it fail"))
    (num-parser test-input)
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

@chunk[<parser>
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
   <if-stmt>
   <maybe-else>
   <for-loop>
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
   <function-call-rule>)]

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

TODO: explain why there's no left-recursion

And finally, we add a function call rule which references a rule we'll define
next for expression lists.

@chunk[<function-call-rule>
((ID LPAREN expr-list RPAREN))
]

That's the last rule for parsing expressions, so we'll look at expression lists
next. Since we don't know how many expressions will show up in an expression
list (for a function call, for instance), we must parse until the end of the
list.

@chunk[<expr-list>
  (expr-list
   (((expr COMMA expr-list) (cons $1 $3))
    ((expr) (list $1))))]

By matching an expression @italic{and} a comma, followed by a recursive match of
the expression list again, we can determine when to stop. When the token after
the next expression is @italic{not} a comma, we can terminate our parsing.

Now we must define our rules for statements - rules for parsing instructions that must
exist on their own semi-colon-segmented line.

@chunk[<stmt>
    (stmt
     (var-decl $1)
     <return-stmt>
     <if-stmt>
     <for-stmt>
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

@chunk[<for-stmt>
     ; for loop
     ((for LPAREN expr SEMI expr SEMI expr LSQUARE stmt-list RSQUARE)
      (list 'for $3 $5 $7 $9))]

@chunk[<if-stmt>
     ; if statement
     ((IF LPAREN expr RPAREN LBRACE stmt-list RBRACE maybe-else)
      (append (list 'if expr stmt-list) maybe-else))]

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
     ((stmt SEMI stmt-list) (cons $1 $3)))]

This rule is actually where whey parse the semi colons. Again, we don't have to
worry about left-recursion for the [TODO: INSERT REASON] stated above. We expect
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
   ((var-decl 'COMMA arg-list) (cons $1 $3))
   ((var-decl) (list $1)))]


TODO: demo


