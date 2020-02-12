#lang scribble/lp2
@(require scribble-code-examples)
@(require scriblib/footnote)

Lexing is the step that "tokenizes" the input source code. The
tokenisation process splits the source into a set of words, each with an
associated type (e.g. keywords, numbers, and identifiers). One of the goals
of lexing is to removes non-semantic whitespace from the code, so any
spurious spaces, tabs or newlines are ignored by later passes. This can
simplify the parsing rules.

For instance, consider a string such as:

"It does not do to leave a live dragon out of your calculations, if you live near him"

We could use spaces as a delimiter for words, and we could assign the
nouns that are magical beasts with a special `MAGICAL-BEAST` tag.

@code-examples[#:lang "racket" #:context #'here]{
    (require racket/match)
    (require racket/string)

    (define string-to-lex "It does not do to leave a live dragon
	            out of your calculations, if you live near him")
    (define magical-beasts '("dragon"))
    (define prepositions '("near"))
    (define (magical-beast? x) (member x magical-beasts))
    (define (preposition? x) (member x prepositions))

    (map (lambda (s)
          (match s
           ((? magical-beast?) `(MAGICAL-BEAST ,s))
           ((? preposition?) `(PREPOSITION ,s))
           ((? string?) `(TOKEN ,s))))
     (string-split string-to-lex))
}

This token list can then be passed to the parser to build the parse tree.

In Racket (and other languages that support string pattern matching with
regular expressions), lexing is easy to do without external library
support. But since @tt{lex} (and tools like it) are popular in other
languages, it's worth writing a simple lexer using a @tt{lex} library in
Racket.

We'll now define a lexer for a subset of C's grammar. We'll use the lex
tool that is implemented in a Racket package called @tt{br-parser-tools}.

First, we'll define all possible tokens that our lexer can generate from the
source code. We'll call the first few @tt{value-tokens} since they contain literal
values from the original source code.

@chunk[<value-tokens>
    (define-tokens value-tokens (INT DOUBLE CHAR STRING ID))
]

Then we'll define the tokens that don't need to contain any literal values
from the source code, since the name of the token reflects the original
value of the word in the source code.

@chunk[<keyword-tokens>
(define-empty-tokens keyword-tokens
 (if else * / + - < > = += -= return
  COMMA POUND SEMI LBRACE RBRACE LPAREN RPAREN LSQUARE RSQUARE EOF))
]

The full lexer will have the following structure:

@chunk[<lexer>
        (define c-lexer
          (lexer
            ((eof) 'EOF)
            <skip-whitespace>
            <match-keyword-tokens>
            <syntax-symbols>
            <integer>
            <double>
            <id>
            <char>
            <string>))]

Since whitespace is semantically insignificant in C, we can completely
ignore it. We'll write a rule to skip whitespace by recursively calling
the lexer function itself on the next token in the input.

@chunk[<skip-whitespace>
        ((:or #\tab #\space #\newline) (c-lexer input-port))
        ]

The @tt{lex} rules are defined by matching a certain string, and returning
the symbol associated with the token. For the rules where the string can
easily be converted into the token of the same name, we'll convert the
string to a symbol with @tt{string->symbol} and return the token
immediately. The @tt{lex} tool automatically creates a variable called
@tt{lexeme} to store the value of the current string.

@chunk[<match-keyword-tokens>
    ((:or "if" "else" "*" "/" "+" "-" "<" ">" "=" "+=" "-=" "return")
     (string->symbol lexeme)) ]

Now we'll write the rules for matching the other tokens.

@chunk[<syntax-symbols>
    ("{" 'LBRACE)
    ("}" 'RBRACE)
    ("[" 'LSQUARE)
    ("]" 'RSQUARE)
    ("(" 'LPAREN)
    (")" 'RPAREN)
    (";" 'SEMI)
    ("#" 'POUND)
    ("," 'COMMA)]

To lex numbers, we'll have to write a slightly more sophisticated rule,
since it isn't viable to write a @tt{lex} rule to match
every possible number. We'll be using @italic{regular expressions} to do
this. Regular expressions are the simplest type of formal language to
parse, and are cheap to compute.

The @tt{:+} function lets us parse one or more matches of a sub-rule. The
@tt{:/} function will match any character in the range of ASCII characters
it is passed. In our case, we want to match the ASCII characters that are
between the value '0' and '9', and we'll use the @tt{:+} function to match
multiple digits.

@chunk[<integer>
    ((:+ (:/ "0" "9"))
     (token-INT (string->number lexeme)))]

Doubles are just two integers separated by a decimal point, so we'll use
the @tt{:seq} function to parse a sequence of patterns. Once the first
pattern fails to match, it moves onto the next one until it finishes
parsing.

@chunk[<double>
        ((:seq (:+ (:/ "0" "9")) #\. (:+ (:/ "0" "9")))
         (token-DOUBLE (string->number lexeme))) ]

Often in C, we expect a word for a type, variable, or function name, but
don't know what the name will be (since it is user defined). To parse
these types of @italic{identifiers} we must define a @tt{lex} rule to
match an identifier. A valid identifier starts with an ASCII character and
has either characters or numbers in the rest of the name
(underscores and capital letters are valid in C's grammar as well, but
	     we'll leave them out for simplicity).

@chunk[<id>
    ((:seq (:/ "a" "z") (:* (:or (:/ "a" "z") (:/ "0" "9"))))
     (token-ID lexeme))]

C characters literals are notated by a quote followed by an ASCII
character and another quote.

@chunk[<char> ((:seq "'" any-char "'")
	       (token-CHAR (string-ref lexeme 1))) ]

And lastly, strings are denoted by a double quote, followed by zero or
more characters that @italic{aren't} a double quote.

We'll use the @tt{:*} function to match zero or more characters, and we'll
use the char-complement function to match any character that isn't the
double quote.

@chunk[<string>
        ((:seq #\" (:* (char-complement #\")) #\")
         (token-STRING
           (substring lexeme 1 (- (string-length lexeme) 2))))
        ]

Putting it all together, we get the final module file:

@chunk[<*>
    (require br-parser-tools/lex
     (prefix-in : br-parser-tools/lex-sre))

    (provide value-tokens keyword-tokens c-lexer)

    <value-tokens>
    <keyword-tokens>
    <lexer>

]

Our lexer can be used in the following manner
(though it generally isn't used on its own - we'll add the parser next):

@(define ev (make-code-eval #:lang "racket"))
@(ev '(require "c-lexer.scrbl"))

@code-examples[#:lang "racket" #:context #'here #:eval ev]{
    (define test-input
    (open-input-string "int main {
        printf(\"hello world!\n\");
        return 0;
    }"))

    ; now we'll lex the whole string
    (let lex-loop ()
     ; lex until we get EOF
     (let ((tok (c-lexer test-input)))
      (if (eq? tok 'EOF)
       '()
       (append (list tok) (lex-loop)))))
}

Racket uses a macro to generate the lexer code at compile time. In the
original @tt{lex} (designed for C), a developer embeds the lexer code in
a specially annotated block in a C program. Then, they call the @tt{lex}
utility on the file to generate the full C source for the lexer.

While lexers and parsers may seem similar, the key difference between the
two is that parsers are more sophisticated than lexers. A lexer simply
matches and splits the input string then the parser builds a tree from the
tokens with semantic structure. A lexer takes flat data as an input
(a string) and returns flat data (an array of tokens), whereas a parser
takes an array of tokens and constructs a tree.

