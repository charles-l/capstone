#lang scribble/lp2
@(require scribble-code-examples)
@(require scriblib/footnote)

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

@code-examples[#:lang "racket" #:context #'here]{
    (require racket/match)
    (require racket/string)

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
}

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

First, we'll define all possible tokens that our lexer can generate from the
source code. We'll call the first few @tt{value-tokens} since they contain literal
values from the original source code.

@chunk[<value-tokens>
    (define-tokens value-tokens (INT DOUBLE CHAR STRING ID))
]

Then we'll define the tokens that don't need to contain any literal values from
the source code, since the name of the token reflects what the original value was.

@chunk[<keyword-tokens>
(define-empty-tokens keyword-tokens
 (if else for * / + - < > = += -= return
  IF
  COMMA POUND SEMI LBRACE RBRACE LPAREN RPAREN LSQUARE RSQUARE EOF))
]

Now we define the matching rules for the lexer so it converts literal
strings in the original source into the correct token.

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



Since whitespace is semantically insignificant in C, we can ignore it. We'll
write a rule that skips whitespace by recursively calling the lexer on the
next token in the input.

@chunk[<skip-whitespace>
        ((:or #\tab #\space #\newline) (c-lexer input-port))
        ]

For the rules where the stringified token equals the name of the token itself,
we can just convert the string into a symbol and return it as the token. In this
parser, the lexer function automatically creates a variable called @tt{lexeme} which
stores the value of the string that was just read in.

@chunk[<match-keyword-tokens>
    ((:or "else" "for" "*" "/" "+" "-" "<" ">" "=" "+=" "-=" "return")
     (string->symbol lexeme)) ]

Then we have the keyword tokens that don't match since they have syntactic meaning
in Racket (so its clearer not to use the symbol itself to reference them).

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

Then we'll parse integer numbers with a slightly more sophisticated rule. The @tt{:+} function means
to parse one or more of whatever arguments it has. The @tt{:/} function will match any character
in the range of ASCII characters it is passed. In our case, we want to match one or more ASCII
characters that are between the value '0' and '9'.

@chunk[<integer>
    ((:+ (:/ "0" "9"))
     (token-INT (string->number lexeme)))]

Doubles are just two integers separated by a decimal point, so we'll use the @tt{:seq} function to
parse a sequence of patterns.

@chunk[<double>
        ((:seq (:+ (:/ "0" "9")) #\. (:+ (:/ "0" "9")))
         (token-DOUBLE (string->number lexeme))) ]

Sometimes in our grammar, we expect a word like a type name, variable name or function name, but don't
know exactly what that function will be. What we do know is that a valid identifier starts with a character
and has either characters or numbers in the rest of the name (they can be capitalized, and we can also have underscores
in a real C grammar, but we'll simplify it for now).

@chunk[<id>
    ((:seq (:/ "a" "z") (:+ (:or (:/ "a" "z") (:/ "0" "9"))))
     (token-ID lexeme))]

Characters are notated by a quote followed by any character and another quote.
@chunk[<char>
        ((:seq "'" any-char "'") (token-CHAR (string-ref lexeme 1)))
        ]

And finally, strings will be denoted by a double quote, followed by zero or more characters that
@italic{aren't} a double quote.

We'll use the @tt{:*} function to match zero or more characters, and we'll use the char-complement
function to match any character except the double quote.

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

@(define ev (make-code-eval #:lang "racket" #:allow-for-require `(,(string->path "c-lexer.scrbl"))))
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

