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

@chunk[<*>
    (require br-parser-tools/lex
     (prefix-in : br-parser-tools/lex-sre))

    (provide value-tokens keyword-tokens c-lexer)

    ; we'll call these value-tokens, since they reference a literal
    ; value in the source code
    (define-tokens value-tokens (INT DOUBLE CHAR STRING ID))

    ; these tokens don't need to hold a value since they're keywords, parts of
    ; syntax, or operators so the name defines what value they originally held
    (define-empty-tokens keyword-tokens
        (if else for * / + - < > = += -= return
         IF
         COMMA POUND SEMI LBRACE RBRACE LPAREN RPAREN LSQUARE RSQUARE EOF))

    (define c-lexer
      (lexer
        ((eof) 'EOF)

        ; this rule allows us to skip tabs, spaces, and newlines
        ; by recursively calling c-lexer on the rest of the input
        ((:or #\tab #\space #\newline) (c-lexer input-port))

        ; lets us match keywords that we then convert into their symbolic
        ; name so they're recognized as tokens
        ((:or "else" "for" "*" "/" "+" "-" "<" ">" "=" "+=" "-=" "return")
         ; lexeme is an implicit variable that is created created
         ; for whatever the rule matched
         (string->symbol lexeme))

        ("{" 'LBRACE)
        ("}" 'RBRACE)
        ("[" 'LSQUARE)
        ("]" 'RSQUARE)
        ("(" 'LPAREN)
        (")" 'RPAREN)
        (";" 'SEMI)
        ("#" 'POUND)
        ("," 'COMMA)

        ; equivalent to the regexp [0-9]+
        ((:+ (:/ "0" "9"))
         (token-INT (string->number lexeme)))

        ; equivalent to the regexp [0-9]+\.[0-9]+
        ((:seq (:+ (:/ "0" "9")) #\. (:+ (:/ "0" "9")))
         (token-DOUBLE (string->number lexeme)))

        ((:seq (:+ (:/ "a" "z") (:or (:/ "a" "z") (:/ "0" "9"))))
         (token-ID lexeme))

        ((:seq "'" any-char "'") (token-CHAR (string-ref lexeme 1)))

        ; equivalent to the regexp ["][^"]*["]
        ((:seq #\" (:* (char-complement #\")) #\")
         (token-STRING
           (substring lexeme 1 (- (string-length lexeme) 2))))))
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

