#lang scribble/book
@(require scribble/manual)
@(bibliography
  (bib-entry
    #:key "Tromp"
    #:title "Binary Lambda Calculus and Combinatory Logic"
    #:is-book? #f
    #:author "John Tromp"
    #:url "https://tromp.github.io/cl/LC.pdf")
  (bib-entry
    #:key "DrS"
    #:title "DrScheme: A Programming Environment for Scheme"
    #:is-book? #f
    #:author "Robert Bruce Findler, John Clements, Cormac Flanagan, Matthew Flatt, Shriram Krishnamurthi, Paul Steckler and Matthias Felleisen"
    #:url "https://users.soe.ucsc.edu/~cormac/papers/jfp01.pdf")
  (bib-entry
    #:key "Leijen, 2001"
    #:title "Parsec: Direct Style Monadic Parser Combinators For The Real World"
    #:is-book? #f
    #:author "Daan Leijen and Erik Meijer"
    #:url "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf")
  (bib-entry
    #:key "SICP"
    #:title "Structure and Interpretation of Computer Programs"
    #:is-book? #t
    #:author "Harold Abelson, Jerry Sussman and Julia Sussman")
  (bib-entry
    #:key "Types and PL"
    #:title "Types and Programming Languages"
    #:is-book? #t
    #:author "Benjamin C. Pierce")
  (bib-entry
    #:key "Parsack docs"
    #:title "Parsack Documentation"
    #:is-book? #f
    #:author "Stephen Chang"
    #:url "http://docs.racket-lang.org/parsack/index.html")
  (bib-entry
      #:key "McCarthy, 1978"
      #:title "A Micro-manual for Lisp - Not the Whole Truth"
      #:is-book? #f
      #:author "John McCarthy")
  (bib-entry
        #:key "Church, 1932"
        #:title "A Set of Postulates for the Foundation of Logic."
        #:is-book? #f
        #:author "Alonzo Church")

  (bib-entry
      #:key "Yunhe"
      #:title "Virtual Machine Showdown: Stack Versus Registers"
      #:is-book? #f
      #:author "Yunhe Shi, David Gregg, Andrew Beatty and M. Anton Ertl")

  (bib-entry
      #:key "Flanagan, 1993"
      #:title "The Essence of Compiling with Continuations"
      #:is-book? #f
      #:author "Cormac Flanagan, Amr Sabry, Bruce F. Duba and Matthias Felleisen")
  (bib-entry
      #:key "Appel, 1998"
      #:title "Modern Compiler Implementation in Java"
      #:is-book? #t
      #:author "Andrew W. Appel")
  (bib-entry
      #:key "Chomsky, 1956"
      #:title "Three Models for the Description of Language"
      #:is-book? #f
      #:author "Noam Chomsky")
  (bib-entry
      #:key "Sarkar, 2005"
      #:title "A Nanopass Framework for Compiler Education"
      #:is-book? #f
      #:author "Dipanwita Sarkar, Oscar Waddell and R. Kent Dybvig")
  (bib-entry
      #:key "Keep, 2012"
      #:title "A Nanopass Framework for Commercial Compiler Development"
      #:is-book? #f
      #:author "Andrew W. Keep")
  (bib-entry
      #:key "Keep, 2013"
      #:title "A Nanopass Framework for Commercial Compiler Development"
      #:is-book? #f
      #:author "Andrew W. Keep and R. Kent Dybvig")
  (bib-entry
      #:key "nanopass-racket documentation"
      #:title "Nanopass Framework"
      #:is-book? #f
      #:author "Andrew W. Keep and Leif Anderson"
      #:url "https://docs.racket-lang.org/nanopass/index.html")
  (bib-entry
      #:key "Dragon book"
      #:title "Compilers: Principles, Techniques, & Tools"
      #:is-book? #t
      #:author "Alfred V. Aho, Monica S. Lam, Ravi Sethi and Jeffrey D. Ullman")
  )

@;{https://arxiv.org/pdf/1611.00467.pdf}
@;{Dragon book}
@;{PAIP}
@;{http://library.readscheme.org/page8.html}
