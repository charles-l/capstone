#lang slideshow
(require slideshow/text)
(require unstable/gui/slideshow)
(require (only-in scribble/base url))
(require (except-in plot/pict shade))

(current-main-font "Roboto Sans")

(define (code t)
  (apply vl-append (map tt (string-split t "\n"))))

(define (graphviz . str)
  (let* ((f (make-temporary-file "rkttmp~a.png"))
         (cmd (~a "printf \"" (string-replace (apply string-append str) "\""
                                              "\\\"")  "\" | dot -Tpng > " f)))
    (display cmd)
    (newline)
    (system cmd)
    (bitmap (~a f))))

(slide
  (bold (t "How to build a programming language (WIPS)")))

(slide
  (bt "What is a programming language?")
  'next
  (item "Formal language")
  'next
  (item "Programmer to machine")
  'next
  (item "Programmer to programmer"))

(slide
  (bt "Why should you care?")
  'next
  (item "Reason about errors")
  'next
  (item "Understand performance")
  'next
  (item "DSLs")
  'next
  (item "Build your own!"))

(slide
  (bitmap "progcat.jpg"))

(slide
  (bt "Paradigms")
  (item "Procedural")
  (item "Object-oriented")
  (item "Functional"))

(slide
  (bt "Types in languages")
  (t "Dynamic vs static")
  (t "Strong vs weak"))

(slide
  (t "Types in languages")
  (parameterize ((plot-width    800)
                 (plot-height   600)
                 (plot-font-size 30)
                 (plot-font-face "Liberation Sans")
                 (plot-x-label  "Weak ↔ Strong")
                 (plot-y-label  "Dynamic ↔ Static"))
    (define xs (build-list 20 (λ _ (random))))
    (define ys (build-list 20 (λ _ (random))))
    (plot '()
          #:x-min -1.1
          #:x-max 1.3
          #:y-min -1.1
          #:y-max 1.1)))

(slide
  (t "Types in languages")
  (parameterize ((plot-width    800)
                 (plot-height   600)
                 (plot-font-size 30)
                 (plot-font-face "Liberation Sans")
                 (plot-x-label  "Weak ↔ Strong")
                 (plot-y-label  "Dynamic ↔ Static"))
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
          #:y-max 1.1)))

(revealing-slide
  (two-columns
    (vc-append 20
      (bt "Interpreter")
      (reveal 1 (item "Dynamically executes code"))
      (reveal 2 (item "Slow"))
      (reveal 3 (item "More debug/runtime info")))
    (vc-append 20
      (bt "Compiler")
      (reveal 1 (item "Generates executable"))
      (reveal 2 (item "Fast"))
      (reveal 3 (item "Less debug/runtime info (unless explicitely added)")))))

(slide
  (t "Frontend")
  (t "Backend"))

(slide
  (bt "Parsing"))

(slide
  (para "The process of converting syntax to a graph structure using a formal grammar"))

(slide
  (item (tt "lex") "+" (tt "yacc"))
  (item "Parser combinator"))

(slide
  (para #:align 'center (tt "lex") (t "+") (tt "yacc"))
  (item "Standard for parsers")
  (item "Old projects"))

(slide
  (tt "lex")
  (item "Tokenizes")
  (item "Tags"))

(slide
  (vl-append
    (tt "int main(int argv, char **argv) {")
    (tt "  int x = rand_number();")
    (tt "  printf(\"%d\", x);")
    (tt "  return 0;")
    (tt "}"))
  (para "→ ..., id{'int'}, id{'x'}, equal, id{'rand_number'}, lparen, rparen, semi, ..."))

(slide
  (para (tt "yacc") "converts stream of tokens to" (italic (t "parse tree"))))

(slide
  (graphviz "digraph G {
            size = \"8,8\";
            ordering=out;
            node [shape = box];
            a [label=\"VARIABLE DECLARATION (int)\"];
            b [label=\"VAR NAME: x\"];
            c [label=\"FCALL: rand_number\"];
            d [label=\"ARGS: []\"];
            a -> b;
            a -> c -> d;
            }"))

(slide
  (t "Parser combinator"))

(slide
  (para "Functional solution to the problem")
  (para "Uses " (italic "higher order functions") " to compose parser functions together"))

(slide
  (para
    (vl-append
      (tt "parse(char, \"woot\") => 'w'")
      (tt "parse(num, \"13\") => 1")))
  'next
  (para (tt "parse(num, \"asdf\") => error ...")))

(slide
  (para (tt "parse(many(char), \"hax0r\") => \"hax\""))
  'next
  (para
    (tt "parse(many(or(char, num)), \"h0w t0 b a l33t hax0r?\")")
    (tt " => \"h0w\"")))

(slide
  (bitmap "runaway.jpg"))

(slide
  (bt "Compilers"))

(slide
  (item "Structured as passes")
  (item "Using nanopass framework for implementation")
  (item "Often written in C++, Haskell, or Lisp (or the language itself)"))

(slide
  (bt "Frontend")
  (graphviz "digraph G {
            size = \"8,8\";
            ordering=out;
            node [shape = box];
            a [label=\"lex\"];
            b [label=\"parse\"];
            c [label=\"semantic analysis\"];
            a -> b -> c;
            }"))

(slide
  (bt "Backend")
  (graphviz "digraph G {
            size = \"8,8\";
            ordering=out;
            node [shape = box];
            a [label=\"desugar\"];
            b [label=\"inlining\"];
            c [label=\"partial evaluation\"];
            d [label=\"deforestation\"];
            e [label=\"data-flow analysis\"];
            f [label=\"dead-code elimination\"];
            g [label=\"code generation\"];
            a -> b -> c -> d -> e -> f ->g;
            }"))

(slide
  (para "In truth, there aren't many pure interpreters anymore"))

(slide
  (para "Most modern \"interpreters\" compile to bytecode (interpreted by a VM)"))

(slide
  (bitmap "its-a-lie.jpg"))

(slide
  (item "Java")
  (item "Python")
  (item "Ruby")
  (item "JavaScript"))

(slide
  (item "Graph walking interpreters")
  (item "Bytecode interpreter"
        (item "Stack-based VM (JVM, CLR)")
        (item "Register-based VM (Lua/LuaJIT, V8)"))
  (item "Just-in-time compilation"))

(slide
  (para "Graph walking interpreters are straightforward"))

(slide
  (tt "So lets build one!"))

(slide
  (tt "the wizard book")
  (bitmap "sicp.jpg"))

(slide
  (bitmap "eval-apply.gif"))

(slide
  (t "metacircular evaluator")
  'next
  (small (t "which we're not gonna do 'cuz Lisp is special")))

(slide
  (bt "Parser")
  (small (code "from pyparsing import *
pexpr = Forward()
pvar = Word(alphas, alphanums)
ptrue = Keyword('true').setParseAction(lambda: True)
pfalse = Keyword('false').setParseAction(lambda: False)
pbool = ptrue | pfalse
pnum = Word(nums).setParseAction(lambda x: int(x[0]))
pliteral = pvar | pbool | pnum
pdecl_ = pvar + Literal('=') + pexpr
pdecl_.setParseAction(lambda toks: ParseResults(['=', toks[0], toks[2]]))
pdecl = Group(pdecl_)
pif = Group(Keyword('if') + pexpr + Suppress(':') + pexpr + pexpr)
plambda = Group(Keyword('lambda') +
                 Group(delimitedList(pvar)) +
                 Suppress(':') +
                 pexpr)
pbegin = Keyword('begin') + Suppress('{') + OneOrMore(pexpr) + Suppress('}')
pargs = Suppress('(') + delimitedList(pexpr) + Suppress(')')
pexpr_ = pargs | Empty()
pexpr << (pdecl + pexpr_ |
         pif + pexpr_ |
         plambda + pexpr_ |
         pbegin + pexpr_ |
         Group(pliteral + pargs) |
         pliteral) + Maybe(Literal(';'))"))
  )

(define (ieval str (print-source #t))
  (string-append
    (if print-source
      (string-append str " => ")
      "")
    (with-output-to-string
      (thunk
        (system (format "echo \"from interp import *\n~a\" | python3"
                        (string-replace str "\"" "\\\"")))))))

(define (pieval str (print-source #t) (pre-eval #f))
  (ieval
    (string-append (if pre-eval
                     (string-append pre-eval "\n")
                     "")
                   (format "print(~a)" str)) print-source))

(slide
  (vl-append
    (tt "def eval(expr, env):")
    (tt "  if is_literal(expr):")
    (tt "    return expr")
    (tt "  ...")
    (blank-line)
    (tt (pieval "parse_and_eval('4', [{}])"))
    (tt (pieval "parse_and_eval('false', [{}])"))))

(slide
  (vl-append
    (tt "def eval(expr, env):")
    (tt "  ...")
    (tt "  elif is_assign_expr():")
    (tt "    val = eval(rhs, env)")
    (tt "    # 0 = current stack frame")
    (tt "    env[0][lhs] = val")
    (tt "    return val")
    (tt "  ...")))

(slide
  (vl-append
    (tt "def eval(expr, env):")
    (tt "  ...")
    (tt "  elif is_identifier(expr):")
    (tt "    for frame in env:")
    (tt "      if expr in frame:")
    (tt "        return frame[expr]")
    (tt "    raise NameError(\"unbound value\")")
    (tt "  ...")
    (blank-line)
    (tt (pieval "parse_and_eval('a', [{'a': 3}])"))
    (tt (pieval "parse_and_eval('b = 2; b', [{}])"))))

(slide
  (vl-append
    (tt "def eval(expr, env):")
    (tt "  ...")
    (tt "  elif is_lambda(expr):")
    (tt "    return Closure(args, body, env)")
    (tt "  ...")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_func_call(expr):")
    (tt "    return apply(fun, args)")))

(slide
  (vl-append
    (tt "def apply(fun, args):")
    (tt "  if is_closure():")
    (tt "    new_env = [dict(zip(fun.argnames, args))]")
    (tt "              + fun.env")
    (tt "    return eval(fun.body, new_env)")
    (blank-line)
    (para (pieval "parse_and_eval('lambda x: x; (4)', [{}])"))))

(slide
  (bt "Not that it does much")
  (bitmap "grumpy.jpg"))

(slide
  (vl-append
    (tt "def apply(function, args):")
    (tt "  ...")
    (tt "  if is_primitive(function):")
    (tt "    return function(*args)")
    (blank-line)
    (para (pieval "parse_and_eval('add(1, 3)', [{'add': lambda x, y: x + y}])"))))

(define prog
  "begin{fac = lambda n:
          if isone(n):
          1
          mul(n, fac(sub1(n)));
   print(fac(10))}")

(define evalstr
  "parse_and_eval(prog, [{'isone': lambda a: a == 1},
                       {'mul': lambda a, b: a * b},
                       {'sub1': lambda a: a - 1},
                       {'print': print}])")

(slide
  (code prog))

(slide
  (vl-append
    (tt "# parses to ...")
    (para (pieval (format "parse('~a')" (string-replace prog "\n" "")) #f))
    (blank-line)

    (tt "# and evals to ... ")
    (para (pieval evalstr #f (format "prog = \"~a\"" (string-replace prog "\n" ""))))))

(slide
  (para #:align 'center (t "It's a real programming language!") (small (t "ish")))
  (bitmap "mindblown.jpg"))

(slide
  (t "Code available in this repo:")
  (tt "https://github.com/charles-l/capstone"))

(slide
  (bt "Further reasources")
  (item "Structure and Interpretation of Computer Programs (The Wizard Book)")
  (item "Types and Programming Languages")
  (item "Paradigms of Artificial Intelligence Programming")
  (item "Compilers: Principles, Tools, and Techniques (The Dragon Book)")
  (item "Racket (programming language)")
  (item "https://github.com/charles-l/capstone and https://github.com/charles-l/comp"))
