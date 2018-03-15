#lang slideshow
(require slideshow/text)
(require unstable/gui/slideshow)
(require (only-in scribble/base url))
(require (except-in plot/pict shade))

(current-main-font "Roboto Sans")

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
  (bt "Parsing"))

(slide
  (para "The process of semantically converting syntax to a structure using a formal grammar"))

(slide
  (item (tt "lex") "+" (tt "yacc"))
  (item "Parser combinator"))

(slide
  (para #:align 'center (tt "lex") (t "+") (tt "yacc"))
  (item "Standard for modern implementations")
  (item "Old approach"))

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
  (t "lexing is a minimal preprocessing pass"))

(slide
  (para #:align 'center "Parsing (with" (tt "yacc") ")"))

(slide
  (para (tt "yacc") "converts stream of tokens to" (italic (t "parse tree"))))

(slide
  (para "for instance, the stream before would become"))

(slide
  (graphviz "digraph G {
            size = \"8,8\";
            ordering=out;
            node [shape = box];
            a [label=\"VARIABLE DECLARATION (int)\"];
            b [label=\"NAME: x\"];
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
      (tt "parse(num, \"13\") => 3")))
  'next
  (para (tt "parse(num, \"31\") => error ...")))

(slide
  (para (tt "parse(many(char), \"hax0r\") => \"hax\""))
  'next
  (para
    (tt "parse(many(or(char, num)), \"h0w t0 b a l33t hax0r?\")")
    (tt " => \"h0w\"")))

(slide
  (bitmap "runaway.jpg"))

(slide
  (para "Compilers are tricky"))

(slide
  (para "Pipeline of passes"))

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
            b [label=\"partial evaluation\"];
            c [label=\"deforestation\"];
            d [label=\"data-flow analysis\"];
            e [label=\"dead-code elimination\"];
            f [label=\"code generation\"];
            a -> b -> c -> d -> e -> f;
            }"))

(slide
  (tt "Let's build an interpreter!"))

(slide
  (para "In truth, there aren't many pure interpreters anymore"))

(slide
  (para "Most modern \"interpreters\" compile to bytecode (interpreted by a VM)"))

(slide
  (item "Java")
  (item "Python")
  (item "Ruby")
  (item "JavaScript"))

(slide
  (para "Graph walking interpreters are straightforward"))

(slide
  (para "An interpreter walks the tree and executes the semantic meaning dynamically"))

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
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  if is_literal(expr):")
    (tt "    return expr")
    (tt "  ...")
    (blank-line)
    (tt "eval(4, [{}]) => 4")
    (tt "eval(\"it's lit\", [{}]) => \"it's lit\"")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_assignment(expr):")
    (tt "    v = eval(expr.rhs, environment)")
    (tt "    environment[0][expr.lhs] = v")
    (tt "    return v")
    (tt "  ...")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_variable(expr):")
    (tt "    for frame in environment:")
    (tt "      if expr in frame:")
    (tt "        return frame[expr]")
    (tt "    raise NameError(\"unbound value\")")
    (tt "  ...")
    (blank-line)
    (tt "eval(a, [{'a': 3}]) => 3")
    (tt "eval([Assign(b, 2), b], [{}]) => 2")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_variable(expr):")
    (tt "    for frame in environment:")
    (tt "      if expr in frame:")
    (tt "        return frame[expr]")
    (tt "    raise NameError(\"unbound value\")")
    (tt "  ...")
    (blank-line)
    (tt "eval(a, [{'a': 3}]) => 3")
    (tt "eval([Assign(b, 2), b], [{}]) => 2")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_lambda(expr):")
    (tt "    return Closure(expr.args, expr.body, env)")
    (tt "  ...")))

(slide
  (vl-append
    (tt "def eval(expr, environment):")
    (tt "  ...")
    (tt "  elif is_func_call(expr):")
    (tt "    return apply(expr.function, expr.args)")))

(slide
  (vl-append
    (tt "def apply(function, args):")
    (tt "  if is_func_call(expr):")
    (tt "    new_env = [dict(zip(function.argnames, args))]")
    (tt "              + f.env")
    (tt "    last = None")
    (tt "    for expr in function.body:")
    (tt "      last = eval(expr, new_env)")
    (tt "    return last")
    (blank-line)
    (tt "# equal to the python code `(lambda x: x)(3)`")
    (tt "eval(Call(Lambda([Id('x')], Id('x')), 3), [{}])")
    (tt " => 3")))

(slide
  (bt "Not that we can really do anything with it")
  (bitmap "grumpy.jpg"))

(slide
  (vl-append
    (tt "def apply(function, args):")
    (tt "  ...")
    (tt "  if is_primitive(function):")
    (tt "    return function(*args)")
    (blank-line)
    (tt "eval(Call(Id('print'), 14), [{'print': print}])")
    (tt " stdout => 14")))

(slide
  (vl-append
    (tt "# supports recursion")
    (tt "fib = lambda n:")
    (tt "        if is_one(n): 1")
    (tt "        else: n * fib(sub1(n))")
    (tt "print(fib 10)")))

(slide
  (vl-append
    (tt "# parses to ...")
    (tt "prog = Begin(")
    (tt "  Assign(Id('fac'), Lambda([Id('n')],")
    (tt "    If(Call(Id('is_one'), Id('n')),")
    (tt "      1,")
    (tt "      Call(Id('*'), Id('n'), Call(Id('fac'),")
    (tt "        Call(Id('sub1'), Id('n'))))))),")
    (tt "  Call(Id('print'), Call(Id('fac'), 10)))")
    (blank-line)
    (tt "eval(prog, [{'is_one', lambda a: a == 1},")
    (tt "            {'*', lambda a, b: a * b},")
    (tt "            {'sub1', lambda a: a - 1},")
    (tt "            {'print', print}])")
    (tt "# and evals to ... ")
    (tt " => 362880")))

(slide
  (para #:align 'center (t "It's a real programming language!") (small (t "ish")))
  (bitmap "mindblown.jpg"))

(slide
  (t "Code available in this repo:")
  (tt "https://github.com/charles-l/comp"))

(slide
  (bt "Further reasources")
  (item "Structure and Interpretation of Computer Programs (The Wizard Book)")
  (item "Types and Programming Languages")
  (item "Paradigms of Artificial Intelligence Programming")
  (item "Compilers: Principles, Tools, and Techniques (The Dragon Book)")
  (item "Racket (programming language)")
  (item "https://github.com/charles-l/capstone"))
