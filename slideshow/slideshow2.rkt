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
  (bold (t "How to build a programming language")))

(slide
  (bt "What is a programming language?")
  (para "Formal languages for communicating with programmers and machines"))

(slide
  (bt "Why should you care?")
  'next
  (item "Reason about language errors")
  'next
  (item "Understand performance")
  'next
  (item "Build your own!"))

(slide
  (bitmap "progcat.jpg"))

(revealing-slide
  (two-columns
    (vc-append 20
      (bt "Interpreter")
      (reveal 1 (item "Dynamically executes code"))
      (reveal 2 (item "Slow"))
      (reveal 3 (item "More debug/runtime info                           ")))
    (vc-append 20
      (bt "Compiler")
      (reveal 1 (item "Generates executable"))
      (reveal 2 (item "Fast"))
      (reveal 3 (item "Less debug/runtime info (unless explicitely added)")))))

(slide
  (bt "We're going to talk about interpreters"))

(slide
  (bt "Let's build one!"))

(slide
  (bt "Parsing")
  (para "Convert the program into a format that is convenient for the interpreter"))

(slide
  (tt "printf(\"my number is %d\", num);")
  'next
  (graphviz
    "digraph G {
    a [label=\"FUNC CALL\"];
    b [label=\"FUNC ID (printf)\"];
    c [label=\"ARGLIST\"];
    d [label=\"STRING (my number is)\"];
    e [label=\"VAR ID (num)\"];
    a -> b;
    a -> c;
    c -> d;
    c-> e;
    }"))

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
  (para (pieval "parse('if true: a; b')"))
  (para (pieval "parse('begin{f(); g()}')"))
  (para (pieval "parse('a = 2')"))
  (para (pieval "parse('a = b = false')"))
  (para (pieval "parse('begin{x = lambda y: add1(y); x(3)}')"))
  )

(slide
  (tt "the wizard book")
  (bitmap "sicp.jpg"))

(slide
  (bitmap "eval-apply.gif"))

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
    (tt "    fun = expr[0]")
    (tt "    args = expr[1]")
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

    (tt "# with the python functions")
    (code "[{'isone': lambda a: a == 1},
 {'mul': lambda a, b: a * b},
 {'sub1': lambda a: a - 1},
 {'print': print}]")

    (tt "# and evals to ... ")
    (para (pieval evalstr #f (format "prog = \"~a\"" (string-replace prog "\n" ""))))))

(slide
  (para #:align 'center (t "We built a real programming language!") (small (t "ish")))
  (bitmap "mindblown.jpg"))

(slide
  (t "Code available in this repo:")
  (tt "https://github.com/charles-l/capstone"))

(slide
  (tt "questions?"))