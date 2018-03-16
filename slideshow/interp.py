from pyparsing import *

expr = Forward()
var = Word(alphas, alphanums)
pbool = Literal('true').setParseAction(lambda: True) | Literal('false').setParseAction(lambda: False)
num = Word(nums).setParseAction(lambda x: int(x[0]))
literal = var | pbool | num
decl_ = var + Literal('=') + expr
decl_.setParseAction(lambda toks: ParseResults(['=', toks[0], toks[2]]))
decl = Group(decl_)
ife = Group(Literal('if') + expr + Suppress(':') + expr + expr)
lm = Group(Literal('lambda') + Group(delimitedList(var)) + Suppress(':') + expr)
begin = Literal('begin') + Suppress('{') + OneOrMore(expr) + Suppress('}')
expr_ = Forward()
args = Suppress('(') + delimitedList(expr) + Suppress(')')
expr << (decl + expr_ | ife + expr_ | lm + expr_ | begin + expr_ | Group(literal + args) | literal)
expr_ << (args | Empty())

class Closure():
    def __init__(self, args, body, env):
        self.args = args
        self.body = body
        self.env = env

def eval(t, env):
    def lookup(v):
        for f in env:
            if v in f:
                return f[v]
        raise NameError(v + " is not bound")

    def assign(i, v):
        env[0][i] = v

    is_literal = lambda x: isinstance(x, (int, bool))
    is_id = lambda x: isinstance(x, str)

    keywords = ('if', '=', 'begin', 'lambda')

    is_if_expr = lambda: t[0] == 'if'
    is_assign_expr = lambda: t[0] == '='
    is_begin_expr = lambda: t[0] == 'begin'
    is_lambda_expr = lambda: t[0] == 'lambda'
    is_call = lambda: isinstance(t, list) and t[0] not in keywords

    if is_literal(t):
        return t
    elif is_id(t):
        return lookup(t)
    elif is_if_expr():
        if eval(t[1], env):
            return eval(t[2], env)
        else:
            return eval(t[3], env)
    elif is_assign_expr():
        if not is_id(t[1]):
            raise Exception("assignment lhs must be an identifier: " + str(t))
        r = eval(t[2], env)
        assign(t[1], r)
        return r
    elif is_begin_expr():
        last = None
        for e in t.asList()[1:]:
            last = eval(e, env)
        return last
    elif is_lambda_expr():
        return Closure(t[1], t[2], env)
    elif is_call():
        # assume its a function call
        return apply(
                eval(t[0], env),
                [eval(a, env) for a in t[1:]])
    else:
        raise Exception("unknown expression" + originalTextFor(t))

def apply(fun, args):
    is_primitive = lambda: callable(fun)
    is_closure = lambda: isinstance(fun, Closure)
    if is_closure():
        new_env = [dict(zip(fun.args, args))] + fun.env
        return eval(fun.body, new_env)
    elif is_primitive():
        return fun(*args)
    else:
        raise TypeError("Not a function " + str(fun))

program = """
begin{
  fac = lambda n:
    if isone(n):
      1
      mul(n, fac(sub1(n)))
  print(fac(10))
}
"""

tree = expr.parseString(program)
print(tree)
eval(tree, [{'isone': lambda x: x == 1,
    'mul': lambda x, y: x * y,
    'sub1': lambda x: x - 1,
    'print': print}])
