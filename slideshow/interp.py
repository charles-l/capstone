from pyparsing import *

pexpr = Forward()
pvar = Word(alphas, alphanums)
ptrue = Keyword('true').setParseAction(lambda: True)
pfalse = Keyword('false').setParseAction(lambda: False)
pbool = ptrue | pfalse
pnum = Word(nums).setParseAction(lambda x: int(x[0]))
pliteral = pbool | pvar | pnum
pdecl_ = pvar + Literal('=') + pexpr
pdecl_.setParseAction(lambda toks: ParseResults(['=', toks[0], toks[2]]))
pdecl = Group(pdecl_)
pif = Group(Keyword('if') + pexpr + Suppress(':') + pexpr + pexpr)
plambda = Group(Keyword('lambda') +
                 Group(delimitedList(pvar)) +
                 Suppress(':') +
                 pexpr)
pbegin = Group(Keyword('begin') + Suppress('{') + OneOrMore(pexpr) + Suppress('}'))
pargs = Suppress('(') + Optional(delimitedList(pexpr)) + Suppress(')')
pexpr_ = pargs | Empty()
pexpr << (pdecl + pexpr_ |
         pif + pexpr_ |
         plambda + pexpr_ |
         pbegin + pexpr_ |
         Group(pliteral + pargs) |
         pliteral) + Optional(Suppress(';'))

class Closure():
    def __init__(self, args, body, env):
        self.args = args
        self.body = body
        self.env = env

def eval(expr, env):
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

    is_if_expr = lambda: expr[0] == 'if'
    is_assign_expr = lambda: expr[0] == '='
    is_begin_expr = lambda: expr[0] == 'begin'
    is_lambda_expr = lambda: expr[0] == 'lambda'
    is_call = lambda: isinstance(expr, list) and expr[0] not in keywords

    if is_literal(expr):
        return expr
    elif is_id(expr):
        return lookup(expr)
    elif is_if_expr():
        if eval(expr[1], env):
            return eval(expr[2], env)
        else:
            return eval(expr[3], env)
    elif is_assign_expr():
        if not is_id(expr[1]):
            raise Exception("assignment lhs must be an identifier: " + str(expr))
        r = eval(expr[2], env)
        assign(expr[1], r)
        return r
    elif is_begin_expr():
        last = None
        for e in expr[1:]:
            last = eval(e, env)
        return last
    elif is_lambda_expr():
        return Closure(expr[1], expr[2], env)
    elif is_call():
        # assume its a function call
        return apply(
                eval(expr[0], env),
                [eval(a, env) for a in expr[1:]])
    else:
        raise Exception("unknown expression: " + str(expr))

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

def parse(str):
    return pexpr.parseString(str).asList()

def parse_and_eval(str, env = [{}]):
    t = parse(str)
    for e in t:
        e = eval(e, env)
    return e
