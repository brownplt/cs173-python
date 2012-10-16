import ast
import sys
import json

class JSONVisitorException(Exception):
  pass

def visit_list(self, expr_list):
  return [self.visit(s) for s in expr_list]
def maybe_visit(self, expr):
  if expr is not None: return self.visit(expr)
  return None

class QuickVisitor(ast.NodeVisitor):
  def generic_visit(self, n):
    if (not (isinstance(n, ast.AST))):
      print (n)
      raise JSONVisitorException("Non-ast passed to visit")
    fields = ast.iter_fields(n)

    def get_item(v):
      t = type(v)
      if v is None: return None
      elif t == list: return list(map (lambda elt: get_item(elt), v))
      elif isinstance(v, ast.AST): return self.visit(v)
      elif t in [int, float, str]: return v
      else: raise JSONVisitorException("Unexpected case")

    n_dict = dict([(f,get_item(v)) for (f,v) in fields])
    n_dict['type'] = n.__class__.__name__
    return n_dict

class JSONVisitor(ast.NodeVisitor):
  def visit_Module(self, m):
    return { 'type' : 'Module', 'body' : visit_list(self, m.body) }
  def visit_Interative(self, i):
    return { 'type' : 'Interactive', 'body' : visit_list(self, m.body) }
  def visit_Expression(self, e):
    return { 'type' : 'Expression', 'body' : visit_list(self, m.body) }
  def visit_Suite(self, s):
    return { 'type' : 'Suite', 'body' : visit_list(self, m.body) }


  def visit_FunctionDef(self, fd):
    return {
      'type' : 'FunctionDef',
      'name' : fd.name,
      'arguments' : self.visit(fd.args),
      'body' : visit_list(self, fd.body),
      'decorator_list' : visit_list(self, fd.decorator_list),
      'returns' : maybe_visit(self, fd.returns)
    }
  def visit_ClassDef(self, cd):
    return {
      'type' : 'ClassDef',
      'name' : cd.name,
    }

  def visit_Pass(self, p):
    return {
      'type' : 'Pass'
    }

  def visit_Num(self, num):
    return {
      'type' : 'Num',
      'n' : num.n
    }

  def visit_arguments(self, arguments):

    kwargannotation = arguments.kwargannotation
    if kwargannotation is not None:
      kwargannotation = self.visit(kwargannotation)
    return {
      'type' : 'arguments',
      'args' : visit_list(self, arguments.args),
      'vararg' : arguments.vararg,
      'varargannotation' : arguments.varargannotation,
      'kwonlyargs' : visit_list(self, arguments.kwonlyargs),
      'kwarg' : arguments.kwarg,
      'kwargannotation' : maybe_visit(self, arguments.kwargannotation),
      'defaults' : visit_list(self, arguments.defaults),
      'kw_defaults' : visit_list(self, arguments.kw_defaults)
    }

  def visit_arg(self, arg):
    return {
      'type' : 'arg',
      'arg' : arg.arg,
      'annotation' : maybe_visit(self, arg.annotation)
    }

# small tests
node = ast.Module()
node.body = []
assert(JSONVisitor().visit(node) == {'type' : 'Module', 'body' : []})

t = ast.parse("""
def foo(a, b=5):
  pass
""")

visited = QuickVisitor().visit(t)
expected = {
  'type' : 'Module',
  'body' : [{
    'type' : 'FunctionDef',
    'name' : 'foo',
    'args' : {
      'type' : 'arguments',
      'args' : [{
        'type' : 'arg',
        'arg' : 'a',
        'annotation' : None,
      }, {
        'type' : 'arg',
        'arg' : 'b',
        'annotation' : None
      }],
      'vararg': None,
      'varargannotation': None,
      'kwonlyargs' : [],
      'kw_defaults' : [],
      'kwarg' : None,
      'kwargannotation' : None,
      'defaults' : [{
        'type' : 'Num',
        'n' : 5
      }],
    },
    'body' : [{
      'type' : 'Pass'
    }],
    'decorator_list' : [],
    'returns' : None
  }]
}
assert(visited == expected)

if __name__ == '__main__':
  print(json.dumps(QuickVisitor().visit(ast.parse(sys.stdin.read()))))
