import ast

class JSONVisitorException(Exception):
  pass

def visit_list(self, expr_list):
  return map (lambda s : self.visit(s), expr_list)

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
      # NOTE(joe): fd.arguments differs from
      # http://docs.python.org/release/3.0.1/library/ast.html, which
      # claims that the field should be called args
      'arguments' : visit_list(self, fd.arguments),
      'body' : visit_list(self, fd.body),
      'decorator_list' : visit_list(self, fd.decorator_list)
    }


# small tests
node = ast.Module()
node.body = []
assert(JSONVisitor().visit(node) == {'type' : 'Module', 'body' : []})

fd = ast.FunctionDef()
fd.name = 'dothings'
fd.arguments = []
fd.body = []
fd.decorator_list = []
node.body = [fd]

assert(JSONVisitor().visit(node) == {
  'type' : 'Module',
  'body' : [{
    'type' : 'FunctionDef',
    'name' : 'dothings',
    'arguments' : [],
    'body' : [],
    'decorator_list' : []  
  }]
})
