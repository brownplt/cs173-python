import ast
import sys
import json

class JSONVisitorException(Exception):
  pass

class QuickVisitor(ast.NodeVisitor):
  def generic_visit(self, n):
    if (not (isinstance(n, ast.AST))):
      raise JSONVisitorException("Unexpected error: Non-ast passed to visit. Please report.")
    fields = ast.iter_fields(n)

    def get_item(v):
      t = type(v)
      if v is None: return None
      elif t == list: return list(map (lambda elt: get_item(elt), v))
      elif isinstance(v, ast.AST): return self.visit(v)
      elif t in [int, float, str]: return v
      else: raise JSONVisitorException("Unexpected error: Missed case.  Please report.")

    n_dict = dict([(f,get_item(v)) for (f,v) in fields])
    n_dict['nodetype'] = n.__class__.__name__
    return n_dict

if __name__ == '__main__':
  print(json.dumps(QuickVisitor().visit(ast.parse(sys.stdin.read()))))
