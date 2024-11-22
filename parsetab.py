
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'ARITHMETIC_OPERATOR ASSIGNMENT_OPERATOR BOOLEAN_VALUE BRACE CHAR_VALUE COMMENT DATATYPE FLOAT_VALUE IDENTIFIER INTEGER_VALUE PAREN PRINT_KEYWORD SEPARATOR STRING_VALUEprogram : declarationsdeclarations : declarations declaration\n                    | emptydeclaration : variable_declaration\n                   | function_declaration\n                   | commentvariable_declaration : DATATYPE IDENTIFIER ASSIGNMENT_OPERATOR expression SEPARATOR\n                            | DATATYPE IDENTIFIER SEPARATORfunction_declaration : DATATYPE IDENTIFIER PAREN PAREN blockblock : BRACE declarations BRACEexpression : INTEGER_VALUE\n                  | FLOAT_VALUE\n                  | CHAR_VALUE\n                  | STRING_VALUE\n                  | BOOLEAN_VALUE\n                  | IDENTIFIER\n                  | expression ARITHMETIC_OPERATOR expressioncomment : COMMENTempty :'
    
_lr_action_items = {'DATATYPE':([0,2,3,4,5,6,7,9,12,22,24,25,27,28,],[-19,8,-3,-2,-4,-5,-6,-18,-8,-7,-9,-19,8,-10,]),'COMMENT':([0,2,3,4,5,6,7,9,12,22,24,25,27,28,],[-19,9,-3,-2,-4,-5,-6,-18,-8,-7,-9,-19,9,-10,]),'$end':([0,1,2,3,4,5,6,7,9,12,22,24,28,],[-19,0,-1,-3,-2,-4,-5,-6,-18,-8,-7,-9,-10,]),'BRACE':([3,4,5,6,7,9,12,21,22,24,25,27,28,],[-3,-2,-4,-5,-6,-18,-8,25,-7,-9,-19,28,-10,]),'IDENTIFIER':([8,11,23,],[10,14,14,]),'ASSIGNMENT_OPERATOR':([10,],[11,]),'SEPARATOR':([10,14,15,16,17,18,19,20,26,],[12,-16,22,-11,-12,-13,-14,-15,-17,]),'PAREN':([10,13,],[13,21,]),'INTEGER_VALUE':([11,23,],[16,16,]),'FLOAT_VALUE':([11,23,],[17,17,]),'CHAR_VALUE':([11,23,],[18,18,]),'STRING_VALUE':([11,23,],[19,19,]),'BOOLEAN_VALUE':([11,23,],[20,20,]),'ARITHMETIC_OPERATOR':([14,15,16,17,18,19,20,26,],[-16,23,-11,-12,-13,-14,-15,23,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'declarations':([0,25,],[2,27,]),'empty':([0,25,],[3,3,]),'declaration':([2,27,],[4,4,]),'variable_declaration':([2,27,],[5,5,]),'function_declaration':([2,27,],[6,6,]),'comment':([2,27,],[7,7,]),'expression':([11,23,],[15,26,]),'block':([21,],[24,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> declarations','program',1,'p_program','sintatico.py',116),
  ('declarations -> declarations declaration','declarations',2,'p_declarations','sintatico.py',120),
  ('declarations -> empty','declarations',1,'p_declarations','sintatico.py',121),
  ('declaration -> variable_declaration','declaration',1,'p_declaration','sintatico.py',128),
  ('declaration -> function_declaration','declaration',1,'p_declaration','sintatico.py',129),
  ('declaration -> comment','declaration',1,'p_declaration','sintatico.py',130),
  ('variable_declaration -> DATATYPE IDENTIFIER ASSIGNMENT_OPERATOR expression SEPARATOR','variable_declaration',5,'p_variable_declaration','sintatico.py',134),
  ('variable_declaration -> DATATYPE IDENTIFIER SEPARATOR','variable_declaration',3,'p_variable_declaration','sintatico.py',135),
  ('function_declaration -> DATATYPE IDENTIFIER PAREN PAREN block','function_declaration',5,'p_function_declaration','sintatico.py',142),
  ('block -> BRACE declarations BRACE','block',3,'p_block','sintatico.py',146),
  ('expression -> INTEGER_VALUE','expression',1,'p_expression','sintatico.py',150),
  ('expression -> FLOAT_VALUE','expression',1,'p_expression','sintatico.py',151),
  ('expression -> CHAR_VALUE','expression',1,'p_expression','sintatico.py',152),
  ('expression -> STRING_VALUE','expression',1,'p_expression','sintatico.py',153),
  ('expression -> BOOLEAN_VALUE','expression',1,'p_expression','sintatico.py',154),
  ('expression -> IDENTIFIER','expression',1,'p_expression','sintatico.py',155),
  ('expression -> expression ARITHMETIC_OPERATOR expression','expression',3,'p_expression','sintatico.py',156),
  ('comment -> COMMENT','comment',1,'p_comment','sintatico.py',166),
  ('empty -> <empty>','empty',0,'p_empty','sintatico.py',170),
]
