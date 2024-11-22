import re
import ply.lex as lex
import ply.yacc as yacc

# expressões regulares para cada tipo de token
token_patterns = [
    ('NUM_INT', r'\b\d+\b'),
    ('NUM_DEC', r'\b\d+\.\d+\b'),
    ('ID', r'\b[a-zA-Z_]\w*\b'),
    ('TEXTO', r'"([^"\\]|\\.)*"'),
    ('PALAVRA_RESERVADA', r'\b(int|float|char|boolean|void|if|else|for|while|scanf|println|main|return)\b'),  # Palavras reservadas
    ('COMENTARIO', r'\/\/.*'),
    ('OPERADOR', r'[+\-*/%]|&&|\|\||!|==|!=|>=|<=|>|<|=|\+\+|\-\-'),
    ('SIMBOLO_ESPECIAL', r'[\(\)\[\]\{\},;]'),
    ('INCREMENTO', r'\+\+'),
    ('DECREMENTO', r'\-\-'),
    ('PONTO', r'\.'),
    ('SETA', r'->'),
    ('PONTO_E_VIRGULA', r';'),
    ('VIRGULA', r','),
    ('ASPAS', r'"'),
    ('ASPAS_SIMPLES', r"'"),
    ('COMENTARIO_MULTILINHA', r'/\*(.|\n)*?\*/'),
    ('OPERADOR_COMPARACAO', r'==|!=|>=|<=|>|<'),
    ('E_LOGICO', r'&&'),
    ('OU_LOGICO', r'\|\|'),  # Operador lógico OR
    ('NEGACAO_LOGICA', r'!'),
]


# Função para analisar o código-fonte e identificar os tokens
def lexical_analyzer(codigo):
    tokens = []
    posicao = 0

    while posicao < len(codigo):
        match_found = False
        for token_nome, token_pattern in token_patterns:
            regex = re.compile(token_pattern)
            match = regex.match(codigo, posicao)
            if match:
                valor = match.group(0)
                if token_nome != 'COMENTARIO':  # Ignorar comentários
                    tokens.append((token_nome, valor))
                posicao = match.end()
                match_found = True
                break
        if not match_found:
            # tentar ignorar o token inválido
            posicao += 1
    return tokens


# tokens
tokens = [
    'IDENTIFIER', 'INTEGER_VALUE', 'FLOAT_VALUE', 'CHAR_VALUE', 'STRING_VALUE',
    'BOOLEAN_VALUE', 'ASSIGNMENT_OPERATOR', 'SEPARATOR', 'PAREN', 'BRACE', 'COMMENT', 'DATATYPE', 'ARITHMETIC_OPERATOR',
    'PRINT_KEYWORD'
]

# regras de expressão regular para tokens simples
t_ASSIGNMENT_OPERATOR = r'='
t_SEPARATOR = r'[;,]'
t_PAREN = r'[()]'
t_BRACE = r'[{}]'
t_ARITHMETIC_OPERATOR = r'[-+*/]'
t_ignore_WHITESPACE = r'\s+'

#expressões regulares -> tokens complexos
def t_COMMENT(t):
    r'//.*|/\*[\s\S]*?\*/'
    pass

def t_BOOLEAN_VALUE(t):
    r'\btrue\b|\bfalse\b'
    return t

def t_FLOAT_VALUE(t):
    r'\b\d+\.\d+\b'
    return t

def t_INTEGER_VALUE(t):
    r'\b\d+\b'
    return t

def t_CHAR_VALUE(t):
    r'\'([^\\\n]|(\\.))*?\''
    return t

def t_STRING_VALUE(t):
    r'\"([^\\\n]|(\\.))*?\"'
    return t

def t_DATATYPE(t):
    r'\bint\b|\bfloat\b|\bdouble\b|\bchar\b|\bboolean\b'
    t.value = {"type": "DATATYPE", "value": t.value}
    return t

def t_PRINT_KEYWORD(t):
    r'\bprint\b'
    return t

def t_IDENTIFIER(t):
    r'\b[A-Za-z_][A-Za-z0-9_]*\b'
    t.value = {"type": "IDENTIFIER", "value": t.value}
    return t

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# Definindo a gramática
def p_program(p):
    '''program : declarations'''
    p[0] = {"type": "Program", "body": p[1]}

def p_declarations(p):
    '''declarations : declarations declaration
                    | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_declaration(p):
    '''declaration : variable_declaration
                   | function_declaration
                   | comment'''
    p[0] = p[1]

def p_variable_declaration(p):
    '''variable_declaration : DATATYPE IDENTIFIER ASSIGNMENT_OPERATOR expression SEPARATOR
                            | DATATYPE IDENTIFIER SEPARATOR'''
    if len(p) == 6:
        p[0] = {"type": "DeclaracaoVariavel", "varType": p[1]["value"], "id": p[2]["value"], "init": p[4]}
    else:
        p[0] = {"type": "DeclaracaoVariavel", "varType": p[1]["value"], "id": p[2]["value"], "init": None}

def p_function_declaration(p):
    '''function_declaration : DATATYPE IDENTIFIER PAREN PAREN block'''
    p[0] = {"type": "DeclaracaoFuncao", "funcType": p[1]["value"], "id": p[2]["value"], "params": [], "body": p[5]}

def p_block(p):
    '''block : BRACE declarations BRACE'''
    p[0] = {"type": "Bloco", "body": p[2]}

def p_expression(p):
    '''expression : INTEGER_VALUE
                  | FLOAT_VALUE
                  | CHAR_VALUE
                  | STRING_VALUE
                  | BOOLEAN_VALUE
                  | IDENTIFIER
                  | expression ARITHMETIC_OPERATOR expression'''
    if len(p) == 4:
        p[0] = {"type": "Expressao", "left": p[1]["value"], "operator": p[2], "right": p[3]["value"]}
    else:
        p[0] = {"type": "Expressao", "value": p[1]}


def p_comment(p):
    '''comment : COMMENT'''
    p[0] = {"type": "Comentario", "value": p[1]}

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error: unexpected end of input")


parser = yacc.yacc()

#imprimir a árvore
def print_tree(node, level=0):
    if isinstance(node, dict):
        if level > 0:
            print("|   " * (level - 1) + "+-- " + node["type"])
        for key, value in node.items():
            if key != "type":
                if isinstance(value, list):
                    for item in value:
                        print_tree(item, level + 1)
                else:
                    print_tree(value, level + 1)
    elif isinstance(node, list):
        for item in node:
            print_tree(item, level)
    else:
        print("|   " * level + "+-- " + str(node))


source_code = """
// Este é um comentário de linha.
int main() {
    int numero1 = 123;
    int numero2 = 456;
    int resultado = numero1 + numero2;

}
"""

# lexer
try:
    tokens = lexical_analyzer(source_code)
    for token in tokens:
        print(token)

    #executa
    ast = parser.parse(source_code)

    print("\nAST:")
    print_tree(ast)
except ValueError as e:
    print("Erro léxico:", e)
