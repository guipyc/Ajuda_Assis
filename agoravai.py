
import lexer


def main():

    # read the current flow source code in test.lang and store it in variable

    content = ""
    with open('test.lang', 'r') as file:
        content = file.read()

    #
    # Lexer
    #

    # We call the lexer class and initialise it with the source code
    lex = lexer.Lexer(content)
    # We now call the tokenize method
    tokens = lex.tokenize()

    #
    # Parser
    #
    import parser1

    parsing_tokens= tokens
    parse = parser1.Parser1(parsing_tokens)
    obj = parse.parse()

    #
    # Check DataTypeValidation
    #

    import dataTypeValidation
    dt_val_tokens = tokens
    dt_validation = dataTypeValidation.DataTypeValidation(dt_val_tokens)
    dt_obj = dt_validation.dt_validation()


main()
