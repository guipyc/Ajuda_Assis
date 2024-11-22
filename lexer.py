import re


class Lexer(object):

    def __init__(self, source_code):
        self.source_code = source_code


    def tokenize(self):

        # Where all the tokens create by lexer will be stored
        tokens = []
        flag = 1


        # Create a wordlist of the source code
        source_code = self.source_code.split()

        # This will keep track of all the word index we have in the source code
        source_index = 0

        # Loop through each word in the source code to generate tokens
        while source_index < len(source_code):

            word = source_code[source_index]

            #
            # CHECK OPERATORS
            #
            if word == "++":
                tokens.append(['INCREMENT_OPERATOR', '++'])
                # break

            elif word == "--":
                tokens.append(['DECREMENT_OPERATOR', '--'])
                # break

            elif word == "+":
                tokens.append(['ADDITION_OPERATOR', '+'])
                # break

            elif word == "-":
                tokens.append(['SUBTRACTION_OPERATOR', '--'])
                # break

            elif word == "*":
                tokens.append(['MULTIPLY_OPERATOR', '*'])
                # break

            elif word == "/":
                tokens.append(['DIVIDE_OPERATOR', '/'])
                # break

            elif word == "=":
                tokens.append(['ASSIGNMENT_OPERATOR', '='])
                # break

            elif word == "-":
                tokens.append(['SUBTRACTION_OPERATOR', '-'])
                # break

            elif word == ",":
                tokens.append(['SEPARATOR_OPERATOR', ','])
                # break


            #
            # CHECK FOR KEYWORDS
            #
            if word.lower() == "for":
                tokens.append(['LOOP_OPERQATOR', word])
            #  break

            elif word.lower() == "while":
                tokens.append(['LOOP_OPERQATOR', word])
            # break

            elif word.lower() == "if":
                tokens.append(['CONDITIONAL_OPERQATOR', word])
                # break

            elif word.lower() == "elseif":
                tokens.append(['CONDITIONAL_OPERQATOR', word])
            # break

            elif word.lower() == "else":
                tokens.append(['CONDITIONAL_OPERQATOR', word])
                # break

            elif word.lower() == "break":
                tokens.append(['BREAK_OPERATOR', word])
                # break

            elif word.lower() == "enum":
                tokens.append(['ENUM_OPERATOR', word])
            # break

            elif word.lower() == "default":
                tokens.append(['DEFAULT_OPERATOR', word])
            # break

            elif word.lower() == "static":
                tokens.append(['STATIC_OPERATOR', word])
            #  break

            #
            # CHECK FOR DATA TYPE
            #
            if word.lower() == "int":
                tokens.append(['DATATYPE_INT', word])
                #  break

            elif word.lower() == "double":
                tokens.append(['DATATYPE_DOUBLE', word])
                # break

            elif word.lower() == "bool":
                tokens.append(['DATATYPE_BOOL', word])
                # break

            elif word.lower() == "string":
                tokens.append(['DATATYPE_STRING', word])
                # break

            elif word.lower() == "char":
                tokens.append(['DATATYPE_CHAR', word])
                # break

            elif word.lower() == "float":
                tokens.append(['DATATYPE_FLOAT', word])
                # break

            elif word.lower() == "enum":
                tokens.append(['DATATYPE_ENUM', word])
                # break

            #
            # GET THE DATA TYPE OF THE VALUE
            #
            if re.search(r'^\d*$', word):
                if word[len(word)-1] == ';':
                    tokens.append(['INTEGER_VALUE', word[0:len(word)-1]])
                else:
                    tokens.append(['INTEGER_VALUE', word])

            elif re.search(r'[0-9]', word) and re.search('.', word):
                if word[len(word)-1] == ";":
                    tokens.append(['FLOAT_VALUE', word[0:len(word)-1]])
                else:
                    tokens.append(['FLOAT_VALUE', word])

            elif re.search(r'\w|\d$', word) and word[0] == "'" and (word[len(word)-1] == "'" or word[len(word)-2 == "'"]):
                if word[len(word)-1] == ";":
                    tokens.append(['CHAR_VALUE', word[0:len(word)-1]])
                else:
                    tokens.append(['CHAR_VALUE', word])

            elif re.search('true', word.lower()) or re.search('false', word.lower()):
                if word[len(word)-1] == ";":
                    tokens.append(['BOOLEAN_VALUE', word[0:len(word)-1]])
                else:
                    tokens.append(['BOOLEAN_VALUE, word'])

            if re.search(r'\w*|\w*[0-9]$', word)and re.search(r'^"', word) or re.search(r'$"',word):
                    if re.match(r'".*"', word):
                        tokens.append(['STRING_VALUE', word])


            # if re.search(r'^"',word):
            #
            #     str_arr.append(word)
            #     source_index += 1
            #     while word != '"' :
            #         str_arr.append(word)
            #         source_index += 1
            #     tokens.append(['STRING_VALUE',str_arr])

            #
            # CHECK FOR IDENTIFIER
            #
            if re.search(r'^_\w*|^_A-Za-z|^A-Za-z[0-9]$', word):
                    tokens.append(['IDENTIFIER', word])

            # If a STATEMENT_END(;) is found at the last character create an OPERATOR token for it

            if word[len(word)-1] == ";":
                tokens.append(['STATEMENT_END', ';'])

            # Increases index after each check
            source_index += 1

        print(tokens)

        # Return created tokens
        return tokens
