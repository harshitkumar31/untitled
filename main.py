from lexer import Lexer
from parser_impl import Parser

text_input = """
print(4 + 4 - 2);
"""

lexer = Lexer().get_lexer()
tokens = lexer.lex(text_input)

pg = Parser()
parser = pg.get_parser()
parsedCont = parser.parse(tokens)
execute = parsedCont.eval()
