
structure Lex = LEX(structure Symbols = Symbols);

structure Parsetree =
PARSETREE (
    structure Markdown = Markdown
);

structure Parser =
PARSER(
    structure Symbols = Symbols
    and Lex = Lex
    and Parsetree = Parsetree
);

structure PolyDoc =
POLYDOC(
    structure Lex = Lex
    and Parsetree = Parsetree
    and Parser = Parser
    and Markdown = Markdown
);
