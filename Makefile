POLYC=polyc

SOURCE= \
HashTable.ML \
LEX.sig \
LEX.sml \
main.ML \
MARKDOWN.sig \
Markdown.sml \
PARSER.ML \
PARSER.sig \
PARSETREE.sig \
PARSETREE.sml \
POLYDOC.ML \
ROOT.ML \
Symbols.ML \
SYMBOLS.sig

polydoc: $(SOURCE)
	$(POLYC) -o polydoc ROOT.ML

