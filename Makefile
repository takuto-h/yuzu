
SOURCES =\
  token.ml lexer.ml ident.ml type.ml literal.ml expr.ml parser.ml scheme.ml inferrer.ml

EXEC = myml

.PHONY: all
all: $(EXEC)

$(EXEC): $(SOURCES)
	ocamlmktop -g -o $(EXEC) $(SOURCES)

.PHONY: clean
clean:
	rm -f $(EXEC) *.cmi *.cmo *.annot

.PHONY: wc
wc:
	wc -l $(SOURCES)
