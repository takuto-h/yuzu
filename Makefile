
SOURCES =\
  token.ml charStream.ml source.ml lexer.ml\
  ident.ml type.ml literal.ml expr.ml top.ml parser.ml\
  scheme.ml inferrer.ml value.ml eva.ml main.ml

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
