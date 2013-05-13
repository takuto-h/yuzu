
SOURCES =\
  token.ml pos.ml source.ml lexer.ml\
  ident.ml type.ml literal.ml expr.ml top.ml scheme.ml decl.ml parser.ml\
  inferrer.ml value.ml eva.ml loader.ml

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
