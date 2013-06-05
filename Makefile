
SOURCES =\
  pos.ml source.ml token.ml lexer.ml\
  literal.ml names.ml pattern.ml type.ml expr.ml top.ml parser.ml\
  trans.ml

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
