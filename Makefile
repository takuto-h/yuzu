
SOURCES =\
  pos.ml source.ml token.ml lexer.ml\
  literal.ml names.ml pattern.ml type.ml expr.ml top.ml parser.ml\
  trans.ml main.ml

EXEC = ./myml

.PHONY: all
all: $(EXEC)

$(EXEC): $(SOURCES)
	ocamlc -g -o $(EXEC) $(SOURCES)

.SUFFIXES: .yz .ml
.yz.ml:
	$(EXEC) $< $@

.PHONY: clean
clean:
	rm -f $(EXEC) *.cmi *.cmo *.annot *.out

.PHONY: wc
wc:
	wc -l $(SOURCES)
