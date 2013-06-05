
OBJS =\
  pos.cmo source.cmo token.cmo lexer.cmo\
  literal.cmo names.cmo pattern.cmo type.cmo expr.cmo top.cmo parser.cmo\
  trans.cmo main.cmo

EXE = ./myml

.PHONY: all
all: $(EXE)

$(EXE): $(OBJS)
	ocamlc -g -o $(EXE) $(OBJS)

.SUFFIXES: .yz .ml .cmi .cmo
.yz.ml:
	$(EXE) $< $@
.ml.cmi:
	ocamlc -g -c $<
.ml.cmo:
	ocamlc -g -c $<

.PHONY: clean
clean:
	rm -f $(EXE) *.cmi *.cmo *.annot *.out

.PHONY: wc
wc:
	find . \( -name "*.ml" -o -name "*.yz" \) | xargs wc -l
