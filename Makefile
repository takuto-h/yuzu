
SRCS =\
  yzList.ml\
  pos.ml source.ml token.ml lexer.ml\
  literal.ml names.ml pattern.ml typeExpr.ml expr.ml typeInfo.ml top.ml parser.ml\
  type.ml scheme.ml module.ml inf.ml trans.ml compiler.ml main.ml

EXE = ./yuzuko

.PHONY: all
all: $(EXE)

$(EXE): $(SRCS)
	ocamlc -g -o $(EXE) $(SRCS)

.SUFFIXES: .yz .ml
.yz.ml:
	$(EXE) $(addsuffix .yz, $(basename $(SRCS)))

.PHONY: clean
clean:
	rm -f $(EXE) *.cmi *.cmo *.annot *.out

.PHONY: wc
wc:
#	find . \( -name "*.ml" -o -name "*.yz" \) | xargs wc -l
	wc -l $(SRCS)
