
YZ_SRCS =\
  yzList.yz pos.yz source.yz token.yz lexer.yz\
  literal.yz names.yz pattern.yz typeExpr.yz expr.yz typeInfo.yz top.yz parser.yz\
  type.yz scheme.yz module.yz decl.yz inf.yz trans.yz compiler.yz main.yz

ML_SRCS = $(addsuffix .ml, $(basename $(YZ_SRCS)))

EXE = ./yuzuko

.PHONY: all
all: $(EXE)

$(EXE): $(ML_SRCS)
	ocamlc -g -o $(EXE) $(ML_SRCS)

.SUFFIXES: .yz .ml
.yz.ml:
	$(EXE) $(YZ_SRCS)

.PHONY: run
run: $(EXE)
	$(EXE)

.PHONY: clean
clean:
	rm -f $(EXE) *.cmi *.cmo *.annot *.out

.PHONY: wc
wc:
	wc -l $(YZ_SRCS)
