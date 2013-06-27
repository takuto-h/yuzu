
YZ_SRCS =\
  yzOption.yz yzList.yz pos.yz source.yz token.yz lexer.yz\
  literal.yz names.yz pattern.yz typeExpr.yz declExpr.yz expr.yz typeInfo.yz top.yz\
  parser.yz type.yz scheme.yz module.yz decl.yz inf.yz trans.yz compiler.yz main.yz

ML_SRCS = $(addsuffix .ml, $(basename $(YZ_SRCS)))

COMPILER = ./yuzuko
TOP = ./yui

.PHONY: all
all: $(COMPILER)

$(COMPILER): $(ML_SRCS)
	ocamlc -g -o $(COMPILER) $(ML_SRCS)

$(TOP): $(ML_SRCS)
	ocamlmktop -o $(TOP) $(ML_SRCS)

.SUFFIXES: .yz .ml
.yz.ml:
	$(COMPILER) $(YZ_SRCS)

.PHONY: run
run: $(COMPILER)
	$(COMPILER)

.PHONY: debug
debug: $(TOP)
	$(TOP)

.PHONY: clean
clean:
	rm -f $(COMPILER) $(TOP) *.cmi *.cmo *.annot *.out

.PHONY: wc
wc:
	wc -l $(YZ_SRCS)
