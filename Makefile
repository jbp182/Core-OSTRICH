.PHONY: all compile start clean

all: compile start

compile:
	@echo "Compiling .ml files..."
	ocamlc -c syntax.ml 
	ocamlc -c utils.ml
	ocamlc -c eval.ml
	ocamlc -c prettyPrint.ml
	ocamlc -c typing.ml  

start:
	@echo "Starting utop..."
	utop -init .init.ml

clean:
	@echo "Cleaning up..."
	rm *.cmi
	rm *.cmo