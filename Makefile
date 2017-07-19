VERSION := 1.0
TARGET := main

all: $(TARGET)

run: $(TARGET)
	./$(TARGET)

$(TARGET): babble.ml
	ocamlfind ocamlopt -linkpkg -thread -package core babble.ml -o $(TARGET)

clean:
	-rm -rf *.cmi *.cmo *.cmx *.o
	-rm -rf $(TARGET)
