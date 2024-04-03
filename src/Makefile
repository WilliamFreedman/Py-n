# Compiler
OCAMLC = ocamlc

# Flags
OCAMLFLAGS = -g -w +A-4-9-41-42-44-45-48

# Source files
SOURCES = main.ml

# Object files
OBJECTS = $(SOURCES:.ml=.cmo)

# Interface files
INTERFACES = $(SOURCES:.ml=.cmi)

# Executable name
EXEC = main

.PHONY: all clean

all: $(EXEC)

$(EXEC): $(OBJECTS)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OBJECTS)

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(EXEC) $(INTERFACES)
