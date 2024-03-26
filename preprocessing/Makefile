# OCaml compiler
OCAMLC = ocamlc

# Flags for the compiler
OCAMLCFLAGS = -g -w +A-4-9-41-42-44-45-48

# Source files
SRCS = macros.ml indents.ml

# Executables
BINS = $(patsubst %.ml,%,$(SRCS))

# Default rule
all: $(BINS)

# Rule to build each executable
%: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $<

# Clean rule
clean:
	rm -f *.cmo *.cmi $(BINS)
