# This program

PROG = pokabot

CAMLC = ocamlc
CAMLFLAGS = -g

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

# Source and Object files
SOURCES = \
	Switches.ml \
	Probs.ml \
	PokerEval.ml \
	Bruteforce.ml \
	Pokabayes.ml Players.ml \
	Controlla.ml \

OBJECTS = $(SOURCES:.ml=.cmo)

# Final Program

$(PROG): $(OBJECTS)
	$(CAMLC) $(CAMLFLAGS) $(OBJECTS) -o $(PROG)


all: $(PROG)

pokereval: pokereval.ml
	ocamlc -g -o pokereval pokereval.ml

clean:
	rm -rf *.cmo *.cmi $(PROG)

cleanup: 
	rm -rf *.cmo *.cmi

.DEFAULT_GOAL := $(PROG)
.PHONY: build run clean
