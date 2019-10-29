NAME = ft_cipher

SOURCES = ./cipher.ml \
		  ./uncipher.ml \
		  ./tests.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

# all: depend $(NAME)

$(NAME) : opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo . cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *~ .*~
	rm -f *.o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).byt
	rm -f $(NAME).opt

# depend: .depend
#     $(CAMLDEP) $(SOURCES) > .depend

re: fclean all

# include .depend
