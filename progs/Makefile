PROGS = dbplotmulti dbplot dbrowaccumall dbcoladd

dbplotmulti: dbfile.ml dbplotmulti.ml
	ocamlc -I ../misc -I ../contrib/gnuplot unix.cma   str.cma ../misc/misc.ml ../misc/mods.ml   \
	../contrib/gnuplot/gnuplot.cma  dbfile.ml  dbplotmulti.ml -o dbplotmulti

dbplot: dbfile.ml dbplot.ml
	ocamlc -I ../misc -I ../contrib/gnuplot unix.cma   str.cma ../misc/misc.ml ../misc/mods.ml   \
	../contrib/gnuplot/gnuplot.cma  dbfile.ml  dbplot.ml -o dbplot

dbrowaccumall: dbfile.ml dbrowaccumall.ml 
	ocamlc -I ../misc str.cma ../misc/misc.ml ../misc/mods.ml dbfile.ml  dbrowaccumall.ml  -o dbrowaccumall

dbcoladd: dbfile.ml dbcoladd.ml
	ocamlc -I ../misc str.cma ../misc/misc.ml ../misc/mods.ml dbfile.ml  dbcoladd.ml  -o dbcoladd

clean:
	rm -f $(PROGS)

all: dbplotmulti dbplot dbrowaccumall dbcoladd
install: $(PROGS)
	cp $(PROGS) ../../../bin