PROGS = dbplotmulti dbplot dbrowaccumall dbcoladd

dbplotmulti:
	ocamlc -I ../misc -I ../contrib/gnuplot unix.cma   str.cma ../misc/misc.ml ../misc/mods.ml   \
	../contrib/gnuplot/gnuplot.cma  dbfile.ml  dbplotmulti.ml -o dbplotmulti

dbplot:
	ocamlc -I ../misc -I ../contrib/gnuplot unix.cma   str.cma ../misc/misc.ml ../misc/mods.ml   \
	../contrib/gnuplot/gnuplot.cma  dbfile.ml  dbplot.ml -o dbplot

dbrowaccumall:
	ocamlc -I ../misc str.cma ../misc/misc.ml ../misc/mods.ml dbfile.ml  dbrowaccumall.ml  -o dbrowaccumall

dbcoladd:
	ocamlc -I ../misc str.cma ../misc/misc.ml ../misc/mods.ml dbfile.ml  dbcoladd.ml  -o dbcolladd

clean:
	rm -f $(PROGS)

all: dbplotmulti dbplot dbrowaccumall dbcoladd