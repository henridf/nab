set xlabel "Distance to destination"
set ylabel "Cost (of node's best entry to destination)"
plot "/tmp/gradient_grepviz.dat", "/tmp/route_grepviz.dat" with lines, "/tmp/floods_grepviz.dat" with yerrorbars
