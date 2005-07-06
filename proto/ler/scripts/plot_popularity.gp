set term x11
#set term postscript color
set output "/tmp/50mostpop.ps"

plot  "/tmp/gradient_fresh.dat", \
"/tmp/gradient_fresh_avg.dat" with lines, \
"/tmp/route_fresh.dat" with linespoints          

#"/home/henridf/work/nab/proto/ler/scripts/50most_pop_distages.dat" with points
#replot "/tmp/route_fresh.dat" with linespoints          
