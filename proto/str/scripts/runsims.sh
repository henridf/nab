#!/bin/sh
# NAB - Network in a Box
# Henri Dubois-Ferriere, LCA/LCAV, EPFL
#  
# Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
# Laboratory for Computer Communications and Applications (LCA), 
# Ecole Polytechnique Federale de Lausanne (EPFL),
# CH-1015 Lausanne, Switzerland
#
# This file is part of NAB. NAB is free software; you can redistribute it 
# and/or modify it under the terms of the GNU General Public License as 
# published by the Free Software Foundation; either version 2 of the License,
# or (at your option) any later version. 
# 
# NAB is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details (enclosed in the file GPL). 


# to do warmups create nab executable as
# make OPT=y SCRIPT=warmup.ml nab

# to do routes create nab executable as
# make OPT=y SCRIPT=proto/str/scripts/strtest.ml nab



# 1000 2000 4000
protos="str-age"
nodes="500"
mobs="rw billiard borderwp"
movers="all dests"
num_runs=4
action=$1


if [ $# != 1 ] ; then
    echo "usage $0 [warmup | route | process] "
    exit 1
fi

if [ $action != "warmup" -a $action != "route" -a $action != "process" ] ; then
    echo "Must specify [warmup | route | process]  !"
    exit
fi

pushd /home/henridf/work/nab/


if [ $action == "process" ]; then
    jdboutfile=all-`date +%F-%Hh%Mm`.jdb
    if [ -f $jdboutfile ]; then
	echo "$jdboutfile already exists"
	exit -1
    fi
fi


for node in $nodes
  do 
  for mob in $mobs
    do
    for mover in $movers
      do
      for proto in $protos
	do
	
	if [ $action == "process" ]; then
	    basename=$node"n-$mob-$mover"
	    cat $basename-*.jdb | dbrowaccumall | dbcolcreate nodes $node mob $mob movers $mover proto $proto | dbstripcomments -h | dbcolneaten >> $jdboutfile
	else
	    run=1
	    while [ $run -le $num_runs ] ; do
		basename=$node"n-$mob-$mover-$run"
		statefile=$basename".dat"
		resultfile=$basename".res"
		if [ $action == "warmup" ]; then
		    cmd="bin/nab -nodes $node -agent $proto -warmup mob -mob $mob -run $run -move $mover -dumpfile $statefile"

		elif [ $action == "route" ]; then

		    pkts_orig=$node
		    cmd="bin/nab -pkts_orig $pkts_orig $statefile  -dumpfile $resultfile $statefile"

		fi
		
		echo ""; echo " [ $cmd ]";  echo ""
		$cmd
		run=$(($run+1))
		echo ""; echo "--------------------------------------------------------------------"; echo ""
	    done
	fi
      done
    done
    
  done
  
done


popd