#!/bin/sh

#script_files="test_3_nodes.ml"
script_files="test_3_nodes.ml test_6_nodes.ml test_7_nodes_rerr.ml"
dir="proto/aodv/scripts/"

make_bail()
{
    echo ""
    echo "Eeek. A build error occurred!!"
    echo ""
    exit 2
}

script_bail() 
{
    echo "\nEeek. An error occurred in a test script!!!"
    exit 2
}


for script in $script_files 
do 
  make SCRIPT=$dir/$script bin/nab || make_bail
  bin/nab || script_bail
done