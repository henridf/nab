#!/bin/sh


# 
# takes as parameter the html (e.g. lesson1.html) file from which to extract toplevel
# ocaml commands and looks for it in ./doc (so you should run this from ./nab).
#
lessons="1 "

error=0

module=nab
tmpdir=/tmp/

srcdir=$tmpdir/$module


make_new_tmp()
{
    tmpfile=`mktemp`
    mv $tmpfile $tmpfile.ml
    tmpfile=$tmpfile.ml
}

check_exit()
{
  if [ $? -ne 0 ] ; then
      echo "\n\neeeeeeek!!! something appears to have failed"
      error=1
  fi
}

usage()
{
    echo "smoke-tutorial.sh [tagname]"
    exit 1
}

do_script()
{
    rm -f bin/nabviz
    make OPT=y SCRIPT=doc/lesson$1.ml bin/nabviz
    bin/nabviz -script; check_exit
}

convert_ocamlinit()
{
# this was done when nab is being checked out into /tmp
#    s="s/home\/henridf\/work\/nab/tmp\/nab/g"
#    sed -i -e $s doc/ocamlinit
a=1
}

extract_script_html()
{

    echo "Extracting script for lesson $1"

    # add this line because it is automatically done when the user launches 
    # in interactive mode.
    echo "#use \"/home/henridf/work/nab/doc/ocamlinit\";;" > $tmpfile
    

    # ;; -> should always be ocaml
    # #.*; *$ -> ; at end of line, but then check for # also, cos frequently this is just text punctuation
    egrep ";;|#.*; *$|test_keep" doc/lesson$1.html | grep -v test_ignore >> $tmpfile
    sed -i -e 's/# //g' $tmpfile

    # use the ocamlinit that comes in the distro, to make sure it stays up to date.
#    sed -i -e 's/use "\/home\/henridf\/.ocamlinit"/use "\/home\/henridf\/work\/nab\/doc\/ocamlinit"/g' $tmpfile

    # remove any <!--test_keep--> comments
    sed -i -e 's/<!--test_keep-->//g' $tmpfile
}

do_extracted_script()
{
    rm -f bin/nabviz-top
    make bin/nabviz-top
    bin/nabviz-top $tmpfile; check_exit
}


do_lesson()
{
    echo " Doing lesson $1"
    make_new_tmp;
    convert_ocamlinit;
    extract_script_html $1;
    do_extracted_script;
}



if [ $# -eq 0 ] 
    then 
     for num in $lessons 
       do
       do_script $num
       do_lesson $num
     done
     do_script 2
elif [ $# -eq 1 ]
    then
    do_script $1
    do_lesson $1
else
    usage 
fi

if [ $error -eq 0 ] 
    then
    echo " *"
    echo " * Looks ok!"
    echo " *"
else
    echo " X"
    echo " X Looks bad!!!!!!!!!!!!"
    echo " X"
fi

exit $error

