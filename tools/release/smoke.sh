#!/bin/sh

# careful these paths are replicated in smoke-tutorial.sh!!!
module=nab
tmpdir=/tmp/
srcdir=$tmpdir/$module
rm -rf $srcdir/*

error=0

check_exit()
{
  if [ $? -ne 0 ] ; then
      echo "\n\neeeeeeek!!! something appears to have failed"
      error=1
  fi
}



usage()
{
    echo "smoke.sh [-tag tagname]"
    exit 1
}


do_checkout()
{
    echo "Checking out copy of NAB .."
    pushd $tmpdir
    cvs -d :ext:henridf@lcavsun10.epfl.ch:/home/anoncvs/camlrepo co $module
    popd
}

do_export()
{
    pushd $tmpdir
    cvs -d :ext:henridf@lcavsun10.epfl.ch:/home/anoncvs/camlrepo export -r $1 $module
    tar cvzf $1.tar.gz $module
    cp $1.tar.gz /tmp
    echo $module tag $1 has been exported into /tmp/$1.tar.gz
}


do_builds() 
{
    pushd $srcdir

# htmldoc makes errors, so don't check exit status
    make htmldoc

    make nab-top; check_exit
    make nabviz-top; check_exit
    make alltargets; check_exit
    make OPT=y allopttargets; check_exit
    popd
}

do_tutorial()
{
    pushd $srcdir

    /home/henridf/work/release/smoke-tutorial.sh
    check_exit
}

if [ $# -eq 0 ] 
    then 

    do_checkout
    do_builds
    do_tutorial
    if [ $error -eq 0 ] 
	then
	echo " *"
	echo " * Looks ok!"
	echo " *"
    fi

elif [ $1 == "-tag" ]
    then
    if [ $# -ne 2 ] ; then 
	usage 
    fi
    do_export $2
fi

exit $error

