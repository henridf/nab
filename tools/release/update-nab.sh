#!/bin/sh
pushd /dir/lcavwww/nab
cvs update
rm .depend
make htmldoc
cp /dir/lcavwww/nab/README /dir/lcavwww/nab/doc
popd
