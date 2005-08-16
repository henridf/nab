# don't forget to modify downloads.html!

# TAG format release_nab_0_6pre_2004_03_29

release=nab-0.7 # then -1 for sub-releases, if any
tag=release_nab-0-7-1_2005-08-16
cd wherever the source tree is
cvs tag $tag
rm -rf /tmp/nab
cd /tmp
cvs -d :ext:henridf@lcav.epfl.ch:/home/anoncvs/camlrepo export -r $tag nab
rm nab/release/announce.txt

tar cvf $release.tar nab
gzip $release.tar 
cp $release.tar.gz /home/henridf/archives
scp $release.tar.gz henridf@lcav.epfl.ch:/dir/lcavwww/nab/doc
ssh lcav.epfl.ch
#run work/release/update-nab.sh from there
# don't forget to update pointer on download.html
