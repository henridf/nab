# don't forget to modify downloads.html!

# TAG format release_nab_0_6pre_2004_03_29

release=nab-0.6-1
tag=release_nab-0-6-1_2004-04-05
cd wherever the source tree is
cvs tag $tag
rm -rf /tmp/nab
cd /tmp
cvs -d :ext:henridf@lcavsun10:/home/anoncvs/camlrepo export -r $tag nab
rm nab/release/announce.txt

tar cvf $release.tar nab
gzip $release.tar 
cp $release.tar.gz /home/henridf/archives
scp $release.tar.gz henridf@lcmpc4.epfl.ch:/dir/lcavwww/nab/doc
ssh lcmpc4.epfl.ch
#run work/release/update-nab.sh from there
# don't forget to update pointer on download.html