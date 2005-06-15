#!/usr/bin/perl

#
# this script comes from cvs.cens.ucla.edu
# written by jeremy elson
#



# This script examines all the CVS entries in a subdirectory tree and
# prints, to standard output, their CVS tag.  If they are not tagged,
# "None" is printed.  If the sources don't all share the same tag,
# "Mixed_Sources" is printed.

use strict;

my $dir;
my $tag = undef;

# First command-line paramter is the directory we should search
$dir = $ARGV[0] or $dir = ".";

my @filelist = split(/\n/, `find $dir -name Entries -print`);

foreach my $file (@filelist) {
    open(FILE, $file) or die "can't open $file: $!\n";
    while (my $line = <FILE>) {
	chomp $line;

	my ($dir, $fname, $rev, $date, $kflags, $rawtag) = split(/\//, $line);
	my $this_tag;

	# Skip directories
	next if ($dir eq "D");

	# First see if this source file is tagged
	if ($rawtag =~ /^T(.*)/) {
	    $this_tag = "cvstag_" . $1;
	} else {
	    $this_tag = "None";
	}

	# for debugging:
	# print "$fname: $this_tag (from $file, line $line)\n";

	# Now see if the tag matches other tags from this repo
	if (!defined $tag) {
	    $tag = $this_tag;
	} else {
	    $tag = "Mixed_Sources" if ($tag ne $this_tag);
	}
    }
    close FILE;
}

#print STDERR "CVS tag in  $dir: $tag\n";
print "$tag";
