#! /bin/sh
# -*- perl -*-
exec perl -w -x $0 ${1+"$@"}
#!perl -w
#line 6

#
# fsm@robots.ox.ac.uk
#
# Usage: zap.pl blah
#
# Result: Generates (on stdout) vcl_blah.h from vcl_blah.hhh
#

$blah = $ARGV[0];
die unless defined($blah);
$blah =~ s/^vcl_(.*)/$1/;
$blah =~ s/^(.*)\.hhh$/$1/;
$blah =~ s/^(.*)\.h$/$1/;

die unless -f "vcl_$blah.hhh";
die unless open(FD, "<vcl_$blah.hhh");
$guard = "vcl_generic_${blah}_h_";
$prefix = "vcl_generic_${blah}_STD";
print "#ifndef ${guard}\n";
print "#define ${guard}\n";
print "\n";
print "// THIS IS A GENERATED FILE. DO NOT EDIT! -- Edit the source .hhh and run make instead\n";
print "\n";
#print "#ifndef ${prefix}\n";
#print "  ** \"error -- you must #define ${prefix} before #including this file\" **\n";
#print "#endif\n";
#print "\n";
while (<FD>) {
  if (m/^\@([a-zA-Z_0-9\:]+)\s*$/) {
    $orig = $1; $orig =~ s/\s//g;
    $name = $orig; $name =~ s/\:\:/_/g;
    print "// $orig\n";
    print "#ifndef vcl_$name\n";
    print "#define vcl_$name ${prefix} :: $orig\n";
    print "#endif\n";
  } else {
    print;
  }
}
print "\n";
print "#endif // ${guard}\n";
