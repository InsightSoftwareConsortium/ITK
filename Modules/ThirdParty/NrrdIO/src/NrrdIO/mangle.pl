#
#  NrrdIO: stand-alone code for basic nrrd functionality
#  Copyright (C) 2013, 2012, 2011, 2010, 2009  University of Chicago
#  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
#  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah
#
#  This software is provided 'as-is', without any express or implied
#  warranty.  In no event will the authors be held liable for any
#  damages arising from the use of this software.
#
#  Permission is granted to anyone to use this software for any
#  purpose, including commercial applications, and to alter it and
#  redistribute it freely, subject to the following restrictions:
#
#  1. The origin of this software must not be misrepresented; you must
#     not claim that you wrote the original software. If you use this
#     software in a product, an acknowledgment in the product
#     documentation would be appreciated but is not required.
#
#  2. Altered source versions must be plainly marked as such, and must
#     not be misrepresented as being the original software.
#
#  3. This notice may not be removed or altered from any source distribution.
#
#
# generates (to stdout) a header file intended to be included into
# source files where there is a concern of name-space collision induced
# by linking to two different version of NrrdIO
#

if (0 != $#ARGV) {
    die "usage: perl mangle.pl <prefix>\n";
}
$prefix = $ARGV[0];

# there's probably a proper way to detect if the compiler is putting
# an underscore in front of all the symbols, but this works to detect
# what happens on macs
if (exists $ENV{OSTYPE} and "darwin" eq $ENV{OSTYPE}) {
    $mac = 1;
} else {
    $mac = 0;
}

print "#ifndef __${prefix}_NrrdIO_mangle_h\n";
print "#define __${prefix}_NrrdIO_mangle_h\n";
print "\n";
print "/*\n";
print "This header file mangles all symbols exported from the\n";
print "NrrdIO library. It is included in all files while building\n";
print "the NrrdIO library.  Due to namespace pollution, no NrrdIO\n";
print "headers should be included in .h files in ITK.\n";
print "\n";
print "This file was created via the mangle.pl script in the\n";
print "NrrdIO distribution:\n";
print "\n";
print "  perl mangle.pl ${prefix} > ${prefix}_NrrdIO_mangle.h\n";
print "\n";
print "This uses nm to list all text (T), data (D) symbols, as well\n";
print "read-only (R) things (seen on Linux) and \"other\" (S) things\n";
print "(seen on Mac).  On Macs, the preceeding underscore is removed.\n";
print "\n";
print "Also ensures that a few others starting with nrrd are included, and\n";
print "prevents variables ending with .N* where N is some number, from inclusion.\n";
print "*/\n";
print "\n";
open(NM, "nm libNrrdIO.a |");
while (<NM>) {
    if (m/.* .* .*\.[0-9]/) {
        next;
    }
    if (m/.*\.eh$/) {
        next;
    }
    if (m/ [TBDRS] /) {
        s|.* [TBDRS] (.*)|$1|g;
        if ($mac) {
            s|^_||g;
        }
        chop;
        $sym = $_;
        print "#define ${sym} ${prefix}_${sym}\n";
    }
}
close(NM);
print "#endif  /* __${prefix}_NrrdIO_mangle_h */ \n";
