#!/bin/sh
# -*- perl -*-
exec perl -w -x $0 ${1+"$@"}
#!perl
#line 6
# If Windows barfs at line 3 here, you will need to run perl -x vxl_doxy.pl
# You can set up as a permanent file association using the following commands
#  >assoc .pl-PerlScript
#  >ftype PerlScript=Perl=C:\Perl\bin\Perl.exe -x "%1" %*

# Script to change the perceps documentation format to Doxygen (JavaDoc) format
# Authors:
#         Dave Cooper
#         Maarten Vergauwen
# Date:
#      17/02/2000
# Modified:
# 11 April 2001 Ian Scott.   Remove support for old perceps commands
#  5 May   2001 Geoff Cross. Correctly handle end of verbatim blocks. Allow two contiguous comments
#  10 May  2001 Ian Scott. Merged Geoffs and my changes


# patterns to be matched
$verbpatt = '\\\\verbatim';
$endverbpatt = '\\\\endverbatim';
$slashslashpatt = '^\\s*//';
$slashslashcolonpatt = '^\\s*//:';
$slashstarstarpatt = '/**';
$spacespacepatt = '  ';
$starpatt = '*';
$starslashpatt = '*/';

# variables that keep state:

# comment found -> first line should start with /**, next lines with *, last line with */
$comment = 0;

# verbatim found -> lines should not start with * (visible in Doxygen)
$verbatim = 0;
# finish verbatim mode at the end of this line.
$should_end_verbatim = 0;

$debug = 0;

# mainloop
while (<>)
{
    # preprocessing
    s/\bVCL_SUNPRO_CLASS_SCOPE_HACK\s*\([^()]*\)//g;
    s/\bVCL_SUNPRO_ALLOCATOR_HACK\s*\(([^()]*)\)/$1/g;
    s/\bVCL_CAN_STATIC_CONST_INIT_(INT|FLOAT)\b/1/g;
    s/\bVCL_STATIC_CONST_INIT_(INT|FLOAT)\s*\(([^()]*)\)/= $2/g;
    s/\bVCL_DFL_TYPE_PARAM_STLDECL\s*\(([^,()]*),([^,()]*)\)/class $1 = $2 /g;
    s/\bDECLARE_DYNCREATE\s*\([^()]*\)//g; # for MFC

    if ( $should_end_verbatim )
    {
        $verbatim = 0;
        $should_end_verbatim = 0;
    }

    # found verbatim ?
    if ( m/$verbpatt/ ) { $verbatim = 1; };

    # found endverbatim ?
    if ( m/$endverbpatt/ ) { $should_end_verbatim = 1; };

    # found start of comment: "//:"  ?
    if ( s!$slashslashcolonpatt!$slashstarstarpatt! )
    {
        chomp; s/\s*$//;
        # escape a space following a dot, add a dot at the end,
#       # and finish brief doc, unless the line is empty or only has '\file':
        unless (m!^\s*\/\*\*\s*(\\file)?\s*$!) {
#         s/\. /.\\ /g; s/(\.)?\s*$/. \*\/\n\/\*/;
          s/\. /.\\ /g; s/(\.)?\s*$/.\n/;
        }
        else { s/$/\n/; }

        if ($comment)
        {
            # Previous comment hasn't ended--two contiguous comment blocks.
            # (Should not happen.)
            print STDERR "Two contiguous comment blocks -- this should not happen\n";
            print "*/\n";
        }
        $comment = 1;
        print; next;
    }

    # Replace '$' with '\f$' (TeX math mode)
    s/(\\f)?\$(.+?)(\\f)?\$/\\f\$$2\\f\$/g if ($comment);

    # found continuation of comment WITH verbatim -> no "*"
    if ( m!$slashslashpatt! && $verbatim && $comment)
    {
        s!$slashslashpatt!$spacespacepatt!;
#       # Make 'Modifications' a section title:
#       s!\b(Modifications?)\b\:?!\<H2\>$1\<\/H2\>!;
        # remove lines of the form ========= or +-+-+-+-+ or ********* or longer:
        print unless m/^\s*[*=+-]{9,}\s*$/; next;
    }

    # found continuation of comment WITHOUT verbatim -> start line with "*"
    if ( m!$slashslashpatt! && $comment )
    {
        s!$slashslashpatt!$starpatt!;
        # remove lines of the form ========= or +-+-+-+-+ or ********* or longer:
        print unless m/^\s*[*=+-]{9,}\s*$/; next;
    }

    # found end of comment -> start line with */
    # NOTE that *every* line within a comment (also empty lines) *must* start with // !
    # (In an earlier version of this script, empty lines were allowed inside comments.)
    if ( $comment && ! m!$slashslashpatt!  )
    {
        print "$starslashpatt\n";
        $comment = 0;
        print; next;
    }

    # just print line if not in comment or in file
    if ( !$comment ) { print; next; }

    # debug - print unprocessed lines (s.b. none)
    if ($debug) { print "LNP:\t"; print; }
}
