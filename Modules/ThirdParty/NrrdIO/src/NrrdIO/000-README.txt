---------------------------------------------------------------------------
License -------------------------------------------------------------------
---------------------------------------------------------------------------

  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah
 
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.
 
  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:
 
  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
 
  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
 
  3. This notice may not be removed or altered from any source distribution.

---------------------------------------------------------------------------
General information -------------------------------------------------------
---------------------------------------------------------------------------

** NOTE: These source files have been copied and/or modified from Teem,
** <http://teem.sf.net>.  Teem is licensed under a weakened GNU Lesser Public
** License (the weakening is to remove burdens on those releasing binaries
** that statically link against Teem) . The non-reciprocal licensing defined
** above applies to only the source files in the NrrdIO distribution, and not
** to Teem.

NrrdIO is a modified and highly abbreviated version of the Teem. NrrdIO
contains only the source files (or portions thereof) required for
creating and destroying nrrds, and for getting them into and out of
files.  The NrrdIO sources are created from the Teem sources by using
GNU Make (pre-GNUmakefile in the NrrdIO distribution).

NrrdIO makes it very easy to add support for the NRRD file format to your
program, which is a good thing considering and design and flexibility of the
NRRD file format, and the existence of the "unu" command-line tool for
operating on nrrds.  Using NrrdIO requires exactly one header file,
"NrrdIO.h", and exactly one library, libNrrdIO.

Currently, the API presented by NrrdIO is a strict subset of the Teem API.
There is no additional encapsulation or abstraction.  This could be annoying
in the sense that you still have to deal with the biff (for error messages)
and the air (for utilities) library function calls.  Or it could be good and
sane in the sense that code which uses NrrdIO can be painlessly "upgraded" to
use more of Teem.  Also, the API documentation for the same functionality in
Teem will apply directly to NrrdIO.

NrrdIO was originally created with the help of Josh Cates in order to add
support for the NRRD file format to the Insight Toolkit (ITK).

---------------------------------------------------------------------------
NrrdIO API crash course ---------------------------------------------------
---------------------------------------------------------------------------

Please read <http://teem.sourceforge.net/nrrd/lib.html>.  The functions that
are explained in detail are all present in NrrdIO.  Be aware, however, that
NrrdIO currently supports ONLY the NRRD file format, and not: PNG, PNM, VTK,
or EPS.

The functionality in Teem's nrrd library which is NOT in NrrdIO is basically
all those non-trivial manipulations of the values in the nrrd, or their
ordering in memory.  Still, NrrdIO can do a fair amount, namely all the
functions listed in these sections of the "Overview of rest of API" in the
above web page:

- Basic "methods"
- Manipulation of per-axis meta-information
- Utility functions
- Comments in nrrd
- Key/value pairs
- Endianness (byte ordering)
- Getting/Setting values (crude!)
- Input from, Output to files

---------------------------------------------------------------------------
Files comprising NrrdIO ---------------------------------------------------
---------------------------------------------------------------------------

NrrdIO.h: The single header file that declares all the functions and variables
that NrrdIO provides.

sampleIO.c: Tiny little command-line program demonstrating the basic NrrdIO
API.  Read this for examples of how NrrdIO is used to read and write NRRD
files.

CMakeLists.txt: to build NrrdIO with CMake

pre-GNUmakefile: how NrrdIO sources are created from the Teem
sources. Requires that TEEM_SRC_ROOT be set, and uses the following two files.

tail.pl, unteem.pl: used to make small modifications to the source files to
convert them from Teem to NrrdIO sources

mangle.pl: used to generate a #include file for name-mangling the external
symbols in the NrrdIO library, to avoid possible problems with programs
that link with both NrrdIO and the rest of Teem.

preamble.c: the preamble describing the non-copyleft licensing of NrrdIO.

qnanhibit.c: discover a variable which, like endianness, is architecture
dependent and which is required for building NrrdIO (as well as Teem), but
unlike endianness, is completely obscure and unheard of.

encodingBzip2.c, formatEPS.c, formatPNG.c, formatPNM.c, formatText.c,
formatVTK.c: These files create stubs for functionality which is fully present
in Teem, but which has been removed from NrrdIO in the interest of simplicity.
The filenames are in fact unfortunately misleading, but they should be
understood as listing the functionality that is MISSING in NrrdIO.

All other files: copied/modified from the air, biff, and nrrd libraries of
Teem.
