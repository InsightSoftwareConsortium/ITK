#
#  NrrdIO: stand-alone code for basic nrrd functionality
#  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
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
# This helps in converting teem source files into NrrdIO source files,
# by changing the way #includes are done, and by excluding the lines
# delimited by "BEGIN non-NrrdIO" and "END non-NrrdIO", as well as doing
# some other hacks
#

if (exists $ENV{"ITK_NRRDIO"}) {
    $ITK = 1;
} else {
    $ITK = 0;
}

$printing = 1;
while (<>) {
    $printing = 0 if (m/BEGIN non-NrrdIO/);
    s|#include "air.h"|#include "NrrdIO.h"|g;
    s|#include "biff.h"|#include "NrrdIO.h"|g;
    s|#include "nrrd.h"|#include "NrrdIO.h"|g;
    s|#include <teem(.*)>|#include "teem$1"|g;
    if ($ITK) {
	s|\/\* NrrdIO-hack-000 \*\/|\/\* THE FOLLOWING INCLUDE IS ONLY FOR THE ITK DISTRIBUTION.\n   This header mangles the symbols in the NrrdIO library, preventing\n   conflicts in applications linked against two versions of NrrdIO. \*\/\n#include "itk_NrrdIO_mangle.h"|g;
    } else {
	s|\/\* NrrdIO-hack-000 \*\/||g;
    }
    if ($ITK) {
        s|\/\* NrrdIO-hack-001 \*\/|#cmakedefine TEEM_STATIC|g;
    } else {
        s|\/\* NrrdIO-hack-001 \*\/||g;
    }
    s|.* \/\* NrrdIO-hack-002 \*\/|#if 1|g;
    s|.* \/\* NrrdIO-hack-003 \*\/|int nrrdStateVerboseIO = 0;|g;
    if ($ITK) {
        s|.* \/\* NrrdIO-hack-004 \*\/|#include "itk_zlib.h"|g;
    } else {
        s|.* \/\* NrrdIO-hack-004 \*\/|#include <zlib.h>|g;
    }
    s|AIR_EXPORT|NRRDIO_EXPORT|g;
    s|BIFF_EXPORT|NRRDIO_EXPORT|g;
    s|NRRD_EXPORT|NRRDIO_EXPORT|g;

    print if $printing;
    $printing = 1 if (m/END non-NrrdIO/);
}
