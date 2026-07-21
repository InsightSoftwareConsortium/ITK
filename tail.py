#!/usr/bin/env python3
#
#  NrrdIO: C library for NRRD file IO (with optional compressions)
#  Copyright (C) 2009--2026  University of Chicago
#  Copyright (C) 2005--2008  Gordon Kindlmann
#  Copyright (C) 1998--2004  University of Utah
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

"""
This removes a large comment-block at the top of a Teem .c or .h file, by
eliding the lines up to and including a line containing nothing but '*/'
"""

import sys
import re

PRINTING = False
for line in sys.stdin:
    if PRINTING:
        sys.stdout.write(line)
    elif re.match(r'^\*/\s*$', line):
        PRINTING = True
