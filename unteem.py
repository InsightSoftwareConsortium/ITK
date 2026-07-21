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
This helps in converting Teem .c,.h source files into NrrdIO source files, by:
- changing the way #includes are done
- excluding the lines delimited by "BEGIN non-NrrdIO" and "END non-NrrdIO"
- as well as doing some other "NrrdIO-hack"-marked tweaks
"""

import os
import re
import sys


def main():
    """Performs the Teem->NrrdIO source conversion,
    reading Teem source from stdin and writing NrrdIO source to stdout"""

    # If stdin is a terminal, print usage info and exit
    if sys.stdin.isatty():
        print(
            'This it to be used as a filter with piping, e.g.\n'
            '   cat fileT.c | ./unteem.py > fileN.c',
            file=sys.stderr,
        )
        sys.exit(0)

    itk = 1 if os.getenv('ITK_NRRDIO') is not None else 0
    printing = True

    for line in sys.stdin:
        # Suppress printing between markers
        if re.search(r'BEGIN non-NrrdIO', line):
            printing = False

        # .c files shall include NrrdIO.h not air.h/biff.h/nrrd.h
        line = re.sub(r'#(.*)include\s+"air\.h"', r'#\1include "NrrdIO.h"', line)
        line = re.sub(r'#(.*)include\s+"biff\.h"', r'#\1include "NrrdIO.h"', line)
        line = re.sub(r'#(.*)include\s+"nrrd\.h"', r'#\1include "NrrdIO.h"', line)
        # moot for TeemV2: line = re.sub(r'#(.*)include\s+<teem(.*)>', r'#\1include "teem\2"', line)

        # NrrdIO hack replacements
        # self-description of header for ITK
        if itk:
            line = re.sub(
                r'/\* NrrdIO-hack-000 \*/',
                (
                    '/* THE FOLLOWING INCLUDE IS ONLY FOR THE ITK DISTRIBUTION.\n'
                    '   This header mangles the symbols in the NrrdIO library, preventing\n'
                    '   conflicts in applications linked against two versions of NrrdIO. */\n'
                    '#include "itk_NrrdIO_mangle.h"'
                ),
                line,
            )
        else:
            line = re.sub(r'/\* NrrdIO-hack-000 \*/', '', line)

        # handle TEEM_STATIC
        if itk:
            line = re.sub(r'/\* NrrdIO-hack-001 \*/', '#cmakedefine TEEM_STATIC', line)
        else:
            line = re.sub(r'/\* NrrdIO-hack-001 \*/', '', line)

        # remove AIR_EXISTS(x) commentary, just make it airExists(x)
        line = re.sub(
            r'.*/\* NrrdIO-hack-002 \*/.*',
            '/* turn AIR_EXISTS() into airExists(), which is like isfinite() */\n'
            '#define AIR_EXISTS(x) (airExists(x))',
            line,
        )

        # dial IO verbosity all the way down
        line = re.sub(
            r'.* /\* NrrdIO-hack-003 \*/',
            'int nrrdDefaultIoVerbose = 0;',
            line,
        )

        # handle ITK-specific #include of zlib.h
        if itk:
            line = re.sub(r'.* /\* NrrdIO-hack-004 \*/', '#include "itk_zlib.h"', line)
        else:
            line = re.sub(r'.* /\* NrrdIO-hack-004 \*/', '#include <zlib.h>', line)

        # rename the symbol export macro
        line = re.sub(r'\bAIR_EXPORT\b', 'NRRDIO_EXPORT', line)
        line = re.sub(r'\bBIFF_EXPORT\b', 'NRRDIO_EXPORT', line)
        line = re.sub(r'\bNRRD_EXPORT\b', 'NRRDIO_EXPORT', line)

        # for some extra elision hacks, to avoid cluttering up Teem source
        # yet more NrrdIO-hack annotations ...
        skip = False
        ## actually we want to keep this, for new nrrd/encodingAscii.c:_nrrdFgetWord
        # skip |= bool(re.search(r'^#define AIR_WHITESPACE', line))

        if printing and not skip:
            sys.stdout.write(line)

        if re.search(r'END non-NrrdIO', line):
            printing = True


if __name__ == '__main__':
    main()
