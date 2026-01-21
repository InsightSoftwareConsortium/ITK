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
This prints (to stdout) a header file intended to be included into
source files where there is a concern of name-space collision induced
by linking to two different version of NrrdIO (or NrrdIO and Teem)
"""

import re
import subprocess
import sys

import textwrap


def main():
    """Filters outout of `nm libNrrdIO.a` to produce #define renames"""
    if len(sys.argv) != 3:   #      0            1            2
        sys.exit('usage: python mangle.py <symbolPrefix> <filePrefix>')
    symb_pfx = sys.argv[1]
    file_pfx = sys.argv[2]

    mac = sys.platform == 'darwin'

    print(
        textwrap.dedent(
            f"""\
        #ifndef __{file_pfx}_NrrdIO_mangle_h
        #define __{file_pfx}_NrrdIO_mangle_h

        /*
        This header file mangles all symbols exported from the NrrdIO library,
        to be included in all .c files while building NrrdIO in settings that
        require symbol renaming.

        This file was made by the mangle.py script in the NrrdIO distribution:

          python mangle.py {symb_pfx} {file_pfx}

        and then > redirecting that to the current file, via the "0-gen.sh itk"
        script in the NrrdIO distribution.
        This uses nm to list all text (T), data (D) symbols, as well as
        read-only (R) things (on Linux) and "other" (S) things (on Mac).
        On Macs, the preceding underscore is removed.

        This also ensures that a few others things starting with nrrd are
        included, and prevents variables ending with .N* where N is some
        number, from inclusion.
        */
        """
        )
    )

    try:
        with subprocess.Popen(['nm', 'libNrrdIO.a'], stdout=subprocess.PIPE, text=True) as nm_proc:
            for line in nm_proc.stdout:
                # Skip local numbered variants (e.g. foo.1) or .eh symbols
                if re.search(r'.*\s.*\.[0-9]', line):
                    continue
                if line.strip().endswith('.eh'):
                    continue
                # Match lines with symbol types of interest
                match = re.search(r'\s[TBDSR]\s', line)
                if match:
                    symbol = re.sub(r'.*\s[TBDSR]\s(.*)', r'\1', line).strip()
                    if mac and symbol.startswith('_'):
                        symbol = symbol[1:]
                    print(f'#define {symbol} {symb_pfx}_{symbol}')
    except FileNotFoundError:
        sys.exit('Error: `nm` not found in PATH')

    # after 'with', process has exited
    if nm_proc.returncode not in (0, None):
        sys.exit(f'`nm` exited with status {nm_proc.returncode}')

    print(f'#endif  /* __{file_pfx}_NrrdIO_mangle_h */')


if __name__ == '__main__':
    main()
