#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

from __future__ import print_function

import sys
import commands
import tempfile
from optparse import OptionParser

parser = OptionParser(usage="usage: %prog")
parser.add_option(
    "--start-from",
    dest="startFrom",
    default=None,
    metavar="CLASS",
    help="")
parser.add_option(
    "-v",
    "--verbose",
    action="store_true",
    dest="verbose",
    default=False,
    help="")
parser.add_option(
    "-k",
    "--keep",
    action="store_true",
    dest="keep",
    default=False,
    help="")
(options, args) = parser.parse_args()

if options.keep:
    mode = "a"
else:
    mode = "w"
segfaultFile = file(args[0], mode)

logs = tempfile.NamedTemporaryFile()

lastClass = options.startFrom
ret = 1
while ret != 0:
    command = "python returnedTypeCoverage.py -v5 --exclude " + \
        sys.argv[1] + " --log-file '" + logs.name + "'"
    if lastClass:
        command += " --start-from " + lastClass
    if options.verbose:
        print(command)
    (ret, output) = commands.getstatusoutput(command)
    if ret != 0:
        # find last args (the ones which caused the segfault)
        faultyArgs = None
        logs.file.seek(0)
        for l in reversed(logs.file.readlines()):
            l = l.strip()
            if faultyArgs is None and l.startswith('('):
                faultyArgs = l
            if faultyArgs is not None:
                # find the last class
                if len(l) != 0 and l[0].isupper():
                    lastClass = l
                    break
        print(repr(faultyArgs))
        segfaultFile.write(faultyArgs + "\n")
        segfaultFile.flush()
