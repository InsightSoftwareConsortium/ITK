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
import re
import itk
import os
from optparse import OptionParser

parser = OptionParser(usage='wrappingCoverage.py paths')

parser.add_option(
    "-b",
    "--base",
    dest="base",
    default="Filter",
    help="Base string used to search for the classes (default: Filter).")
parser.add_option(
    "-e",
    "--exclude",
    dest="exclude",
    default=None,
    help="Path of a file with one class to exclude per line (default: None).")
parser.add_option(
    "-E",
    "--no-error",
    action="store_true",
    dest="noError",
    help="Don't generate an error code if all the classes are not wrapped.")

opts, args = parser.parse_args()

# declares classes which will not be wrapped
excluded = set([])
if opts.exclude:
    with open(opts.exclude, 'r') as fp:
        to_exclude = [c.strip() for c in fp.readlines()]
        excluded.update(set(to_exclude))

# get classes from sources
headers = []
for d in args:
    headers += sum([f for p, d, f in os.walk(d)
                   if "Deprecated" not in p and "TestKernel" not in p], [])
classes = set([f[len('itk'):-len('.h')] for f in headers if f.startswith("itk")
              and not f.startswith("itkv3") and
              f.endswith(opts.base + ".h")]) - excluded

# get filter from wrapper files
# remove classes which are not in the toolkit (external projects,
# PyImageFilter, ...)
wrapped = set([a for a in dir(itk) if a.endswith(opts.base)]
              ).intersection(classes)

nonWrapped = classes - wrapped


# print non wrapped classes without much text to stdout, so they can be
# easily reused
for f in sorted(nonWrapped):
    print(f)

# and print stats in stderr to avoid poluting the list above
print("", file=sys.stderr)
print('%i %s' % (len(classes), opts.base), file=sys.stderr)
print('%i wrapped %s' % (len(wrapped), opts.base), file=sys.stderr)
print('%i non wrapped %s' % (len(nonWrapped), opts.base), file=sys.stderr)
covered = len(wrapped) / float(len(classes))
print('%f%% covered' % (covered * 100), file=sys.stderr)
print("", file=sys.stderr)

if not opts.noError:
    sys.exit(len(nonWrapped))
