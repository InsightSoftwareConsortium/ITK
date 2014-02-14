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

import itkBase
import sys
import os
import stat

import itkData

# Put the ITK_TEST_ROOT setting in the global namespace.  This
# package is only used for testing, so this is okay.

ITK_TEST_ROOT = ""

# Look for the -T command line option.
if not ITK_TEST_ROOT:
    for a in range(len(sys.argv)):
        if sys.argv[a] == "-T" and a < len(sys.argv):
            ITK_TEST_ROOT = sys.argv[a + 1]
            break

# Check for the environment variable ::ITK_TEST_ROOT.
if not ITK_TEST_ROOT and 'ITK_TEST_ROOT' in os.environ:
    ITK_TEST_ROOT = os.environ['ITK_TEST_ROOT']


# Use the default output directory.
if not ITK_TEST_ROOT:
    ITK_TEST_ROOT = itkBase.defaultTestRoot
    if ITK_TEST_ROOT == "<NO_DEFAULT>":
        sys.stderr.write("Set ITK_TEST_ROOT or use -T option to specify.\n")
        sys.exit(1)

# Setup testing directories.
ITK_TEST_BASELINE = "%s/Baseline" % itkData.ITK_DATA_ROOT
ITK_TEST_INPUT = "%s/Input" % itkData.ITK_DATA_ROOT
ITK_TEST_OUTPUT = "%s/Output" % ITK_TEST_ROOT

try:
    if not os.path.exists(ITK_TEST_ROOT):
        os.mkdir(ITK_TEST_OUTPUT)
except:
    sys.stderr.write("Bla: %s\n" % repr(sys.exc_info()[0]))
    sys.stderr.write(
        "Unable to create testing output directory with name: %s\n" %
        ITK_TEST_OUTPUT)
    sys.exit(1)
