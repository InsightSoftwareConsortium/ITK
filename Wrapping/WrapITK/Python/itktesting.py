import itkbase
import sys
import os
import stat

import itkdata

# Put the ITK_TEST_ROOT setting in the global namespace.  This
# package is only used for testing, so this is okay.
  
ITK_TEST_ROOT = ""

# Look for the -T command line option.
if not ITK_TEST_ROOT:
  for a in range(len(sys.argv)):
    if sys.argv[a] == "-T" and a < len(sys.argv):
      ITK_TEST_ROOT = sys.argv[a+1]
      break

# Check for the environment variable ::ITK_TEST_ROOT.
if not ITK_TEST_ROOT and os.environ.has_key('ITK_TEST_ROOT'):
  ITK_TEST_ROOT = os.environ['ITK_TEST_ROOT']

  
# Use the default output directory.
if not ITK_TEST_ROOT:
  ITK_TEST_ROOT = itkbase.defaultTestRoot
  if ITK_TEST_ROOT == "<NO_DEFAULT>":
    sys.stderr.write("Set ITK_TEST_ROOT or use -T option to specify.\n")
    sys.exit(1)

# Setup testing directories.
ITK_TEST_BASELINE = "%s/Baseline" % itkdata.ITK_DATA_ROOT
ITK_TEST_INPUT    = "%s/Input"    % itkdata.ITK_DATA_ROOT
ITK_TEST_OUTPUT   = "%s/Output"   % ITK_TEST_ROOT

try:
  if not os.path.exists(ITK_TEST_ROOT):
    os.mkdir(ITK_TEST_OUTPUT)
except:
  sys.stderr.write("Bla: %s\n" % `sys.exc_info()[0]`)
  sys.stderr.write("Unable to create testing output directory with name: %s\n" % ITK_TEST_OUTPUT)
  sys.exit(1)


