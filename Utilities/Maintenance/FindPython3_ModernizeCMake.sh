#!/bin/bash

#% NAME
#%     FindPython3_ModernizeCMake.sh - rewrite old CMake Python variables
#%
#% SYNOPSIS
#%     FindPython3_ModernizeCMake.sh
#%     FindPython3_ModernizeCMake.sh -h | --help
#%
#% DESCRIPTION
#%     Help move CMake code from the deprecated find_package(PythonInterp)
#%     and find_package(PythonLibs) to find_package(Python3). Every file
#%     named CMakeLists.txt, or with .cmake in its name, under the current
#%     directory is edited in place:
#%
#%     - find_package(PythonInterp ...) becomes
#%       find_package(Python3 COMPONENTS Interpreter ...), and
#%       find_package(PythonLibs ...) becomes
#%       find_package(Python3 COMPONENTS Development ...).
#%     - PYTHONINTERP_FOUND, PYTHON_EXECUTABLE, PYTHON_VERSION_STRING,
#%       PYTHON_VERSION_MAJOR, PYTHON_VERSION_MINOR, PYTHON_VERSION_PATCH,
#%       PYTHONLIBS_FOUND, PYTHON_LIBRARIES, PYTHON_INCLUDE_DIRS, and
#%       PYTHONLIBS_VERSION_STRING become their Python3_ equivalents.
#%     - PYTHON_INCLUDE_PATH, PYTHON_INCLUDE_DIR, PYTHON_LIBRARY, and
#%       PYTHON_DEBUG_LIBRARIES have no equivalent. They become names
#%       containing XXX and NO_MAPPING, which must be fixed by hand.
#%     - SIMPLE_PYTHON_EXECUTABLE and ITK_WRAP_PYTHON_LIBRARY are restored
#%       after the substitutions above.
#%
#%     The result is a starting point, not a finished change: review every
#%     edit before committing. The script does not commit. Only the
#%     first argument is checked for -h or --help; other arguments are
#%     ignored.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit with status 1.
#%
#% IMPLEMENTATION
#%     Needs GNU sed; on macOS it calls gsed. Run from the top of the tree
#%     to convert. Writes /tmp/temp.sed (the sed program) and
#%     /tmp/git_message.txt (a suggested commit message), and leaves both
#%     in place.
#%
#% EXAMPLES
#%     cd ~/src/ITK && ./Utilities/Maintenance/FindPython3_ModernizeCMake.sh
#%         Convert the CMake files of an ITK checkout, then review the diff.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.

COMMIT_INFO="COMP: Script to initiate updating to find_package(Python3)

Now that cmake FindPython3 can be universally
used, prefer to only use that set of variable
names for identifying the python executables.

Intial procesing with manual cleanup by:

ITK/Utilities/Maintenance/FindPython3_ModernizeCMake.sh

This maintenance script can be used to assist with moving from
the deprecated find_package(PythonInterp) and find_package(PythonLib)
to the preferred find_package(Python3) variable names.

------------------------------
NOTE: This script is intended to make it easier
       to identify old find_package(PythonInterp
       uses, but it can not be blindly used.
       each change must be carefully reviewed.
"

# Utility functions
usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

die() {
  echo "$@" 1>&2; exit 1
}

while test $# -gt 0;
do
  opt="$1";
  case "$opt" in
    "-h"|"--help")
      shift;
      help=true
      break;;
    *)
      break;;
  esac
done

if test $help; then
  usage
  exit 1
fi

# Deprecated (since cmake 3.12) variables from the FindPythonInterp
# PYTHONINTERP_FOUND
# PYTHON_EXECUTABLE
# PYTHON_VERSION_STRING
# PYTHON_VERSION_MAJOR
# PYTHON_VERSION_MINOR
# PYTHON_VERSION_PATCH
# Additional deprecated (since cmake 3.12) variables from the FindPythonLibs
# PYTHONLIBS_FOUND
# PYTHON_LIBRARIES
# PYTHON_INCLUDE_PATH
# PYTHON_INCLUDE_DIRS
# PYTHON_DEBUG_LIBRARIES
# PYTHONLIBS_VERSION_STRING
# PYTHON_LIBRARY
# PYTHON_INCLUDE_DIR



cat > /tmp/temp.sed << EOF
s/PYTHONINTERP_FOUND/Python3_Interpreter_FOUND/g
s/PYTHON_EXECUTABLE/Python3_EXECUTABLE/g
s/PYTHON_VERSION_STRING/Python3_VERSION/g
s/PYTHON_VERSION_MAJOR/Python3_VERSION_MAJOR/g
s/PYTHON_VERSION_MINOR/Python3_VERSION_MINOR/g
s/PYTHON_VERSION_PATCH/Python3_VERSION_PATCH/g
s/PYTHONLIBS_FOUND/Python3_Development_FOUND/g
s/PYTHON_LIBRARIES/Python3_LIBRARIES/g
s/PYTHON_INCLUDE_PATH/Python3_XXXXXXX_INCLUDE_PATH_NO_MAPPING_FOR_THIS_PYTHON.h_PATH_FINDER/g
s/PYTHON_INCLUDE_DIRS/Python3_INCLUDE_DIRS/g
s/PYTHON_DEBUG_LIBRARIES/Python3_XXXXX_NO_MAPPING_FOR_THIS_DEBUG_LIBRARIES_FLAG/g
s/PYTHONLIBS_VERSION_STRING/Python3_VERSION/g
s/PYTHON_LIBRARY/Python3_XXXXXX_LIBARY__NO_MAPPING_FOR_THIS_/g
s/PYTHON_INCLUDE_DIR/Python3_XXXXXXX_INCLUDE_DIR_NO_MAPPING_FOR_THIS_PYTHON.h_PATH_FINDER/g
s/find_package *( *PythonInterp *REQUIRED *)/find_package(Python3 COMPONENTS Interpreter REQUIRED)/g
s/find_package *( *PythonInterp *)/find_package(Python3 COMPONENTS Interpreter)/g
s/find_package *( *PythonLibs *)/find_package(Python3 COMPONENTS Development)/g
s/find_package *( *PythonInterp/find_package(Python3 COMPONENTS Interpreter/g
s/find_package *( *PythonLibs/find_package(Python3 COMPONENTS Development/g
s/below_restore_pattern_match_changes_to_original/values/g
s/SIMPLE_Python3_EXECUTABLE/SIMPLE_PYTHON_EXECUTABLE/g
s/ITK_WRAP_Python3_XXXXXX_LIBARY__NO_MAPPING_FOR_THIS_/ITK_WRAP_PYTHON_LIBRARY/g
EOF

if [ $(uname) == "Darwin" ] ; then
  SEDBIN=gsed
else
  SEDBIN=sed
fi

# Do the replacement
find . -type f \( -name 'CMakeLists.txt' -o -name '*\.cmake*' \) -exec ${SEDBIN} -i''  -f /tmp/temp.sed {} \;

cat > /tmp/git_message.txt << EOF
${COMMIT_INFO}
EOF
