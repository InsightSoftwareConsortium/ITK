#!/bin/bash

#% NAME
#%     FindMissingCopyrightStatementsInITK.sh - list files missing the license
#%
#% SYNOPSIS
#%     FindMissingCopyrightStatementsInITK.sh
#%     FindMissingCopyrightStatementsInITK.sh -h | --help
#%
#% DESCRIPTION
#%     Print the path of each source file under Modules/ that does not
#%     contain the Apache License URL of the ITK license header. Paths
#%     containing ThirdParty are not reported. Nothing is modified.
#%
#%     The exit status is 0 if any file is reported, and 1 otherwise.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Modules/ is found from the path used to call the script, three
#%     directory levels up, so call it by a path that includes
#%     Utilities/Maintenance, as in the example.
#%
#%     The file-name pattern is a bracket expression, not a list of
#%     extensions: it selects every file whose name ends in c, h, x, a
#%     period, or a vertical bar, such as .c and .sh files as well as .h,
#%     .hxx, and .cxx files.
#%
#% EXAMPLES
#%     ./Utilities/Maintenance/FindMissingCopyrightStatementsInITK.sh
#%         List files without the license header, run from the ITK root.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/UpdateCopyrightStatementsInITK.py:
#%         Update the license header of ITK source files.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

MODULES_DIR=$(dirname $(dirname $(dirname $0)))/Modules

for i in $(find ${MODULES_DIR} -type f -name "*[.hxx|.h|.cxx]" ); do
  grep "https://www.apache.org/licenses/LICENSE-2.0.txt" $i > /dev/null;
  if [ $? -ne 0 ]; then
    echo "$i" ;
  fi;
done|fgrep -v ThirdParty

##
## for i in $(ls -d $(pwd)/Modules/* |fgrep -v ThirdParty) ; do
##  python Utilities/Maintenance/UpdateCopyrightStatementsInITK.py $i;
## done
