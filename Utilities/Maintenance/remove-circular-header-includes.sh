#!/bin/bash

#% NAME
#%     remove-circular-header-includes.sh - drop .h includes from .hxx files
#%
#% SYNOPSIS
#%     remove-circular-header-includes.sh SRC_DIR
#%     remove-circular-header-includes.sh -h | --help
#%
#% DESCRIPTION
#%     For each .hxx file under SRC_DIR, delete the lines that include the
#%     .h file of the same name, which would include that header
#%     recursively. Paths containing ThirdParty are skipped. Files are edited
#%     in place.
#%
#% OPTIONS
#%     SRC_DIR
#%         Directory to search, such as Modules in an ITK checkout.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Requires GNU sed. Edits are not staged or committed.
#%
#%     The match is loose: a deleted line is any line containing #include
#%     followed later by the header name and one more character, and the
#%     period in the name matches any character. An include of the .hxx file
#%     itself, or of any header whose name contains the .h file name, is
#%     deleted too, as is such a line inside a comment.
#%
#%     If SRC_DIR is missing or not a directory, the script prints an error
#%     but does not stop; find then fails and no file is changed.
#%
#% EXAMPLES
#%     ./Utilities/Maintenance/remove-circular-header-includes.sh Modules
#%         Remove self-includes from the .hxx files of an ITK checkout.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/FindRedundantHeaderIncludes.py:
#%         Find header includes that other includes already provide.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

SRC_DIR=$1
if [ ! -d "${SRC_DIR}" ]; then
  echo "ERROR: Directory not found ${SRC_DIR}"
fi


for hdr_imp in $(find "${SRC_DIR}" -name "*.hxx" |grep -F -v ThirdParty ); do
  hdr_imp_base=$(basename "${hdr_imp}")
  hdr_base=${hdr_imp_base//.hxx/.h}
  sed -i "/ *#include.*${hdr_base}./d" "${hdr_imp}"
done
