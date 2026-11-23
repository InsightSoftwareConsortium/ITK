#!/usr/bin/env bash

#% NAME
#%     WrappingModulesCheck.sh - find wrapping files in the wrong module
#%
#% SYNOPSIS
#%     WrappingModulesCheck.sh
#%     WrappingModulesCheck.sh -h | --help
#%
#% DESCRIPTION
#%     Report each wrapping specification (*.wrap) that lives in a different
#%     module from the C++ header of the class it wraps. A wrapping file
#%     belongs in the same module as its header. VNL and VCL wrapping files
#%     are not checked. Nothing is modified.
#%
#%     Each mismatch prints as "Wrong module: <wrap file> <header>". The exit
#%     status is 1 if any mismatch is found, and 0 otherwise.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Run from the root of the ITK source tree; the script searches the
#%     relative path Modules/. A wrapping file matches every header with the
#%     same base name, so a class name used in two modules can be reported
#%     even when the wrapping file is in the right place.
#%
#% EXAMPLES
#%     ./Utilities/Maintenance/WrappingModulesCheck.sh
#%         List misplaced wrapping files in the current source tree.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

error=0
ws=`find Modules/ -name *.wrap | grep -v vcl | grep -v vnl`
for w in $ws; do
  cpp=`basename $w | cut -d. -f1`.h
  cs=`find Modules -name $cpp`
  for c in $cs; do
    bc=`dirname $c`
    bc=`dirname $bc`
    bw=`dirname $w`
    bw=`dirname $bw`
    if [ "$bc" != "$bw" ]; then
      echo Wrong module: $w $c
      error=1
    fi
  done
done
exit $error
