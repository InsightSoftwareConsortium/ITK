#!/bin/bash
## This script is intended to find files that are missing the proper ITK license.

MODULES_DIR=$(dirname $(dirname $(dirname $0)))/Modules

for i in $(find ${MODULES_DIR} -type f -name "*[.hxx|.h|.cxx]" ); do
  grep "http://www.apache.org/licenses/LICENSE-2.0.txt" $i > /dev/null;
  if [ $? -ne 0 ]; then
    echo "$i" ;
  fi;
done|fgrep -v ThirdParty

##
## for i in $(ls -d $(pwd)/Modules/* |fgrep -v ThirdParty) ; do
##  python Utilities/Maintenance/UpdateCopyrightStatementsInITK.py $i;
## done
