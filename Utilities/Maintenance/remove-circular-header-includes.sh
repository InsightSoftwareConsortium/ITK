#!/bin/bash
# This script finds cases where .hxx files include same named
# .h file, which results in recursive inclusion of the .h.

SRC_DIR=$1
if [ ! -d "${SRC_DIR}" ]; then
  echo "ERROR: Directory not found ${SRC_DIR}"
fi


for hdr_imp in $(find "${SRC_DIR}" -name "*.hxx" |grep -F -v ThirdParty ); do
  hdr_imp_base=$(basename "${hdr_imp}")
  hdr_base=${hdr_imp_base//.hxx/.h}
  sed -i "/ *#include.*${hdr_base}./d" "${hdr_imp}"
done
