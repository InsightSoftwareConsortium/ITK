#!/bin/bash
#
# A script to assist with updating the f2c libary from 
# more recent upstream versions
#

if [ ! -d /tmp/libf2c ]; then
  git clone git@github.com:albertony/libf2c.git /tmp/libf2c
fi

for filename in *; do
  # Upstream uses tabs, local uses only spaces
  gsed -i"" "s/\t/        /g"         /tmp/libf2c/src/$filename
  # Replace with local f2c.h with vxl variant
  gsed -i"" 's/"f2c.h"/"v3p_f2c.h"/g' /tmp/libf2c/src/$filename
  # if there is a difference between local and upstream, then compare differences
  if ! diff --ignore-space-change $filename /tmp/libf2c/src/$filename; then
          vimdiff $filename /tmp/libf2c/src/$filename
  fi
done
if ! diff --ignore-space-change ../v3p_f2c_original.h /tmp/libf2c/src/f2c.h; then
    vimdiff ../v3p_f2c_original.h /tmp/libf2c/src/f2c.h
fi
