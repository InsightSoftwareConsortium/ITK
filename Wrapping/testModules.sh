#!/usr/bin/env bash

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
