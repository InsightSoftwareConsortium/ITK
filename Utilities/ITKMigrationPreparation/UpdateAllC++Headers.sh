#!/bin/bash

for hdr in \
assert \
ctype \
errno \
fenv \
float \
inttypes \
iso646 \
limits \
locale \
math \
setjmp \
signal \
stdarg \
stdbool \
stddef \
stdint \
stdio \
stdlib \
string \
tgmath \
time \
uchar \
wchar \
wctype \
; do
  newstring="<c${hdr}>"
  oldstring="<${hdr}.h>"
  echo "sed -i '' -e \"s/${oldstring}/${newstring}/g\""

git grep -l "${oldstring}" | \
  fgrep -v ThirdParty | \
  xargs sed -i '' -e "s/${oldstring}/${newstring}/g"
done
