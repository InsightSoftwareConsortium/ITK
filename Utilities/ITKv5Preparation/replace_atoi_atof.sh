#!/bin/bash
# \author


git grep  -l "\<atoi\> *(" |fgrep "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/atoi_files.list
for ff in $(cat /tmp/atoi_files.list); do
  sed -i "" 's/ atoi *(/ std::stoi(/g' $ff
done

git grep  -l "\<atof\> *(" |fgrep "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/atoi_files.list
for ff in $(cat /tmp/atof_files.list); do
  sed -i "" 's/ atof *(/ std::stod(/g' $ff
done

echo > /tmp/COMMIT_MESSAGE_atoi <<EOF
STYLE: Prefer error checked std::sto[id] over ato[if]

The ato[if] functions do not provide mechanisms for distinguishing between
'0' and the error condion where the input can not be converted.

std::sto[id] provides exception handling and detects when an invalid
string attempts to be converted to an [integer|double].

ato[if]()
   Con: No error handling.
   Con: Handle neither hexadecimal nor octal.

The use of ato[if] in code can cause it to be subtly broken.
ato[if] makes two very big assumptions indeed:
   The string represents an integer/floating point value.
   The integer can fit into an int.
EOF

echo "Reference commit messsage in /tmp/COMMIT_MESSAGE_atoi"
