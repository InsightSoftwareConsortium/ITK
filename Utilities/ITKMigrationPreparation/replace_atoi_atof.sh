#!/bin/bash
# \author Hans J. Johnson

#% NAME
#%     replace_atoi_atof.sh - replace atoi and atof with std::stoi and std::stod
#%
#% SYNOPSIS
#%     replace_atoi_atof.sh
#%     replace_atoi_atof.sh -h | --help
#%
#% DESCRIPTION
#%     Replace calls to atoi with std::stoi in the tracked C++ files at or
#%     below the current directory, and write a suggested commit message to
#%     /tmp/COMMIT_MESSAGE_atoi. The ato[if] functions cannot tell a parsed
#%     zero from a failed conversion; std::sto[id] throw an exception
#%     instead. Nothing is staged or committed, and no #include is added.
#%
#%     The script is also meant to replace atof with std::stod, but it writes
#%     the atof file list to /tmp/atoi_files.list and reads it from
#%     /tmp/atof_files.list. Unless a file of that name already exists, no
#%     atof call is changed.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Requires git and BSD or macOS sed: the script passes an empty suffix
#%     to sed -i as a separate argument, which GNU sed rejects without
#%     changing any file.
#%
#%     Only calls preceded by a space are replaced. The file-type filter is
#%     an unanchored regular expression in which each period matches any
#%     character, so it also accepts many paths that are not C++ files. The
#%     script writes /tmp/atoi_files.list and /tmp/COMMIT_MESSAGE_atoi.
#%
#% EXAMPLES
#%     ./Utilities/ITKMigrationPreparation/replace_atoi_atof.sh
#%         Replace atoi calls in an ITK checkout, run from its top.
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

git grep  -l "\<atoi\> *(" |grep -E "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/atoi_files.list
for ff in $(cat /tmp/atoi_files.list); do
  sed -i "" 's/ atoi *(/ std::stoi(/g' $ff
done

git grep  -l "\<atof\> *(" |grep -E "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/atoi_files.list
for ff in $(cat /tmp/atof_files.list); do
  sed -i "" 's/ atof *(/ std::stod(/g' $ff
done

cat > /tmp/COMMIT_MESSAGE_atoi <<EOF
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

echo "Reference commit message in /tmp/COMMIT_MESSAGE_atoi"
