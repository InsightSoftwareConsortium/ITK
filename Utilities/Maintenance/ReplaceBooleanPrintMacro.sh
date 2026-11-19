#!/bin/bash

#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

#% NAME
#%     ReplaceBooleanPrintMacro.sh - use itkPrintSelfBooleanMacro in PrintSelf
#%
#% SYNOPSIS
#%     ReplaceBooleanPrintMacro.sh
#%     ReplaceBooleanPrintMacro.sh -h | --help
#%
#% DESCRIPTION
#%     Replace PrintSelf statements that print a boolean member as On or Off
#%     with itkPrintSelfBooleanMacro. A statement such as
#%
#%         os << indent << "Minimize: " << (m_Minimize ? "On" : "Off")
#%            << std::endl;
#%
#%     becomes
#%
#%         itkPrintSelfBooleanMacro(Minimize);
#%
#%     The member may also be written this->m_Minimize, and the statement
#%     may be split across lines. The files edited and the number of files
#%     are printed for each of the two patterns. Arguments are ignored.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Edits .cxx, .h, and .hxx files in place under the directory named by
#%     the environment variable dir, or under the current directory when
#%     dir is unset. Needs GNU find, GNU grep (for -P), and perl.
#%
#% EXAMPLES
#%     cd Modules && ../Utilities/Maintenance/ReplaceBooleanPrintMacro.sh
#%         From the top of an ITK checkout, convert every module.
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

function adopt_boolean_macro_pattern() {

  local _pattern="$1"

  # Find relevant filenames
  fnames=($(find ${dir} -type f \( -name "*.cxx" -o -name "*.h" -o -name "*.hxx" \) -exec grep -lzP "(?m)\s*${_pattern}\s*" {} + | sort))

  echo "Files identified containing the regex pattern:"
  echo ${_pattern}
  printf '%s\n' "${fnames[@]}"
  echo "File count:" "${#fnames[@]}"

  replacement='itkPrintSelfBooleanMacro'

  # Make the substitution in each file
  echo "Applying substitution..."
  for fname in "${fnames[@]}"; do

    echo ${fname}

    perl -0777 -i -pe "s/${_pattern}/${replacement}(\6);/g" "${fname}"

  done

  echo "Finished"
}

# Define the patterns of interest
pattern1='os << indent((\r\n|\r|\n).*| )<< \"(.*): \"((\r\n|\r|\n).*| )<< \(m_([a-zA-Z0-9]+) \? \"On\" : \"Off\"\)((\r\n|\r|\n).*| )<< std::endl;'

adopt_boolean_macro_pattern "${pattern1}"

pattern2='os << indent((\r\n|\r|\n).*| )<< \"(.*): \"((\r\n|\r|\n).*| )<< \(this->m_([a-zA-Z0-9]+) \? \"On\" : \"Off\"\)((\r\n|\r|\n).*| )<< std::endl;'

adopt_boolean_macro_pattern "${pattern2}"
