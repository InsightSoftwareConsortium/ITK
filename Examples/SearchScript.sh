#!/bin/bash
#
#  Author: Darren Weber
# 
#    Copyright (c) Insight Software Consortium. All rights reserved.
#    See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
#    This software is distributed WITHOUT ANY WARRANTY; without even 
#    the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
#    PURPOSE.  See the above copyright notices for more information.
#
#

if [ $# -lt 1 ]; then
    echo "$0 'search term' ['search term' ...]"
    exit 1
fi

#
# Search the CXX files
#
itkExamplePath="./*/*.cxx"
echo "Searching ITK .cxx files in: ${itkExamplePath}"

for term in $@; do
    echo
    echo "Search term: ${term}"
    grep -l -E -e ${term} ${itkExamplePath}
done
