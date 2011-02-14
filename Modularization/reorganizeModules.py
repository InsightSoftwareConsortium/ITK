#!/usr/bin/python
#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#
#==========================================================================*/
# This script is used to rename the modules and put them into  a new
# non-flat directory structure.
import os
import string

UpperCaseList=['io','ipl','bmp','pde','nrrd','png','gipl','nifti','vtk','ge','tiff','raw','gdcm','jpeg','lsm','xml','klm','fem','fft']

o = open('./Manifest.txt','w')
for line in  open('./Manifest_old.txt','r'):
 if line[0] != '#':
    words = line.split()
    if len(words) != 4:
        print "Missing entries at: "+line
        exit(-1)

    itkFileName = words[0]
    groupName   = words[1]
    moduleName  = words[2]
    subdirName  = words[3]

    mWords = moduleName.split('-');
    if (groupName == 'io'):
      newGroupName = 'IO'
    else:
      newGroupName = string.capitalize(groupName)

    newModuleName = "ITK-"
    newPath = ""

    for word in mWords[1:]:
         if word in UpperCaseList:
            if word == 'io':
              newModuleName = newModuleName +'IO-'
            else:
              newModuleName = newModuleName + string.upper(word)
              newPath = newPath + string.upper(word)
         else:
            newModuleName = newModuleName + string.capitalize(word)
            if word != 'registration':
                newPath = newPath + string.capitalize(word)
    newline = string.ljust(itkFileName,90)+' '+string.ljust(newGroupName,20)+' '+ string.ljust(newModuleName,30)+' '+newPath+'\n'
    o.write(newline)
 else:
    o.write(line)

o.close()
