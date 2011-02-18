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
# This script is used to format the Manifest.txt for easy viewing (line up the columes).
import os
import string


o = open('./Manifest_tmp.txt','w')
for line in  open('./Manifest.txt','r'):
 if line[0] != '#':
    words = line.split()
    if len(words) != 4:
        print "Missing entries at: "+line
        exit(-1)

    itkFileName = words[0]
    groupName   = words[1]
    moduleName  = words[2]
    subdir      = words[3]

    line = string.ljust(itkFileName,80)+' '+string.ljust(groupName,15)+' '+ string.ljust(moduleName,30)+' '+ subdir+ '\n'
    o.write(line)
 else:
    o.write(line)

o.close()
os.system('mv -f ./Manifest_tmp.txt Manifest.txt')
