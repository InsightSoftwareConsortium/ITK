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



# To run it, type ./modulizer.py  ITK_PATH  ModularITK_PATH   from
# the itk-modulizer root directory.



import shutil
import os.path
import re
import sys
import os
import stat
import glob


if len(sys.argv) != 3:
    print("USAGE:  {0} [monolithic ITK PATH] [modular ITK PATH]".format(sys.argv[0]))
    sys.exit(-1)

HeadOfITKTree = sys.argv[1];
if (HeadOfITKTree[-1] == '/'):
    HeadOfITKTree = HeadOfITKTree[0:-1]

HeadOfModularITKTree = sys.argv[2];
if (HeadOfModularITKTree[-1] ==  '/'):
    HeadOfModularITKTree = HeadOfModularITKTree[0:-1]


moduleName = 'itk-common'
if os.path.isdir(HeadOfModularITKTree+'/'+moduleName):
   #  shutil.copy('./templateModule/'+moduleName+'/CMakeLists.txt', HeadOfModularITKTree+'/'+moduleName)
   #  shutil.copy('./templateModule/'+moduleName+'/src/itkConfigure.h.in', HeadOfModularITKTree+'/'+moduleName+'/src')
    # shutil.copy('./templateModule/'+moduleName+'/itk-module.cmake', HeadOfModularITKTree+'/'+moduleName)

#/src /CMakeLists.txt
     cxxFiles = glob.glob(HeadOfModularITKTree+'/'+moduleName+'/src/*.cxx')
     cxxFileList='';
     for cxxf in cxxFiles:
          filename=cxxf.split('/')[-1]
          if filename[:3] !='vnl':
             cxxFileList = cxxFileList+filename+'\n'

     o = open( HeadOfModularITKTree+'/'+moduleName+'/src/CMakeLists.txt','w')
     for line in open('./templateModule/'+moduleName+'/src/CMakeLists.txt','r'):
            line = line.replace('LIST_OF_CXX_FILES',cxxFileList[0:-1]) #get rid of the last \n
            o.write(line);
     o.close()
