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
# This script is used to automate the modularization process. The following
# steps  are included:
# 1. Move the files in the monolithic ITK into modules of the modularized ITK.
#    A manifest text file that lists all the files and their destinations is
#    required to run the script.By default, the manifest file is named as
#    "Manifest.txt" in the  same directory of this script.
# 2. Create CMake Files and put them into modules.



# To run it, type ./modulizer.py  ITK_PATH  ModularITK_PATH   from
# the itk-modulizer root directory.

print "*************************************************************************"
print "WARNINGs! This modularization script is still in its experimental stage."
print "Current ITK users should not run this script."
print "*************************************************************************"


import shutil
import os.path
import re
import sys
import os
import stat
import glob


if len(sys.argv) < 3:
    print("USAGE:  {0} [monolithic ITK PATH] [modular ITK PATH]".format(sys.argv[0]))
    sys.exit(-1)

HeadOfITKTree = sys.argv[1];
if (HeadOfITKTree[-1] == '/'):
    HeadOfITKTree = HeadOfITKTree[0:-1]

HeadOfModularITKTree = sys.argv[2];
if (HeadOfModularITKTree[-1] ==  '/'):
    HeadOfModularITKTree = HeadOfModularITKTree[0:-1]

# copy the whole ITK tree over to a tempery dir
HeadOfTempTree ="./ITK_remaining"

if os.path.isdir(HeadOfTempTree):
    shutil.rmtree(HeadOfTempTree)

print("Start to copy" + HeadOfITKTree + " to  ./ITK_remaining ...")
shutil.copytree(HeadOfITKTree,HeadOfTempTree, ignore = shutil.ignore_patterns('.git','.git*'))
print("Done copying!")


# clean up the dirs first
if os.path.isdir(HeadOfModularITKTree):
   if  len(sys.argv) > 3:
      if (sys.argv[3] == 'y'):
         answer = 'y'
   else:
      print("Warning: The directory {0} exists! It needs to be wiped out first.".format(HeadOfModularITKTree))
      answer = raw_input("Do you want to clean up this directory? [y/n]: " )

   if (answer == 'y'):
       shutil.rmtree(HeadOfModularITKTree)
       cmd ='git clone git@kwsource.kitwarein.com:itk/modularITK.git  '+HeadOfModularITKTree
       os.system(cmd)
   else:
       print('please choose another directory for modularized ITK')
       exit(-1)

if not os.path.isdir('./logs'):
  os.makedirs('./logs')

# read the manifest file
print ("moving files from ./ITK_remaining into modules in {0}".format(HeadOfModularITKTree))
numOfMissingFiles = 0;
missingf =  open('./logs/missingFiles.log','w')
moduleList=[];
for line in open("./Manifest.txt",'r'):
   # parse the string
 if line[0] != '#':
    words = line.split()
    if len(words) != 4:
        print "Missing entries at: "+line
        exit(-1)

    itkFileName = words[0]
    groupName = words[1]
    moduleName = words[2]
    subdirName = words[3]


    inputfile = HeadOfTempTree+'/'+words[0]
    outputPath = HeadOfModularITKTree+'/'+ moduleName+'/'+subdirName
    if len(moduleList) == 0:
       moduleList.append(moduleName)
    elif moduleName != moduleList[-1]:
       moduleList.append(moduleName)

    # creat the path
    if not os.path.isdir(outputPath):
       os.makedirs(outputPath)


    # copying files to the destination
    if  os.path.isfile(inputfile):
       shutil.move(inputfile, outputPath)
    else:
       missingf.write(inputfile+'\n')
       numOfMissingFiles = numOfMissingFiles + 1

missingf.close()
print ("listed {0} missing files to ./logs/missingFiles.log").format(numOfMissingFiles)

# find the unique module names
def unique(seq):
    # not order preserving
    set = {}
    map(set.__setitem__, seq, [])
    return set.keys()


moduleList = unique(moduleList)


# list the new files
newf =  open('./logs/newFiles.log','w')
for (root, subDirs, files) in os.walk(HeadOfTempTree):
   for afile in files:
     newf.write(os.path.join(root, afile)+'\n')
newf.close()
print ("listed new files to ./logs/newFiles.log")

###########################################################################

print ('creating cmake files for each module (from the template module)')
#moduleList = os.listdir(HeadOfModularITKTree)
for  moduleName in moduleList:
  if os.path.isdir(HeadOfModularITKTree+'/'+moduleName):
     # cooy the LICENSE and NOTICE
     shutil.copy('./templateModule/itk-template-module/LICENSE', HeadOfModularITKTree+'/'+moduleName)
     shutil.copy('./templateModule/itk-template-module/NOTICE',  HeadOfModularITKTree+'/'+moduleName)

     # write CMakeLists.txt
     filepath = HeadOfModularITKTree+'/'+moduleName+'/CMakeLists.txt'
     if not os.path.isfile(filepath):
       o = open(filepath,'w')
       for line in open('./templateModule/itk-template-module/CMakeLists.txt','r'):
           line = line.replace('itk-template-module',moduleName)
           o.write(line);
       o.close()

     # write src/CMakeLists.txt
     # list of CXX files
     if os.path.isdir(HeadOfModularITKTree+'/'+moduleName+'/src'):
       cxxFiles = glob.glob(HeadOfModularITKTree+'/'+moduleName+'/src/*.cxx')
       cxxFileList='';
       for cxxf in cxxFiles:
            cxxFileList = cxxFileList+cxxf.split('/')[-1]+'\n'
       filepath = HeadOfModularITKTree+'/'+moduleName+'/src/CMakeLists.txt'
       if not os.path.isfile(filepath):
         o = open(filepath,'w')
         for line in open('./templateModule/itk-template-module/src/CMakeLists.txt','r'):
            line = line.replace('itk-template-module',moduleName)
            line = line.replace('LIST_OF_CXX_FILES',cxxFileList[0:-1]) #get rid of the last \n
            o.write(line);
         o.close()

     # write  test/CMakeLists.txt
     if os.path.isdir(HeadOfModularITKTree+'/'+moduleName+'/test'):
       cxxFiles = glob.glob(HeadOfModularITKTree+'/'+moduleName+'/test/*.cxx')
       cxxFileList='';
       for cxxf in cxxFiles:
            cxxFileList = cxxFileList+cxxf.split('/')[-1]+'\n'
       filepath = HeadOfModularITKTree+'/'+moduleName+'/test/CMakeLists.txt'
       if not os.path.isfile(filepath):
           o = open(filepath,'w')
           for line in open('./templateModule/itk-template-module/test/CMakeLists.txt','r'):
              words= moduleName.split('-')
              moduleNameMod='';
              for word in words:
                 moduleNameMod=moduleNameMod + word.capitalize()
              line = line.replace('itkTemplateModule',moduleNameMod)
              line = line.replace('itk-template-module',moduleName)
              line = line.replace('LIST_OF_CXX_FILES',cxxFileList[0:-1]) #get rid of the last \n
              o.write(line);
           o.close()


    # write CTestConfig.cmake
     filepath = HeadOfModularITKTree+'/'+moduleName+'/CTestConfig.cmake'
     if not os.path.isfile(filepath):
        o = open(filepath,'w')
        for line in open('./templateModule/itk-template-module/CTestConfig.cmake','r'):
            line = line.replace('itk-template-module',moduleName)
            o.write(line);
        o.close()

   # write itk-module.cmake, which contains dependency info
     filepath = HeadOfModularITKTree+'/'+moduleName+'/itk-module.cmake'
     if not os.path.isfile(filepath):
        o = open(filepath,'w')
        for line in open('./templateModule/itk-template-module/itk-module.cmake','r'):
            line = line.replace('itk-template-module',moduleName)
            o.write(line);
        o.close()
