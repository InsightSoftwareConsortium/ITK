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

import re
import sys
import os
import shutil
import stat
import glob
import modulizerHelper

if len(sys.argv) < 3:
    print("USAGE:  "+sys.argv[0]+" [monolithic ITK PATH] [modular ITK PATH]")
    sys.exit(-1)

HeadOfITKTree = sys.argv[1];
if (HeadOfITKTree[-1] == '/'):
    HeadOfITKTree = HeadOfITKTree[0:-1]

HeadOfModularITKTree = sys.argv[2];
if (HeadOfModularITKTree[-1] ==  '/'):
    HeadOfModularITKTree = HeadOfModularITKTree[0:-1]


#----------------------------------------------------------------------------------------------------
# copy the whole ITK tree over to a tempery dir, except the data
HeadOfTempTree = HeadOfModularITKTree+'/ITK_remaining'

print("Start to copy" + HeadOfITKTree + " to " + HeadOfTempTree + "   ...")
modulizerHelper.copy_directory(HeadOfITKTree, HeadOfTempTree)
print("Done copying!")

LogDir=HeadOfModularITKTree+'/logs'
if not os.path.isdir(LogDir):
  os.makedirs(LogDir)

# read the manifest file
print ("moving files from "+HeadOfTempTree+" into "+HeadOfModularITKTree)
numOfMissingFiles = 0;
missingf =  open(LogDir+'/missingFiles.log','w')
moduleList=[];
for line in open("./Manifest.txt",'r'):
   # parse the string
 if line[0] != '#':
    words = line.split()
    if len(words) != 4:
        print "Missing entries at: "+line
        exit(-1)

    itkFileName = words[0]
    groupName   = words[1]
    moduleName  = words[2]
    fileExt = itkFileName.split('.')[-1]
    if fileExt == 'h' or fileExt == 'txx' or fileExt == 'inc':
       subdir = 'include'
    elif fileExt == 'cxx' or fileExt =='c' or fileExt == 'in' or fileExt == 'cl' :
        if 'Test' in itkFileName or 'test' in itkFileName:
            if moduleName != 'ITK-TestKernel':
                subdir = 'test'
            else:
                subdir =  'include'
        else:
            subdir = 'src'

    desPath = groupName + '/'+words[3] + '/'+subdir


    inputfile = HeadOfTempTree+'/'+words[0]
    outputPath = HeadOfModularITKTree+'/'+desPath
    if len(moduleList) == 0:
       moduleList.append(moduleName)
    elif moduleName != moduleList[-1]:
       moduleList.append(moduleName)

    # copying files to the destination
    if  os.path.isfile(inputfile):
       # creat the path
       if not os.path.isdir(outputPath):
          os.makedirs(outputPath)
       os.system('mv ' +inputfile+'  '+ outputPath)
    else:
       missingFileName = inputfile.split(HeadOfTempTree+'/')[1]
       missingf.write(missingFileName + '\n')
       numOfMissingFiles = numOfMissingFiles + 1

missingf.close()


# generate the modules list
moduleList = modulizerHelper.unique(moduleList)

# list the new files
newf =  open(LogDir+'/newFiles.log','w')
for (root, subDirs, files) in os.walk(HeadOfTempTree):
   for afile in files:
     newFilePath = os.path.join(root, afile)
     newFileName = newFilePath.split(HeadOfTempTree+'/')[1]
     newf.write(newFileName+'\n')
newf.close()
print ("listed new files to"+LogDir+"/newFiles.log")




#----------------------------------------------------------------------------------------------------
# create CMake codes for each module
print ('creating cmake files for each module (from the template module)')
for  moduleName in moduleList:
  modulePath = modulizerHelper.searchModulePathTable(moduleName)
  filepath = HeadOfModularITKTree + '/'+ modulePath +'/itk-module.cmake'
  if os.path.isfile(filepath):
     # read dependency list from  itk-module.cmake
     o = open(filepath,'r')
     line = o.read()
     # parse the syntax, e.g. itk_module(itk-io-tiff DEPENDS itk-tiff itk-io-base)
     dependsModuleList = (((line.split('(')[1]).split(')')[0]).split())[2:]

     dependsModuleLibrariesList =""
     for dependsModule in dependsModuleList:
         dependsModuleLibrariesList = dependsModuleLibrariesList+ " ${"+dependsModule+"_LIBRARIES}"

      # write CMakeLists.txt
     filepath = HeadOfModularITKTree + '/'+ modulePath +'/CMakeLists.txt'
     if not os.path.isfile(filepath):
       o = open(filepath,'w')
       o.write('cmake_minimum_required(VERSION 2.8 FATAL_ERROR)\n')
       o.write('project('+moduleName+')\n')
       o.write('find_package(itk-base 4.0 EXACT REQUIRED)\n')
       if os.path.isdir(HeadOfModularITKTree + '/'+ modulePath +'/src'):
          o.write('set('+moduleName+'_LIBRARIES ' +moduleName +')\n')
       o.write('itk_module_impl()')
       o.close()


     # write src/CMakeLists.txt
     # list of CXX files
     if os.path.isdir(HeadOfModularITKTree + '/'+ modulePath +'/src'):
       cxxFiles = []
       cxxFiles = glob.glob(HeadOfModularITKTree + '/'+ modulePath +'/src/*.cxx')
       cxxFiles.extend(glob.glob(HeadOfModularITKTree + '/'+ modulePath +'/src/*.c'))
       cxxFileList='';
       for cxxf in cxxFiles:
            cxxFileList = cxxFileList+cxxf.split('/')[-1]+'\n'
       filepath = HeadOfModularITKTree + '/'+ modulePath +'/src/CMakeLists.txt'
       if not os.path.isfile(filepath):
         o = open(filepath,'w')
         for line in open('./templateModule/itk-template-module/src/CMakeLists.txt','r'):
            line = line.replace('itk-template-module',moduleName)
            line = line.replace('LIST_OF_CXX_FILES',cxxFileList[0:-1]) #get rid of the last \n
            line = line.replace('@DEPEND_MODULE_LIBRARIES@',dependsModuleLibrariesList) #get rid of the last \n
            o.write(line);
         o.close()


     # write  test/CMakeLists.txt
     if os.path.isdir(HeadOfModularITKTree + '/'+ modulePath +'/test'):
       cxxFiles = glob.glob(HeadOfModularITKTree + '/'+ modulePath +'/test/*.cxx')
       cxxFileList='';
       for cxxf in cxxFiles:
            cxxFileList = cxxFileList+cxxf.split('/')[-1]+'\n'
       filepath = HeadOfModularITKTree + '/'+ modulePath +'/test/CMakeLists.txt'

       if not os.path.isfile(filepath):
         o = open(filepath,'w')
         line = 'create_test_sourcelist(Tests '+moduleName+'-tests.cxx\n'+cxxFileList+')\n\n'
         o.write(line)

         #line = 'set (TestsTorun ${Tests})\nremove(TestsToRun '+moduleName+'-tests.cxx)\n\n'
         #o.write(line)

         line = 'add_executable('+moduleName+'-tests  ${Tests} )\n'
         o.write(line)

         line = 'target_link_libraries('+moduleName+'-tests  ${'+moduleName+'_LIBRARIES} )\n\n'
         o.write(line)

         #line = 'set('+ moduleName+'_TESTS'+ '  ${ITK_EXECUTABLE_PATH}/'+moduleName+'-tests)\n'
         #o.write(line)
         for cxxf in cxxFiles:
            cxxFileName = cxxf.split('/')[-1]
            executableName = cxxFileName[0:-4];
            line = modulizerHelper.ModularITKAddTest(executableName, moduleName)
            #line = 'add_test(NAME ' +  '\n      COMMAND '+moduleName+'-tests  ' + executbaleName +')\n\n'
            o.write(line)
         o.close()


     # write CTestConfig.cmake
     filepath = HeadOfModularITKTree + '/'+ modulePath +'/CTestConfig.cmake'
     if not os.path.isfile(filepath):
        o = open(filepath,'w')
        for line in open('./templateModule/itk-template-module/CTestConfig.cmake','r'):
            line = line.replace('itk-template-module',moduleName)
            o.write(line);
        o.close()

     # copy the LICENSE and NOTICE
     os.system('cp ./templateModule/itk-template-module/LICENSE'+'  '+ HeadOfModularITKTree + '/'+ modulePath )
     os.system('cp ./templateModule/itk-template-module/NOTICE'+'  '+ HeadOfModularITKTree + '/'+ modulePath )

#----------------------------------------------------------------------------------------------------
#clean up the temporary  directory
if os.path.isdir(HeadOfTempTree):
   os.system("rm -Rf "+ HeadOfTempTree)
