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


if len(sys.argv) < 3:
    print("USAGE:  "+sys.argv[0]+" [monolithic ITK PATH] [modular ITK PATH]")
    sys.exit(-1)

HeadOfITKTree = sys.argv[1];
if (HeadOfITKTree[-1] == '/'):
    HeadOfITKTree = HeadOfITKTree[0:-1]

HeadOfModularITKTree = sys.argv[2];
if (HeadOfModularITKTree[-1] ==  '/'):
    HeadOfModularITKTree = HeadOfModularITKTree[0:-1]


# clean up the dirs first
if os.path.isdir(HeadOfModularITKTree):
   if  len(sys.argv) > 3:
      if (sys.argv[3] == 'y'):
         answer = 'y'
   else:
      print('Warning: The directory '+HeadOfModularITKTree+' exists! It needs to be wiped out first.')
      answer = raw_input("Do you really want to clean up this directory? [y/n]: " )

   if (answer == 'y'):
       os.system("rm -Rf "+ HeadOfModularITKTree)
       print('removed '+HeadOfModularITKTree)
       # get the supporting modules and cmake packaing files
       cmd ='git clone http://itk.org/tmp/modularITKSupport.git '+HeadOfModularITKTree
       os.system(cmd)
       print("modularITKSupport repository cloned into " + HeadOfModularITKTree)
   if (answer =='n'):
       print("Please rerun the program with a different directory.")
       exit(-1)
   if (answer =='a'): #advanced
       print('Advanced Option Warning: This is only for developer\'s convinience.\nThe git clone step will be skipped; Old files in the directory will be overwritten.')
else:
   # get the supporting modules and cmake packaing files
   cmd ='git clone http://itk.org/tmp/modularITKSupport.git '+HeadOfModularITKTree
   os.system(cmd)
   print("modularITKSupport repository cloned into " + HeadOfModularITKTree)



# copy the whole ITK tree over to a tempery dir, except the data
HeadOfTempTree = HeadOfModularITKTree+'/ITK_remaining'

def copy_directory(source, target):
    if not os.path.exists(target):
        os.mkdir(target)
    for root, dirs, files in os.walk(source):
        if '.git' in dirs:
            dirs.remove('.git')
        if 'Data' in dirs:
            dirs.remove('Data')
        for file in files:
            if os.path.splitext(file)[-1] in ('.git*'):
               continue
            from_ = os.path.join(root, file)
            to_ = from_.replace(source, target, 1)
            to_directory = os.path.split(to_)[0]
            if not os.path.exists(to_directory):
                os.makedirs(to_directory)
            shutil.copyfile(from_, to_)

print("Start to copy" + HeadOfITKTree + " to " + HeadOfTempTree + "   ...")
copy_directory(HeadOfITKTree, HeadOfTempTree)
print("Done copying!")

LogDir=HeadOfModularITKTree+'/logs'
if not os.path.isdir(LogDir):
  os.makedirs(LogDir)

# read the manifest file
print ("moving files from "+HeadOfTempTree+" into modules in "+HeadOfModularITKTree)
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
    groupName = words[1]
    moduleName = words[2]
    subdirName = words[3]


    inputfile = HeadOfTempTree+'/'+words[0]
    outputPath = HeadOfModularITKTree+'/modules/'+ moduleName+'/'+subdirName
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

# find the unique module names
def unique(seq):
    # not order preserving
    set = {}
    map(set.__setitem__, seq, [])
    return set.keys()


moduleList = unique(moduleList)


# list the new files
newf =  open(LogDir+'/newFiles.log','w')
for (root, subDirs, files) in os.walk(HeadOfTempTree):
   for afile in files:
     newFilePath = os.path.join(root, afile)
     newFileName = newFilePath.split(HeadOfTempTree+'/')[1]
     newf.write(newFileName+'\n')
newf.close()
print ("listed new files to"+LogDir+"/newFiles.log")

###########################################################################
def ModularITKAddTest(executableSearch, moduleName):
    addTestLines="";
    for line in  open("./AddTestsWithArguments.txt",'r'):
      if(line[0] != '#'):
        argns=""
        words = line[0:-1].split(";")
        if (len(words) < 3):
            # No test driver , no arguments
            testName = words[0]
            # parse the executableName from the path
            executableName = words[1].split("/")[-1]
        else:
            testName = words[0]
            testDriver = words[1]
            if (words[2] == "--compare"):
               #ignore now, TO DO
               executableName = "--";
            else:
               executableName = words[2]
               argns =' '.join(words[3:])
            if (executableName != executableSearch):
                # try again , assuming no test driver
               executableName = words[1].split("/")[-1]
               argns =' '.join(words[2:])

        if (executableName == executableSearch):
            addTestLines = addTestLines + "add_test(NAME "+ testName+ "\n      COMMAND "+moduleName+'-tests  ' + executableName
            if (argns !=""):
               addTestLines = addTestLines + "\n              "+argns
            addTestLines = addTestLines +")\n"
    return  addTestLines


print ('creating cmake files for each module (from the template module)')
#moduleList = os.listdir(HeadOfModularITKTree)
for  moduleName in moduleList:
  if os.path.isdir(HeadOfModularITKTree+'/modules/'+moduleName):
     # cooy the LICENSE and NOTICE
     os.system('cp ./templateModule/itk-template-module/LICENSE'+'  '+ HeadOfModularITKTree+'/modules/'+moduleName)
     os.system('cp ./templateModule/itk-template-module/NOTICE'+'  '+ HeadOfModularITKTree+'/modules/'+moduleName)

     # write CMakeLists.txt
     filepath = HeadOfModularITKTree+'/modules/'+moduleName+'/CMakeLists.txt'
     if not os.path.isfile(filepath):
       o = open(filepath,'w')
       for line in open('./templateModule/itk-template-module/CMakeLists.txt','r'):
           line = line.replace('itk-template-module',moduleName)
           o.write(line);
       o.close()

     # write itk-module.cmake, which contains dependency info
     filepath = HeadOfModularITKTree+'/modules/'+moduleName+'/itk-module.cmake'
     if not os.path.isfile(filepath):
        o = open(filepath,'w')
        for line in open('./templateModule/itk-template-module/itk-module.cmake','r'):
            line = line.replace('itk-template-module',moduleName)
            o.write(line);
        o.close()
        dependsModuleLibrariesList = "${itk-common_LIBRARIES}"
     else:
        # read dependency list from  itk-module.cmake
        o = open(filepath,'r')
        line = o.read()
        # parse the syntax, e.g. itk_module(itk-io-tiff DEPENDS itk-tiff itk-io-base)
        dependsModuleList = (((line.split('(')[1]).split(')')[0]).split())[2:]

        dependsModuleLibrariesList =""
        for dependsModule in dependsModuleList:
            dependsModuleLibrariesList = dependsModuleLibrariesList+ " ${"+dependsModule+"_LIBRARIES}"


     # write src/CMakeLists.txt
     # list of CXX files
     if os.path.isdir(HeadOfModularITKTree+'/modules/'+moduleName+'/src'):
       cxxFiles = glob.glob(HeadOfModularITKTree+'/modules/'+moduleName+'/src/*.cxx')
       cxxFileList='';
       for cxxf in cxxFiles:
            cxxFileList = cxxFileList+cxxf.split('/')[-1]+'\n'
       filepath = HeadOfModularITKTree+'/modules/'+moduleName+'/src/CMakeLists.txt'
       if not os.path.isfile(filepath):
         o = open(filepath,'w')
         for line in open('./templateModule/itk-template-module/src/CMakeLists.txt','r'):
            line = line.replace('itk-template-module',moduleName)
            line = line.replace('LIST_OF_CXX_FILES',cxxFileList[0:-1]) #get rid of the last \n
            line = line.replace('@DEPEND_MODULE_LIBRARIES@',dependsModuleLibrariesList) #get rid of the last \n
            o.write(line);
         o.close()

     # write  test/CMakeLists.txt
     if os.path.isdir(HeadOfModularITKTree+'/modules/'+moduleName+'/test'):
       cxxFiles = glob.glob(HeadOfModularITKTree+'/modules/'+moduleName+'/test/*.cxx')
       cxxFileList='';
       for cxxf in cxxFiles:
            cxxFileList = cxxFileList+cxxf.split('/')[-1]+'\n'
       filepath = HeadOfModularITKTree+'/modules/'+moduleName+'/test/CMakeLists.txt'

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
            line = ModularITKAddTest(executableName, moduleName)
            #line = 'add_test(NAME '+ + '\n      COMMAND '+moduleName+'-tests  ' + executbaleName +')\n\n'
            o.write(line)
         o.close()


    # write CTestConfig.cmake
     filepath = HeadOfModularITKTree+'/modules/'+moduleName+'/CTestConfig.cmake'
     if not os.path.isfile(filepath):
        o = open(filepath,'w')
        for line in open('./templateModule/itk-template-module/CTestConfig.cmake','r'):
            line = line.replace('itk-template-module',moduleName)
            o.write(line);
        o.close()



#clean up the temporary  directory
if os.path.isdir(HeadOfTempTree):
   os.system("rm -Rf "+ HeadOfTempTree)
