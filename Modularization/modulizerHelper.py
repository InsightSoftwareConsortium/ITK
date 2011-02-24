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
# This script stores useful functions for running  modulariizer.py
import os
import sys
import shutil

def copy_directory(source, target):
    if not os.path.exists(target):
        os.mkdir(target)
    for root, dirs, files in os.walk(source):
        if '.git' in dirs:
            dirs.remove('.git')
        for file in files:
            if os.path.splitext(file)[-1] in ('.git*'):
               continue
            from_ = os.path.join(root, file)
            to_ = from_.replace(source, target, 1)
            to_directory = os.path.split(to_)[0]
            if not os.path.exists(to_directory):
                os.makedirs(to_directory)
            shutil.copyfile(from_, to_)


def formAddTestCMD( testName, testDriverName, executableName, argns):
      line = "add_test(NAME "+ testName+ "\n      COMMAND "+testDriverName+' ' +executableName

      if (argns !=""):
           line = line + "\n              "+argns

      line = line +")\n"
      return line



def ModularITKAddTest(executableSearch, moduleName):
    addTestLines="";
    testDriverName = moduleName+'TestDriver'
    o = open ("./tmp.txt",'w')
    for line in  open("./RemainingTests.txt",'r'):
      addTestCmd =""
      if(line[0] != '#'):
        argns=""
        words = line[0:-1].split(";")
        if (len(words) < 3):
            # No test driver, no arguments
            testName = words[0]
            executableName = words[1].split("/")[-1]
            argns=''
            if (executableName == executableSearch):
               addTestCmd =  formAddTestCMD(testName, testDriverName, executableName, argns)
        else:
            # test driver used
            testName = words[0]
            if (words[2] == "--compare"):
               # regression test
               baselineFileName = words[3]
               compareFileName = words[4]
               executableName = words[5]
               argns =' '.join(words[6:])
               if (executableName == executableSearch):
                    addTestCmd = 'add_test(NAME '+ testName+ '\n      COMMAND '+testDriverName+'\n' + '    --compare ' + baselineFileName + '\n' + '              '+compareFileName +'\n' +'    '+ executableName
                    if (argns !=""):
                         addTestCmd = addTestCmd + ' '+argns

                    addTestCmd = addTestCmd +")\n"
            else:
                # non regression test
                executableName = words[2]
                argns =' '.join(words[3:])
                if (executableName == executableSearch):
                   addTestCmd = formAddTestCMD(testName, testDriverName, executableName, argns)


            if (addTestCmd == ""):
            # try again, assume no test driver, but with arguments
               executableName = words[1].split("/")[-1]
               argns =' '.join(words[2:])
               if (executableName == executableSearch):
                   addTestCmd = formAddTestCMD(testName, testDriverName, executableName, argns)
        if addTestCmd == "":
            o.write(line)
        else: # found
            addTestLines = addTestLines + addTestCmd
    o.close()
    os.system('mv -f ./tmp.txt ./RemainingTests.txt')
    return  addTestLines


def searchModulePathTable(ModuleName):
   for line in open('./ModulePathTable.txt','r'):
        w = line.split()
        if (w[1] == ModuleName):
             return  (w[0]+'/'+w[2])
   return ('Not Found'+ModuleName)

# find the unique module names
def unique(seq):
    # not order preserving
    set = {}
    map(set.__setitem__, seq, [])
    return set.keys()
