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


def searchModulePathTable(ModuleName):
   for line in open('./ModulePathTable.txt','r'):
        w = line.split()
        if (w[1] == ModuleName):
             return  (w[0]+'/'+w[2])
   return ('Not Found'+ModuleName+'and'+ w[1])

# find the unique module names
def unique(seq):
    # not order preserving
    set = {}
    map(set.__setitem__, seq, [])
    return set.keys()
