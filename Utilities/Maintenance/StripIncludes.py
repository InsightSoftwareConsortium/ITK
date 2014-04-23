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
#==========================================================================*/

## This script is designed to asssit stripping out the unecessary header includes
## for all modules.

## To run the script you need to prepare a filelist.txt that list all the  .cxx
## and .hxx files you would like to process. You also need to set all the inputs
## inside this script.


## Author: Pat Marion.
## Modified by Xiaoxiao Liu.

########################   Input: need edit ###########################
sourceDir = "src/ITK" #source tree
buildDir  = "bin/ITK" #binary tree
relativeFileList = "filelist.txt" # files to process
includesToSkip = ["itkVersion.h","<cstring>", "<iostream>", "<fstream>","vnl/vnl_math.h","<string>","itkConfigure.h","<stdlib>","<time.h>"] #keep those headers
#######################################################################

from __future__ import print_function

import os

def tryCompile(fileName):
    # Use -B so that the target is always rebuilt
    return os.system("make -B %s.o" % fileName)

def writeFile(lines, fileName):

    f = open(fileName, 'w')
    for line in lines:
        f.write(line)
        f.write("\n")

def removeLines(lines, removedLines):
    newLines = []
    for i, line in enumerate(lines):
        if i in removedLines: continue
        newLines.append(line)
    return newLines

def shouldSkipInclude(line):
    for includeFile in includesToSkip:
        if includeFile in line: return True
    return False

def checkIfDef(line, ifDefCounter):
    if line.startswith("#ifdef") or line.startswith("if defined"):
        return ifDefCounter + 1
    elif line.startswith("#endif") and ifDefCounter > 0:
        return ifDefCounter - 1
    return ifDefCounter

def processFile(directory, fileName):

    absFileName = "/".join([sourceDir, directory, fileName])
    lines = open(absFileName, 'r').read().splitlines()
    removedLines = []
    ifDefCounter = 0
    for i, line in enumerate(lines):

        ifDefCounter = checkIfDef(line, ifDefCounter)
        if ifDefCounter > 0: continue
        if line.startswith('#include'):
            if shouldSkipInclude(line): continue

            print("Try remove:", line)
            lines[i] = ""

            writeFile(lines, absFileName)
            returnCode = tryCompile(fileName)
            if returnCode == 0:
                removedLines.append(i)
            else:
                print("Restoring:", line)
                lines[i] = line

    # Write final changes to file
    lines = removeLines(lines, removedLines)
    writeFile(lines, absFileName)


def processDirectory(directory, directoryFileList):

    makeDir = buildDir + "/" + directory
    try: os.chdir(makeDir)
    except: return

    for filename in directoryFileList:
        processFile(directory, filename)

def getFilesByDirectory(fileList):

    filesByDirectory = dict()
    for filename in fileList:
        filepath, filename = os.path.split(filename)
        if not filepath in filesByDirectory:
            filesByDirectory[filepath] = list()
        filesByDirectory[filepath].append(filename)
    return filesByDirectory

def processFileList(fileList):

    filesByDirectory = getFilesByDirectory(fileList)
    for directory, directoryFileList in filesByDirectory.iteritems():
        processDirectory(directory, directoryFileList)


def main():

    fileList = open(relativeFileList, 'r').read().splitlines()
    processFileList(fileList)


if __name__ == "__main__":
    main()
