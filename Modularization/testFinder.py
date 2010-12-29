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
# This script is used to find the tests corresponding to classes listed
# in the Manifest.txt file.

#
# io run it, type ./testfinder.py   ITK_SOURCE_TREE
#
# from the directory where the Manifest.txt file is.
# an output file called ManifestTests.txt will be generated.
#

import glob
import sys
import os.path

if len(sys.argv) != 2:
    print("USAGE:  {0} [monolithic ITK PATH] ".format(sys.argv[0]))
    sys.exit(-1)


HeadOfITKTree = sys.argv[1];
if (HeadOfITKTree[-1] == '/'):
    HeadOfITKTree = HeadOfITKTree[0:-1]


testFiles = glob.glob(HeadOfITKTree+'/Testing/Code/*/*.cxx')

testmanifest =  open('./ManifestOfITKTests.txt','w')
print('created ./ManifestOfITKTests.txt')

testmanifest.write('# Testing code \n')

for line in open("./Manifest.txt",'r'):
  if line[0]!='#':
    words = line.split()
    inputfile = words[0]
    group = words[1]
    module = words[2]
    destinationSubdir = words[3]
    if destinationSubdir == 'src' or destinationSubdir == 'include':
      basepath, basefilename = os.path.split(inputfile)
      basename, extension = os.path.splitext(basefilename)
      basenametest = basename+'Test'
      for testfilename in testFiles:
        index = testfilename.find( basenametest )
        if index != -1:
          testFileSplit=testfilename.split(HeadOfITKTree)
          testmanifest.write('.'+testFileSplit[1]+'\t'+group+'\t'+module+'\t'+'test'+'\n')

testmanifest.close()
