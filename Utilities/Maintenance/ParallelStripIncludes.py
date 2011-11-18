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
# Author: Pat Marion
# Modified by Xiaoxiao Liu.

## This script is designed to run StriIncludes.py in parallel to significantly reduce
## the processing time for removing unnecessary header includes.

## To run the script you need to edit StripIncludes.py.
## You should also set up the number of processes according to the machine configuration.


from StripIncludes import *
from multiprocessing import Pool

###############  Inputs: need edit  #############
FILES_PER_PROCESS = 2
NUMBER_OF_PROCESSES = 8
#################################################

def main():

    fileList = open(relativeFileList, 'r').read().splitlines()

    args = []
    for i in xrange(0, len(fileList), FILES_PER_PROCESS):
        args.append(fileList[i:i+FILES_PER_PROCESS])

    pool = Pool(processes=NUMBER_OF_PROCESSES)
    pool.map(processFileList, args)


if __name__ == "__main__":
    main()
