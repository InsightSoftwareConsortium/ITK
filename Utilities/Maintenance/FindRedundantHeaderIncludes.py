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


## Author: Hans J. Johnson
## This script is designed to find redundant header files
## For a given file, if a sibling include file has a
## grand-child include file that is the same, then
## you can remove the sibling include.

## in the ITK/Code directory issue the following command
from __future__ import print_function

import os
import sys
import re
import collections

class FileToPathMapping:
    def __init__(self):
        self.filePathBaseDirs=dict()
        self.DependUponTree_dict=collections.defaultdict(list)

    def getBaseDirs(self):
        return self.filePathBaseDirs


    def FillFromWalkingTree(self,basedir):
        os.path.walk(basedir,makeGlobalMapping,self.filePathBaseDirs)
        for all_files in self.filePathBaseDirs:
           testfile=self.filePathBaseDirs[all_files]+'/'+all_files
           curr_file=os.path.basename(testfile)
           curr_dir=os.path.dirname(testfile)
           if not os.path.isfile(testfile):
             continue
           #print(testfile)
           ff=open(testfile)
           search_string=r'^#include *([<"])(itk[^<"]*h)([>"])'
           myregexp=re.compile(search_string)
           for line in ff:
              gg=myregexp.match(line)
              if gg != None and ( len(gg.groups()) == 3 ):
                  inc=gg.group(2)
                  #make empty list if not found already
                  self.DependUponTree_dict[inc]
                  self.DependUponTree_dict[curr_file].append(inc)
                  #print("{0} in {1}".format(inc,curr_file))
           ff.close()
        return self.DependUponTree_dict

    def comment_out(self,filename,remove_header):
      """Get rid of include lines that are redundant"""
      ff=open(self.filePathBaseDirs[filename]+"/"+filename)
      outfile=open(self.filePathBaseDirs[filename]+"/"+filename+"_cleaned","w")
      for line in ff:
          if line.find(remove_header) != -1:
            print("          Removing {0} from {1}".format(line,self.filePathBaseDirs[filename]+"/"+filename))
          else:
            outfile.write(line)
      ff.close()
      outfile.close()
      os.rename(self.filePathBaseDirs[filename]+"/"+filename+"_cleaned",self.filePathBaseDirs[filename]+"/"+filename)

    def proc_children(self,node,dupcandidate,starting_child):
        ## Pocess all children
        isdone=donenode.get((starting_child,dupcandidate),None)
        if isdone != None:
          #print("found {0} {1}".format(starting_child,dupcandidate))
          return False
        else:
          #print("adding {0} {1}".format(starting_child,dupcandidate))
          donenode[(starting_child,dupcandidate)]=True

        nodeEdges=myDependTree[node]
        #print(nodeEdges)
        if ( dupcandidate not in nodeEdges): # Nothing to do
          print("-")
          return False
        elif (len(nodeEdges) == 0 ):  ## leaf node
          print("l")
          return False
        else:
          for currEdge in nodeEdges:
            if dupcandidate in myDependTree[currEdge]:
              print("Remove {0} from {1}:  found hereditary same include in {2}".format(dupcandidate,starting_child,currEdge))
              ## Update Mapping to remove race condition where an include is removed
              ## after is is referenced as a hereditary source
              # print("{0} --> {1}".format(starting_child,myDependTree[starting_child]))
              temp=myDependTree[starting_child]
              temp.remove(dupcandidate)
              myDependTree[node]=temp
              # print("{0} --> {1}".format(starting_child,myDependTree[starting_child]))
              self.comment_out(starting_child,dupcandidate)
              return True
            else:
              #print("Processsng proc_childeren({0},{1},{2})".format(currEdge,dupcandidate,starting_child))
              self.proc_children(currEdge,dupcandidate,starting_child)
        return False

basedir=sys.argv[1]  ## i.e. python FindRedundantHeaderIncludes.py $HOME/Dashboards/ITK_TESTS/ITK
if os.path.isfile(basedir+"/Documentation/InsightLogo.gif"):  ## Currently hard-coded to only work with ITK dir.
  print("Processing: {0}".format(basedir))
else:
  print("The directory must be the base ITK dir: {0} failed".format(basedir))
  exit(-1)

mymapper=FileToPathMapping()
myDependTree=mymapper.FillFromWalkingTree(basedir)

#for parentFiles in myDependTree:
#    print(parentFiles)
#    for childFiles in myDependTree[parentFiles]:
#        print(" "*3+childFiles)
#print(mymapper.filePathBaseDirs)


donenode=dict()
distance=0

file_count=0
#process_file="itkMatrixOffsetTransformBase.h"
#process_file="itkConstNeighborhoodIterator.h"
#for process_file in ["itkImage.h"]:
for process_file in myDependTree.keys():
  for remove_test_file in myDependTree[process_file]:
    #print("Starting test for: {0} and {1}".format(process_file,remove_test_file))
    mymapper.proc_children(process_file,remove_test_file,process_file)
    file_count+=1
print("Processed {0} files.".format(file_count))
