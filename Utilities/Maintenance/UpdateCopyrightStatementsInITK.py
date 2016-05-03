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

#
# \author Hans J. Johnson
#
# This script is designed to help change the copyright notices in all ITK files to a common format.
# For files that are .h, .cxx, .hxx, .c, if there is no other copyright information, add the itkCopyright.

from __future__ import print_function

import re
import sys
import os

## New license as specified on: https://itk.org/Wiki/ITK_Release_4/Licensing
NewITKCopyrightNotice="""/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
"""
NewVTKDependantCopyrightNotice="""/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
"""

## Patterns that match the old copyright notice sections
## ITK only copyright
ITKOnlyOldHeader=""" */\* *==.*Program:.*Insight Segmentation & Registration Toolkit.*Copyright .* Insight.*Consortium. All rights reserved.*See ITKCopyright.txt or https://www.itk.org/HTML/Copyright.htm for details.[\n\r ]*This software is distributed WITHOUT ANY WARRANTY; without even.*the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR.*PURPOSE.  See the above copyright notices for more information.*=== *\*/[\n\r ]*"""
ITKOnlyOldRE=re.compile(ITKOnlyOldHeader,re.MULTILINE|re.DOTALL|re.IGNORECASE)

## Files that originated in VTK, and now have ITK also
ITKVTKOldHeader=""" */\* *==.*Program:.*Insight Segmentation & Registration Toolkit.*Copyright .* Insight Software Consortium. All rights reserved.*See ITKCopyright.txt or https://www.itk.org/HTML/Copyright.htm for details.[\n\r ]*.*VTKCopyright.txt.*This software is distributed WITHOUT ANY WARRANTY; without even.*the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR.*PURPOSE.  See the above copyright notices for more information.*=== *\*/[\n\r ]*"""
ITKVTKOldRE=re.compile(ITKVTKOldHeader,re.MULTILINE|re.DOTALL|re.IGNORECASE)

## Looking for new files.
NewITKHeader=""" */\* *==.*http://www.apache.org/licenses/LICENSE-2.0.txt.*=== *\*/"""
NewITKHeaderRE=re.compile(NewITKHeader,re.MULTILINE|re.DOTALL|re.IGNORECASE)

eolSpaceRemove=re.compile(r'  *$',re.MULTILINE)

## The exception list contains files that should not have the ITK copyright notices added.
ExclusionList=['Utilities','.git']

ExtensionsThatNeedCopyright=['.cxx','.c','.h','.hxx']


############
############
############
############
############
############
############

if len(sys.argv) != 2:
    print("USAGE:  {0} <Top of ITK tree to process>".format(sys.argv[0]))
    sys.exit(-1)

HeadOfITKTree=sys.argv[1]

for top,directory,files in os.walk(HeadOfITKTree):
    ## First remove Excluded directories
    for dd in directory:
      if dd[0] == '.': #Skip all directories that begin with '.'
        directory.remove(dd)
        continue
      if dd in ExclusionList:
          directory.remove(dd)
          continue
    ##  Now process each file
    for ff in files:
      if ff in ExclusionList:
        files.remove(ff)
        continue
      if ff[0] == '.': #Skip all files that begin with '.'
        files.remove(ff)
        #print("@@@@@@@",ff)
        continue
      currFile=os.path.join(top,ff)
      print(currFile)

      infile=open(currFile,'r')
      file_text=infile.read()
      newstring=file_text # default output to input, just in case all search patterns fail
      infile.close()

      substitutionMade=0
      testITKOnlySearch=ITKOnlyOldRE.search(file_text)
      if testITKOnlySearch:
        print("{0} is ITKOnlyHeader".format(currFile))
        newstring=ITKOnlyOldRE.sub(NewITKCopyrightNotice,file_text)
        newstring=eolSpaceRemove.sub("",newstring) ## a few files still have eol spaces
        substitutionMade=1

      testITKVTKSearch=ITKVTKOldRE.search(file_text)
      if testITKVTKSearch:
        print("{0} is VTKITKHeader".format(currFile))
        newstring=ITKVTKOldRE.sub(NewITKCopyrightNotice+NewVTKDependantCopyrightNotice,file_text)
        newstring=eolSpaceRemove.sub("",newstring) ## a few files still have eol spaces
        substitutionMade=1

      ##Add new copyright if it had not already existed.
      root,ext=os.path.splitext(currFile)
      if ext in ExtensionsThatNeedCopyright:
        testNewITKHeaderRE=NewITKHeaderRE.search(file_text) # see if new CopyRight notice already exists.
        if testNewITKHeaderRE:
          print("Already Processed {0}".format(currFile))
        elif (substitutionMade == 0):
          print("{0} needed copyright header.".format(currFile))
          newstring=NewITKCopyrightNotice+file_text
          newstring=eolSpaceRemove.sub("",newstring) ## a few files still have eol spaces

      outfile=open(currFile,'w')
      outfile.write(newstring)
      outfile.close()
