#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    itkdata.py
#  Language:  C++
#  Date:      $Date$
#  Version:   $Revision$
#
#  Copyright (c) Insight Software Consortium. All rights reserved.
#  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
#     This software is distributed WITHOUT ANY WARRANTY; without even 
#     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
#     PURPOSE.  See the above copyright notices for more information.
#


import itkbase
import sys
import os

# Put the ITK_DATA_ROOT setting in the global namespace.  This
# package is only used for testing, so this is okay.
  
ITK_DATA_ROOT = ""

# Look for the -D command line option.
if not ITK_DATA_ROOT:
  for a in range(len(sys.argv)):
    if sys.argv[a] == "-D" and a < len(sys.argv):
      ITK_DATA_ROOT = sys.argv[a+1]
      break

# Check for the environment variable ::ITK_DATA_ROOT.
if not ITK_DATA_ROOT and os.environ.has_key('ITK_DATA_ROOT'):
  ITK_DATA_ROOT = os.environ['ITK_DATA_ROOT']

  
# Use the default output directory.
if not ITK_DATA_ROOT:
  ITK_DATA_ROOT = itkbase.defaultDataRoot


