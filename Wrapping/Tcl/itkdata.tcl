#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    itkdata.tcl
#  Language:  C++
#  Date:      $Date$
#  Version:   $Revision$
#
#  Copyright (c) 2002 Insight Consortium. All rights reserved.
#  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
#     This software is distributed WITHOUT ANY WARRANTY; without even 
#     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
#     PURPOSE.  See the above copyright notices for more information.
#

# Define ITK Tcl data utilities.
namespace eval itk::data {
  
  # Require the itk packages.
  package require InsightToolkit
  
  # Put the ITK_DATA_ROOT setting in the global namespace.
  
  # Look for the -D command line option.
  if {! [info exists ::ITK_DATA_ROOT] && [info exists argc]} {
    set argcm1 [expr $argc - 1]
    for {set i 0} {$i < $argcm1} {incr i} {
      if {[lindex $argv $i] == "-D" && $i < $argcm1} {
        set ::ITK_DATA_ROOT [lindex $argv [expr $i + 1]]
        break
      }
    }
  }
  
  # Check for the environment variable ::ITK_DATA_ROOT.
  if {! [info exists ::ITK_DATA_ROOT] && [info exists env(ITK_DATA_ROOT)]} {
    set ::ITK_DATA_ROOT $env(ITK_DATA_ROOT)
  }
  
  # Use the default data root.
  if {! [info exists ::ITK_DATA_ROOT]} {
    set ::ITK_DATA_ROOT $::itk::data::defaultDataRoot
  }
}
