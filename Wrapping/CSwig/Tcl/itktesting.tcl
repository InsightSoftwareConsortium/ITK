#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    itktesting.tcl
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

# Define ITK Tcl testing utilities.
namespace eval itk::testing {
  
  # Require the itk packages.
  package require InsightToolkit
  package require itkdata
  
  # Put the ITK_TEST_ROOT setting in the global namespace.  This
  # package is only used for testing, so this is okay.
  
  # Look for the -T command line option.
  if {! [info exists ::ITK_TEST_ROOT] && [info exists argc]} {
    set argcm1 [expr $argc - 1]
    for {set i 0} {$i < $argcm1} {incr i} {
      if {[lindex $argv $i] == "-T" && $i < $argcm1} {
        set ::ITK_TEST_ROOT [lindex $argv [expr $i + 1]]
        break
      }
    }
  }
  
  # Check for the environment variable ::ITK_TEST_ROOT.
  if {! [info exists ::ITK_TEST_ROOT] && [info exists env(ITK_TEST_ROOT)]} {
    set ::ITK_TEST_ROOT $env(ITK_TEST_ROOT)
  }
  
  # Use the default output directory.
  if {! [info exists ::ITK_TEST_ROOT]} {
    set dtr $::itk::testing::defaultTestRoot
    if {$dtr == "<NO_DEFAULT>"} {
      error "Set ITK_TEST_ROOT or use -T option to specify."
    } else {
      set ::ITK_TEST_ROOT $dtr
    }
  }

  # Setup testing directories.
  set ::ITK_TEST_BASELINE "${::ITK_DATA_ROOT}/Baseline"
  set ::ITK_TEST_INPUT "${::ITK_DATA_ROOT}/Input"
  set ::ITK_TEST_OUTPUT "${::ITK_TEST_ROOT}/Output"
  file mkdir "${::ITK_TEST_OUTPUT}"
}
