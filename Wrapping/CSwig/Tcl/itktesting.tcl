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
