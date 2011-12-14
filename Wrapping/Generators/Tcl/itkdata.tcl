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
