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
#  Example on the use of the SigmoidImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderIUC2_New ]
set writer [ itkImageFileWriterIUC2_New ]

set filter [ itkSigmoidImageFilterIUC2IUC2_New ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

$filter SetOutputMinimum [expr [lindex $argv 2]]
$filter SetOutputMaximum [expr [lindex $argv 3]]

$filter SetAlpha  [expr [lindex $argv 4]]
$filter SetBeta   [expr [lindex $argv 5]]

$writer Update


exit
