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
#  Example on the use of the MeanImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderIUC2_New ]
set writer [ itkImageFileWriterIUC2_New ]

set filter [ itkMeanImageFilterIUC2IUC2_New ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

set radius [expr [ lindex $argv 2] ]

itkSize2 sizeRadius
sizeRadius SetElement  0  $radius
sizeRadius SetElement  1  $radius

$filter SetRadius  sizeRadius

$writer Update


exit
