#
#  Example on the use of the BinaryThresholdImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderIUS2_New ]
set writer [ itkImageFileWriterIUS2_New ]

set filter [ itkBinaryThresholdImageFilterIUS2IUS2_New ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

$filter SetLowerThreshold [expr [lindex $argv 2]]
$filter SetUpperThreshold [expr [lindex $argv 3]]

$filter SetOutsideValue [expr [lindex $argv 4]]
$filter SetInsideValue  [expr [lindex $argv 5]]

$writer Update


exit

