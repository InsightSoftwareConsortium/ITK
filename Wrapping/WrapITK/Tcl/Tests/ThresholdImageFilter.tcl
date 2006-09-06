#
#  Example on the use of the ThresholdImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderIUS2_New ]
set writer [ itkImageFileWriterIUS2_New ]

set filter [ itkThresholdImageFilterIUS2_New ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

$filter SetOutsideValue [expr [lindex $argv 2]]
$filter ThresholdAbove  [expr [lindex $argv 3]]

$writer Update


exit

