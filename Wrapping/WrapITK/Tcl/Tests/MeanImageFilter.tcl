#
#  Example on the use of the MeanImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderIUS2_New ]
set writer [ itkImageFileWriterIUS2_New ]

set filter [ itkMeanImageFilterIUS2IUS2_New ]

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

