#
#  Example on the use of the BinaryDilateImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderUC2_New ]
set writer [ itkImageFileWriterUC2_New ]

set filter [ itkBinaryDilateImageFilterUC2UC2_New ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

itkBinaryBallStructuringElementUC2  element 

element  SetRadius 1
element  CreateStructuringElement

$filter SetKernel  element 
$filter SetDilateValue 255

$writer Update


exit

