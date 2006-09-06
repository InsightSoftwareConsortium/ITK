#
#  Example on the use of the BinaryDilateImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderIUS2_New ]

set filter [ itkBinaryDilateImageFilterIUS2IUS2SE2_New ]

set cast [ itkCastImageFilterIUS2IUC2_New ]

set writer [ itkImageFileWriterIUC2_New ]

$filter     SetInput [ $reader  GetOutput ]
$cast     SetInput [ $filter  GetOutput ]
$writer     SetInput [ $cast  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

itkSize2 radius
radius Fill 5
set element [ itkFlatStructuringElement2_Ball radius ]

$filter SetKernel $element 
$filter SetDilateValue 200

$writer Update


exit

