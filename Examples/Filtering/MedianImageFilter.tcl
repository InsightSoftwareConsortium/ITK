#
#  Example on the use of the MedianImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderUS2_New ]
set writer [ itkImageFileWriterUS2_New ]

set filter [ itkMedianImageFilterUS2US2_New ]

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

