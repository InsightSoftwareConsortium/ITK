#
#  Example on the use of the MeanImageFilter
#

set reader [ itkImageFileReaderUS2_New ]
set writer [ itkImageFileWriterUS2_New ]

set filter [ itkMeanImageFilterUS2US2_New ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

itkSize2 sizeRadius 
sizeRadius SetElement  0  [expr [lindex $argv 2]]
sizeRadius SetElement  1  [expr [lindex $argv 3]]

$filter SetRadius  sizeRadius 

$writer Update


