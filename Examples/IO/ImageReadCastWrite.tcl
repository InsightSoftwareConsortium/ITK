#
#  Example on the use of the Reader, 
#  Writer and RescaleIntensity filters
#

set reader [ itk::create ImageFileReaderUS2 ]
set writer [ itk::create ImageFileWriterUS2 ]

set filter [ itk::create RescaleIntensityImageFilterUS2US2 ]

$filter     SetInput [ $reader     GetOutput ]
$writer     SetInput [ $filter     GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

set minimum [expr [lindex $argv 2]]
set maximum [expr [lindex $argv 3]]

$filter SetOutputMinimum   minimum
$filter SetOutputMaximum   maximum

$writer Update


