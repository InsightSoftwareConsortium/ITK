#
#  Example on the use of the CannyEdgeDetection filter
#
package require InsightToolkit

set reader [ itkImageFileReaderF2_New ]
set writer [ itkImageFileWriterUC2_New ]

set outputCast [ itkRescaleIntensityImageFilterF2UC2_New ]

set filter [ itkCannyEdgeDetectionImageFilterF2F2_New ]

$filter     SetInput [ $reader     GetOutput ]
$outputCast SetInput [ $filter     GetOutput ]
$writer     SetInput [ $outputCast GetOutput ]


$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]


$outputCast SetOutputMinimum     0
$outputCast SetOutputMaximum   255

set variance   [expr [lindex $argv 2]]
set lowerThreshold  [expr [lindex $argv 3]]
set upperThreshold  [expr [lindex $argv 4]]

$filter SetVariance     $variance
$filter SetLowerThreshold    $lowerThreshold
$filter SetUpperThreshold    $upperThreshold

$writer Update

exit

