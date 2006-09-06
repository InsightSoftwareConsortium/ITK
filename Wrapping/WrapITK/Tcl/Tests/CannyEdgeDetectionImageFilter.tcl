#
#  Example on the use of the CannyEdgeDetection filter
#
package require InsightToolkit

set reader [ itkImageFileReaderIF2_New ]
set writer [ itkImageFileWriterIUC2_New ]

set outputCast [ itkRescaleIntensityImageFilterIF2IUC2_New ]

set filter [ itkCannyEdgeDetectionImageFilterIF2IF2_New ]

$filter     SetInput [ $reader     GetOutput ]
$outputCast SetInput [ $filter     GetOutput ]
$writer     SetInput [ $outputCast GetOutput ]


$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]


$outputCast SetOutputMinimum     0
$outputCast SetOutputMaximum   255

set variance   [expr [lindex $argv 2]]
set threshold  [expr [lindex $argv 3]]

$filter SetVariance     $variance
$filter SetThreshold    $threshold


$writer Update


exit

