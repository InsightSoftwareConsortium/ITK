#
#  Example on the use of the VoronoiSegmentationImageFilter.
#
package require InsightToolkit

set readerInput [ itkImageFileReaderUC2_New ]
set readerPrior [ itkImageFileReaderUC2_New ]

$readerInput SetFileName [lindex $argv 0]
$readerPrior SetFileName [lindex $argv 1]

$readerInput Update
$readerPrior Update


set filter [ itkVoronoiSegmentationImageFilterUC2UC2UC2_New ]

$filter     SetInput   [ $readerInput  GetOutput ]
$filter     TakeAPrior [ $readerPrior  GetOutput ]

$filter SetMeanPercentError [lindex $argv 3]
$filter SetSTDPercentError  [lindex $argv 4]


set writer [ itkImageFileWriterUC2_New ]

$writer SetInput [ $filter  GetOutput ]
$writer SetFileName [lindex $argv 2]

$writer Update

exit

