#
#  Example on the use of the CastImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderUC2_New ]
set writer [ itkImageFileWriterUS2_New ]

set filter [ itkCastImageFilterUC2US2_New ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

$writer Update


exit


