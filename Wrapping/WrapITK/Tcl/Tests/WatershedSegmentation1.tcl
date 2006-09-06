#
#  Example on the use of the WatershedSegmentation.
#
package require InsightToolkit

set reader [ itkImageFileReaderF2_New ]
$reader SetFileName [lindex $argv 0]

set diffusion [ itkGradientAnisotropicDiffusionImageFilterF2F2_New ]

$diffusion  SetInput   [ $reader  GetOutput ]

$diffusion  SetTimeStep 0.0625
$diffusion  SetConductanceParameter 9.0
$diffusion  SetNumberOfIterations 5

set gradient [ itkGradientMagnitudeImageFilterF2F2_New ]
$gradient  SetInput   [ $diffusion  GetOutput ]

set watershed [ itkWatershedImageFilterF2_New ]
$watershed  SetInput   [ $gradient GetOutput ]

$watershed  SetThreshold 0.01
$watershed  SetLevel 0.2


set writer [ itkImageFileWriterUL2_New ]

$writer SetInput [ $watershed  GetOutput ]
$writer SetFileName [lindex $argv 1]

$writer Update

exit

