#
#  Example on the use of the GradientAnisotropicDiffusionImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderUS2_New ]
set writer [ itkImageFileWriterUS2_New ]

set inputCast  [ itkCastImageFilterUS2F2_New    ]
set outputCast [ itkRescaleIntensityImageFilterF2US2_New ]

set filter [ itkGradientAnisotropicDiffusionImageFilterF2F2_New ]

$inputCast  SetInput [ $reader     GetOutput ]
$filter     SetInput [ $inputCast  GetOutput ]
$outputCast SetInput [ $filter     GetOutput ]
$writer     SetInput [ $outputCast GetOutput ]


$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]


$outputCast SetOutputMinimum       0
$outputCast SetOutputMaximum   65535

set numberOfIterations [expr [lindex $argv 2]]
set timeStep           [expr [lindex $argv 3]]
set conductance        [expr [lindex $argv 4]]


$filter SetNumberOfIterations     $numberOfIterations
$filter SetTimeStep               $timeStep
$filter SetConductanceParameter   $conductance


$writer Update


exit

