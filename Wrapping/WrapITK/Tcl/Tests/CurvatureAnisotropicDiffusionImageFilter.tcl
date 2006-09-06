#
#  Example on the use of the CurvatureAnisotropicDiffusionImageFilter
#
package require InsightToolkit

set reader [ itkImageFileReaderIF2_New ]
set filter [ itkCurvatureAnisotropicDiffusionImageFilterIF2IF2_New ]
set outputCast [ itkRescaleIntensityImageFilterIF2IUC2_New ]
set writer [ itkImageFileWriterIUC2_New ]

$filter     SetInput [ $reader  GetOutput ]
$outputCast SetInput [ $filter     GetOutput ]
$writer     SetInput [ $outputCast GetOutput ]


$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]


$outputCast SetOutputMinimum       0
$outputCast SetOutputMaximum   255

set numberOfIterations [expr [lindex $argv 2]]
set timeStep           [expr [lindex $argv 3]]
set conductance        [expr [lindex $argv 4]]


$filter SetNumberOfIterations     $numberOfIterations
$filter SetTimeStep               $timeStep
$filter SetConductanceParameter   $conductance


$writer Update


exit

