#
#  Example on the use of the CurvatureFlowImageFilter
#

set reader [ itk::create ImageFileReaderUS2 ]
set writer [ itk::create ImageFileWriterUS2 ]

set inputCast  [ itk::create CastImageFilterUS2F2    ]
set outputCast [ itk::create RescaleIntensityImageFilterF2US2 ]

set filter [ itk::create CurvatureFlowImageFilterF2F2 ]

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

$filter SetNumberOfIterations  $numberOfIterations
$filter SetTimeStep            $timeStep


$writer Update


