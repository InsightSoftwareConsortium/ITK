package require InsightToolkit
package require itkinteraction


set inputFileName  "$env(ITK_DATA_ROOT)/Input/cthead1.png"
set outputFileName "$env(ITK_DATA_ROOT)/cthead1canny.png"

# Setup pipeline.

set reader   [itk::create ImageFileReaderF2]
set canny    [itk::create CannyEdgeDetectionImageFilterF2F2]
set rescaler [itk::create RescaleIntensityImageFilterF2US2]
set writer   [itk::create ImageFileWriterUS2]


$canny     SetInput [$reader    GetOutput]
$rescaler  SetInput [$canny     GetOutput]
$writer    SetInput [$rescaler  GetOutput]

$rescaler SetOutputMinimum 0
$rescaler SetOutputMaximum 65535

$reader SetFileName $inputFileName  
$writer SetFileName $outputFileName 

$writer SetImageIO [itk::create PNGImageIO]

$writer Update

