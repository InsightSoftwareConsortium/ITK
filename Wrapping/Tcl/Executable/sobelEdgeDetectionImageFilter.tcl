package require InsightToolkit
package require itkinteraction

set inputFileName  "/home/ibanez/data/Vessels/Synthetic1.png"
set outputFileName "/home/ibanez/data/Vessels/Synthetic1sobel.png"

# Setup pipeline.

set reader   [itk::create ImageFileReaderF2]
set sobel    [itk::create SobelEdgeDetectionImageFilterF2F2]
set rescaler [itk::create RescaleIntensityImageFilterF2US2]
set writer   [itk::create ImageFileWriterUS2]


$sobel     SetInput [$reader    GetOutput]
$rescaler  SetInput [$sobel     GetOutput]
$writer    SetInput [$rescaler  GetOutput]

$rescaler SetOutputMinimum 0
$rescaler SetOutputMaximum 65535

$reader SetFileName $inputFileName  
$writer SetFileName $outputFileName 

$writer SetImageIO [itk::create PNGImageIO]

$writer Update
