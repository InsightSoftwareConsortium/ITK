package require InsightToolkit
package require itkinteraction

set inputFileName  "/home/ibanez/data/Vessels/Synthetic1.png"
set outputFileName "/home/ibanez/data/Vessels/Synthetic1laplacian.png"

# Setup pipeline.

set reader     [itk::create ImageFileReaderF2]
set laplacian  [itk::create LaplacianImageFilterF2F2]
set rescaler   [itk::create RescaleIntensityImageFilterF2US2]
set writer     [itk::create ImageFileWriterUS2]


$laplacian SetInput [$reader     GetOutput]
$rescaler  SetInput [$laplacian  GetOutput]
$writer    SetInput [$rescaler   GetOutput]

$rescaler SetOutputMinimum 0
$rescaler SetOutputMaximum 65535

$reader SetFileName $inputFileName  
$writer SetFileName $outputFileName 

$writer SetImageIO [itk::create PNGImageIO]

$writer Update
