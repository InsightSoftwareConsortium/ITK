
INCLUDE_DIRECTORIES(
../../..
../../../Code/Common
../../../Code/Numerics
../../../Code/Numerics/vxl
../../../Code/Numerics/vxl/vcl
${CMAKE_SOURCE_DIR}/Code/BasicFilters
)

LINK_DIRECTORIES(
../../../Code/Numerics/vxl
../../../Code/Common
${CMAKE_BINARY_DIR}/Code/BasicFilters
)

LINK_LIBRARIES (
ITKBasicFilters 
VXLNumerics
ITKCommon
dl
pthread
)

TESTS(
itkBasicArchitectureTest
itkCovariantVectorGeometryTest
itkCurvatureAnisotropicDiffusionImageFilterTest
itkCyclicReferences
itkFilterDispatchTest
itkFilterImageAddTest
itkFilterImageGradientMagnitudeTest
itkFilterImageSingleOperatorTest
itkBasicFiltersHeaderTest
itkImageAdaptorPipeLineTest
itkImportImageTest
itkShrinkImageTest
itkThresholdImageFilterTest
itkVectorAnisotropicDiffusionImageFilterTest
)
