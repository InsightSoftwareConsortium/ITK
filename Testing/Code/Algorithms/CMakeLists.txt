
INCLUDE_DIRECTORIES(
../../..
../../../Code/Common
../../../Code/Numerics
../../../Code/Numerics/vxl
../../../Code/Numerics/vxl/vcl
${CMAKE_SOURCE_DIR}/Code/BasicFilters
${CMAKE_SOURCE_DIR}/Code/Algorithms
)

LINK_DIRECTORIES(
../../../Code/Numerics/vxl
../../../Code/Common
${CMAKE_BINARY_DIR}/Code/BasicFilters
)

LINK_LIBRARIES (
ITKBasicFilters 
ITKCommon
VXLNumerics
dl
pthread
)

TESTS(
itkCurvatureFlowTest
itkExtractMeshConnectedRegionsTest
itkFastMarchingTest
itkGeodesicActiveContoursTest
itkAlgorithmsHeaderTest
itkFuzzyConnectednessImageFilterTest
itkImageMomentsTest
itkInterpolateTest
itkLevelSetShapeDetectionTest
itkMRFLabellerTest
itkMutualInformationTest
itkNewTest
itkRegionGrow2DTest
itkRegistrationImageToImageAffine
itkRegistrationModelToImage
itkRegistrationPoints3D2DAffine
itkRegistrationProcrustes
itkSupervisedClassifierTest
itkTraitsTest
itkUnsupervisedClassifierTest
)
