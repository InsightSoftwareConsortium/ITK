IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Common ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Common)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

SET(COMMON_TESTS ${CXX_TEST_PATH}/itkCommonTests)
SET(COMMON_HEADER_TEST ${CXX_TEST_PATH}/itkCommonHeaderTest)
SET(SYSTEM_INFORMATION_TEST ${CXX_TEST_PATH}/itkSystemInformationTest)

ADD_TEST(itkChainCodePath2DTest ${COMMON_TESTS} itkChainCodePath2DTest)
ADD_TEST(itkChainCodePathTest ${COMMON_TESTS} itkChainCodePathTest)
ADD_TEST(itkCommonPrintTest ${COMMON_TESTS} itkCommonPrintTest)
ADD_TEST(itkModifiedTimeTest ${COMMON_TESTS} itkModifiedTimeTest)
ADD_TEST(itkAdaptorComparisonTest ${COMMON_TESTS} itkAdaptorComparisonTest)
ADD_TEST(itkAffineTransformTest ${COMMON_TESTS} itkAffineTransformTest)
ADD_TEST(itkFixedCenterOfRotationAffineTransformTest ${COMMON_TESTS} itkFixedCenterOfRotationAffineTransformTest)
ADD_TEST(itkFixedArrayTest ${COMMON_TESTS} itkFixedArrayTest)
ADD_TEST(itkArrayTest ${COMMON_TESTS} itkArrayTest)
ADD_TEST(itkArray2DTest ${COMMON_TESTS} itkArray2DTest)
ADD_TEST(itkAutoPointerTest ${COMMON_TESTS} itkAutoPointerTest)
ADD_TEST(itkAzimuthElevationToCartesianTransformTest ${COMMON_TESTS} itkAzimuthElevationToCartesianTransformTest)
ADD_TEST(itkBinaryThresholdImageFunctionTest ${COMMON_TESTS} itkBinaryThresholdImageFunctionTest)
ADD_TEST(itkBoundingBoxTest ${COMMON_TESTS} itkBoundingBoxTest)
ADD_TEST(itkBSplineDeformableTransformTest ${COMMON_TESTS} itkBSplineDeformableTransformTest)
ADD_TEST(itkBSplineInterpolationWeightFunctionTest ${COMMON_TESTS} itkBSplineInterpolationWeightFunctionTest)
ADD_TEST(itkBSplineKernelFunctionTest ${COMMON_TESTS} itkBSplineKernelFunctionTest)
ADD_TEST(itkByteSwapTest ${COMMON_TESTS} itkByteSwapTest)
ADD_TEST(itkCenteredRigid2DTransformTest ${COMMON_TESTS} itkCenteredRigid2DTransformTest)
ADD_TEST(itkCenteredAffineTransformTest ${COMMON_TESTS} itkCenteredAffineTransformTest)
ADD_TEST(itkCentralDifferenceImageFunctionTest ${COMMON_TESTS} itkCentralDifferenceImageFunctionTest)
ADD_TEST(itkConstNeighborhoodIteratorTest ${COMMON_TESTS} itkConstNeighborhoodIteratorTest)
ADD_TEST(itkConstShapedNeighborhoodIteratorTest ${COMMON_TESTS} itkConstShapedNeighborhoodIteratorTest)
ADD_TEST(itkCovariantVectorGeometryTest ${COMMON_TESTS} itkCovariantVectorGeometryTest)
ADD_TEST(itkCovarianceImageFunctionTest ${COMMON_TESTS} itkCovarianceImageFunctionTest)
ADD_TEST(itkDataTypeTest ${COMMON_TESTS} itkDataTypeTest)
ADD_TEST(itkDynamicMeshTest ${COMMON_TESTS} itkDynamicMeshTest)
ADD_TEST(itkEuler2DTransformTest ${COMMON_TESTS} itkEuler2DTransformTest)
ADD_TEST(itkEuler3DTransformTest ${COMMON_TESTS} itkEuler3DTransformTest)
ADD_TEST(itkExceptionObjectTest ${COMMON_TESTS} itkExceptionObjectTest)
ADD_TEST(itkFileOutputWindowTest ${COMMON_TESTS} itkFileOutputWindowTest)
ADD_TEST(itkFloodFilledSpatialFunctionTest ${COMMON_TESTS} itkFloodFilledSpatialFunctionTest)
ADD_TEST(itkFourierSeriesPathTest ${COMMON_TESTS} itkFourierSeriesPathTest)
ADD_TEST(itkGaussianBlurImageFunctionTest ${COMMON_TESTS} itkGaussianBlurImageFunctionTest)
ADD_TEST(itkGaussianDerivativeImageFunctionTest ${COMMON_TESTS} itkGaussianDerivativeImageFunctionTest)
ADD_TEST(itkCommonHeaderTest ${COMMON_HEADER_TEST})
ADD_TEST(itkHashTableTest ${COMMON_TESTS} itkHashTableTest)
ADD_TEST(itkImageAdaptorTest ${COMMON_TESTS} itkImageAdaptorTest)
ADD_TEST(itkImageDuplicatorTest ${COMMON_TESTS} itkImageDuplicatorTest)
ADD_TEST(itkImageIteratorTest ${COMMON_TESTS} itkImageIteratorTest)
ADD_TEST(itkImageIteratorsForwardBackwardTest ${COMMON_TESTS} itkImageIteratorsForwardBackwardTest)
ADD_TEST(itkImageIteratorWithIndexTest ${COMMON_TESTS} itkImageIteratorWithIndexTest)
ADD_TEST(itkImageLinearIteratorTest ${COMMON_TESTS} itkImageLinearIteratorTest)
ADD_TEST(itkImageRandomIteratorTest ${COMMON_TESTS} itkImageRandomIteratorTest)
ADD_TEST(itkImageRegionTest ${COMMON_TESTS} itkImageRegionTest)
ADD_TEST(itkImageRegionExclusionIteratorWithIndexTest ${COMMON_TESTS} itkImageRegionExclusionIteratorWithIndexTest)
ADD_TEST(itkImageReverseIteratorTest ${COMMON_TESTS} itkImageReverseIteratorTest)
ADD_TEST(itkImageSliceIteratorTest ${COMMON_TESTS} itkImageSliceIteratorTest)
ADD_TEST(itkIteratorTests ${COMMON_TESTS} itkIteratorTests)
ADD_TEST(itkLightObjectTest ${COMMON_TESTS} itkLightObjectTest)
ADD_TEST(itkLevelSetFunctionTest ${COMMON_TESTS} itkLevelSetFunctionTest)
ADD_TEST(itkMatrixTest ${COMMON_TESTS} itkMatrixTest)
ADD_TEST(itkMapContainerTest ${COMMON_TESTS} itkMapContainerTest)
ADD_TEST(itkMahalanobisDistanceThresholdImageFunctionTest ${COMMON_TESTS} itkMahalanobisDistanceThresholdImageFunctionTest)
ADD_TEST(itkMaximumDecisionRuleTest ${COMMON_TESTS} itkMaximumDecisionRuleTest)
ADD_TEST(itkMaximumRatioDecisionRuleTest ${COMMON_TESTS} itkMaximumRatioDecisionRuleTest)
ADD_TEST(itkMeanImageFunctionTest ${COMMON_TESTS} itkMeanImageFunctionTest)
ADD_TEST(itkMedianImageFunctionTest ${COMMON_TESTS} itkMedianImageFunctionTest)
ADD_TEST(itkMemoryLeakTest ${COMMON_TESTS} itkMemoryLeakTest)
ADD_TEST(itkMeshTest ${COMMON_TESTS} itkMeshTest)
ADD_TEST(itkMeshFstreamTest ${COMMON_TESTS} itkMeshFstreamTest)
ADD_TEST(itkMinimumDecisionRuleTest ${COMMON_TESTS} itkMinimumDecisionRuleTest)
ADD_TEST(itkNearestNeighborExtrapolateImageFunctionTest ${COMMON_TESTS} itkNearestNeighborExtrapolateImageFunctionTest)
ADD_TEST(itkNeighborhoodTest ${COMMON_TESTS} itkNeighborhoodTest)
ADD_TEST(itkNeighborhoodIteratorTest ${COMMON_TESTS} itkNeighborhoodIteratorTest)
ADD_TEST(itkNeighborhoodOperatorImageFunctionTest ${COMMON_TESTS} itkNeighborhoodOperatorImageFunctionTest)
ADD_TEST(itkNeighborhoodOperatorTest ${COMMON_TESTS} itkNeighborhoodOperatorTest)
ADD_TEST(itkNumericTraitsTest ${COMMON_TESTS} itkNumericTraitsTest)
ADD_TEST(itkPathFunctionsTest ${COMMON_TESTS} itkPathFunctionsTest)
ADD_TEST(itkPathIteratorTest ${COMMON_TESTS} itkPathIteratorTest)
ADD_TEST(itkPCAShapeSignedDistanceFunctionTest ${COMMON_TESTS} itkPCAShapeSignedDistanceFunctionTest)
ADD_TEST(itkPeriodicBoundaryConditionTest ${COMMON_TESTS} itkPeriodicBoundaryConditionTest)
ADD_TEST(itkPixelAccessTest ${COMMON_TESTS} itkPixelAccessTest)
ADD_TEST(itkPointGeometryTest ${COMMON_TESTS} itkPointGeometryTest)
ADD_TEST(itkPointSetTest ${COMMON_TESTS} itkPointSetTest)
ADD_TEST(itkPolyLineParametricPathTest ${COMMON_TESTS} itkPolyLineParametricPathTest)
ADD_TEST(itkRGBPixelTest ${COMMON_TESTS} itkRGBPixelTest)
ADD_TEST(itkRGBInterpolateImageFunctionTest ${COMMON_TESTS} itkRGBInterpolateImageFunctionTest)
ADD_TEST(itkRGBToVectorImageAdaptorTest ${COMMON_TESTS} itkRGBToVectorImageAdaptorTest)
ADD_TEST(itkRigid2DTransformTest ${COMMON_TESTS} itkRigid2DTransformTest)
ADD_TEST(itkRigid3DTransformTest ${COMMON_TESTS} itkRigid3DTransformTest)
ADD_TEST(itkRigid3DPerspectiveTransformTest ${COMMON_TESTS} itkRigid3DPerspectiveTransformTest)
ADD_TEST(itkScalarToRGBPixelFunctorTest ${COMMON_TESTS} itkScalarToRGBPixelFunctorTest)
ADD_TEST(itkSemaphoreTest ${COMMON_TESTS} itkSemaphoreTest)
ADD_TEST(itkShapedNeighborhoodIteratorTest ${COMMON_TESTS} itkShapedNeighborhoodIteratorTest)
ADD_TEST(itkSimilarity2DTransformTest ${COMMON_TESTS} itkSimilarity2DTransformTest)
ADD_TEST(itkSliceIteratorTest ${COMMON_TESTS} itkSliceIteratorTest)
ADD_TEST(itkSpatialFunctionTest ${COMMON_TESTS} itkSpatialFunctionTest)
ADD_TEST(itkSphereSignedDistanceFunctionTest ${COMMON_TESTS} itkSphereSignedDistanceFunctionTest)
ADD_TEST(itkSmartPointerTest ${COMMON_TESTS} itkSmartPointerTest)
ADD_TEST(itkSTLContainerAdaptorTest ${COMMON_TESTS} itkSTLContainerAdaptorTest)
ADD_TEST(itkTimeProbesTest ${COMMON_TESTS} itkTimeProbesTest)
ADD_TEST(itkTransformTest ${COMMON_TESTS} itkTransformTest)
ADD_TEST(itkThreadDefsTest ${COMMON_TESTS} itkThreadDefsTest)
ADD_TEST(itkTranslationTransformTest ${COMMON_TESTS} itkTranslationTransformTest)
ADD_TEST(itkVarianceImageFunctionTest ${COMMON_TESTS} itkVarianceImageFunctionTest)
ADD_TEST(itkVectorGeometryTest ${COMMON_TESTS} itkVectorGeometryTest)
ADD_TEST(itkVectorMeanImageFunctionTest ${COMMON_TESTS} itkVectorMeanImageFunctionTest)
ADD_TEST(itkVersorTest ${COMMON_TESTS} itkVersorTest)
ADD_TEST(itkVersorRigid3DTransformTest ${COMMON_TESTS} itkVersorRigid3DTransformTest)
ADD_TEST(itkVectorTest ${COMMON_TESTS} itkVectorTest)
ADD_TEST(itkVectorInterpolateImageFunctionTest ${COMMON_TESTS} itkVectorInterpolateImageFunctionTest)
ADD_TEST(itkVectorToRGBImageAdaptorTest ${COMMON_TESTS} itkVectorToRGBImageAdaptorTest)
ADD_TEST(itkScaleTransformTest ${COMMON_TESTS} itkScaleTransformTest)
ADD_TEST(itkScaleLogarithmicTransformTest ${COMMON_TESTS} itkScaleLogarithmicTransformTest)
ADD_TEST(itkSplineKernelTransformTest ${COMMON_TESTS} itkSplineKernelTransformTest)
ADD_TEST(itkEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS} itkEllipsoidInteriorExteriorSpatialFunctionTest)
ADD_TEST(itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS} itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
ADD_TEST(itkMetaDataDictionaryTest ${COMMON_TESTS} itkMetaDataDictionaryTest)
ADD_TEST(itkSTLThreadTest ${COMMON_TESTS} itkSTLThreadTest)
ADD_TEST(itkOctreeTest ${COMMON_TESTS} itkOctreeTest)

#-----------------------------------------------------------------------------
# Setup the system information test.  Write out some basic failsafe
# information in case the test doesn't run.

ADD_TEST(itkSystemInformationTest ${SYSTEM_INFORMATION_TEST})

IF(ITK_BINARY_DIR)
  SET(ITK_SYSTEM_INFORMATION_DIR ${ITK_BINARY_DIR})
ELSE(ITK_BINARY_DIR)
  SET(ITK_SYSTEM_INFORMATION_DIR ${ITKTesting_BINARY_DIR})
ENDIF(ITK_BINARY_DIR)

WRITE_FILE(
  "${ITK_SYSTEM_INFORMATION_DIR}/Testing/HTML/TestingResults/Sites/${SITE}/${BUILDNAME}/BuildNameNotes.xml"

  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<Site BuildName=\"CMakeCache\" Name=\"crd\">\n"
  "<BuildNameNotes>\n"
  "<Note>\n"
  "<DateTime>Wed Oct 24 1:00:00 EST</DateTime>\n"
  "<Text>\n"
  "The full itkSystemInformationTest did not run.\n"
  "Here is some basic information:\n"
  "ITK_SOURCE_DIR = ${ITK_SOURCE_DIR}\n"
  "ITK_BINARY_DIR = ${ITK_BINARY_DIR}\n"
  "ITKTesting_SOURCE_DIR = ${ITKTesting_SOURCE_DIR}\n"
  "ITKTesting_BINARY_DIR = ${ITKTesting_BINARY_DIR}\n"
  "CMAKE_C_COMPILER = ${CMAKE_C_COMPILER}\n"
  "CMAKE_C_FLAGS = ${CMAKE_C_FLAGS}\n"
  "CMAKE_CXX_COMPILER = ${CMAKE_CXX_COMPILER}\n"
  "CMAKE_CXX_FLAGS = ${CMAKE_CXX_FLAGS}\n"
  "CMAKE_SYSTEM = ${CMAKE_SYSTEM}\n"
  "CMAKE_MAKE_PROGRAM = ${CMAKE_MAKE_PROGRAM}\n"
  "</Text>\n"
  "</Note>\n"
  "</BuildNameNotes>\n"
  "</Site>\n"
)
#-----------------------------------------------------------------------------

SET(BasicTests_SRCS
itkCommonPrintTest.cxx
itkModifiedTimeTest.cxx
itkAdaptorComparisonTest.cxx
itkAffineTransformTest.cxx
itkArrayTest.cxx
itkArray2DTest.cxx
itkAutoPointerTest.cxx
itkAzimuthElevationToCartesianTransformTest.cxx
itkBinaryThresholdImageFunctionTest.cxx
itkBoundingBoxTest.cxx
itkBSplineDeformableTransformTest.cxx
itkBSplineInterpolationWeightFunctionTest.cxx
itkBSplineKernelFunctionTest.cxx
itkByteSwapTest.cxx
itkCenteredRigid2DTransformTest.cxx
itkCenteredAffineTransformTest.cxx
itkCentralDifferenceImageFunctionTest.cxx
itkChainCodePath2DTest.cxx
itkChainCodePathTest.cxx
itkConstNeighborhoodIteratorTest.cxx
itkConstShapedNeighborhoodIteratorTest.cxx
itkCovariantVectorGeometryTest.cxx
itkCovarianceImageFunctionTest.cxx
itkDataTypeTest.cxx
itkDynamicMeshTest.cxx
itkEuler2DTransformTest.cxx
itkEuler3DTransformTest.cxx
itkExceptionObjectTest.cxx
itkFileOutputWindowTest.cxx
itkFixedArrayTest.cxx
itkFixedCenterOfRotationAffineTransformTest.cxx
itkFloodFilledSpatialFunctionTest.cxx
itkFourierSeriesPathTest.cxx
itkGaussianBlurImageFunctionTest.cxx
itkGaussianDerivativeImageFunctionTest.cxx
itkHashTableTest.cxx
itkImageAdaptorTest.cxx
itkImageDuplicatorTest.cxx
itkImageIteratorTest.cxx
itkImageIteratorsForwardBackwardTest.cxx
itkImageIteratorWithIndexTest.cxx
itkImageLinearIteratorTest.cxx
itkImageRandomIteratorTest.cxx
itkImageRegionTest.cxx
itkImageRegionExclusionIteratorWithIndexTest.cxx
itkImageReverseIteratorTest.cxx
itkImageSliceIteratorTest.cxx
itkIteratorTests.cxx
itkLevelSetFunctionTest.cxx
itkLightObjectTest.cxx
itkMatrixTest.cxx
itkMapContainerTest.cxx
itkMahalanobisDistanceThresholdImageFunctionTest.cxx
itkMaximumDecisionRuleTest.cxx
itkMaximumRatioDecisionRuleTest.cxx
itkMeanImageFunctionTest.cxx 
itkMedianImageFunctionTest.cxx 
itkMemoryLeakTest.cxx
itkMeshTest.cxx
itkMeshFstreamTest.cxx
itkMinimumDecisionRuleTest.cxx
itkNearestNeighborExtrapolateImageFunctionTest.cxx
itkNeighborhoodTest.cxx
itkNeighborhoodIteratorTest.cxx
itkNeighborhoodOperatorImageFunctionTest.cxx
itkNeighborhoodOperatorTest.cxx
itkNumericTraitsTest.cxx
itkObjectStoreTest.cxx
itkOctreeTest.cxx
itkPathFunctionsTest.cxx
itkPathIteratorTest.cxx
itkPCAShapeSignedDistanceFunctionTest.cxx
itkPeriodicBoundaryConditionTest.cxx
itkPixelAccessTest.cxx
itkPointGeometryTest.cxx
itkPointSetTest.cxx
itkPolyLineParametricPathTest.cxx
itkRigid2DTransformTest.cxx
itkRigid3DTransformTest.cxx
itkRigid3DPerspectiveTransformTest.cxx
itkRGBPixelTest.cxx
itkRGBInterpolateImageFunctionTest.cxx
itkRGBToVectorImageAdaptorTest.cxx
itkScalarToRGBPixelFunctorTest.cxx
itkScaleTransformTest.cxx
itkScaleLogarithmicTransformTest.cxx
itkShapedNeighborhoodIteratorTest.cxx
itkSemaphoreTest.cxx
itkSimilarity2DTransformTest.cxx
itkSliceIteratorTest.cxx
itkSmartPointerTest.cxx
itkSTLContainerAdaptorTest.cxx
itkSpatialFunctionTest.cxx
itkSphereSignedDistanceFunctionTest.cxx
itkSplineKernelTransformTest.cxx
itkThreadDefsTest.cxx
itkTimeProbesTest.cxx
itkTransformTest.cxx
itkTranslationTransformTest.cxx
itkVectorGeometryTest.cxx
itkVarianceImageFunctionTest.cxx
itkVectorMeanImageFunctionTest.cxx
itkVersorTest.cxx
itkVersorRigid3DTransformTest.cxx
itkVectorTest.cxx
itkVectorInterpolateImageFunctionTest.cxx
itkVectorToRGBImageAdaptorTest.cxx
itkEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkMetaDataDictionaryTest.cxx
itkSTLThreadTest.cxx
)

CONFIGURE_FILE(${ITKTesting_SOURCE_DIR}/Code/Common/itkSystemInformationTest.h.in
               ${ITKTesting_BINARY_DIR}/Code/Common/itkSystemInformationTest.h
               @ONLY IMMEDIATE)
INCLUDE_DIRECTORIES("${ITKTesting_BINARY_DIR}")

ADD_EXECUTABLE(itkSystemInformationTest itkSystemInformationTest.cxx)
ADD_EXECUTABLE(itkCommonHeaderTest itkCommonHeaderTest.cxx)
ADD_EXECUTABLE(itkCommonTests itkCommonTests.cxx ${BasicTests_SRCS})
TARGET_LINK_LIBRARIES(itkCommonHeaderTest ITKCommon)
TARGET_LINK_LIBRARIES(itkCommonTests ITKIO ITKCommon)
