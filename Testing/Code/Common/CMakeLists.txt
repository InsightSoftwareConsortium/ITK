IF (TCL_TCLSH)
ADD_TEST(PrintSelf-Common ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/Common)
ENDIF (TCL_TCLSH)

IF( CXX_TEST_PATH )
  SET(COMMON_TESTS ${CXX_TEST_PATH}/itkCommonTests)
  SET(COMMON_HEADER_TEST ${CXX_TEST_PATH}/itkCommonHeaderTest)
ELSE( CXX_TEST_PATH )
  SET(COMMON_TESTS itkCommonTests)
  SET(COMMON_HEADER_TEST itkCommonHeaderTest)
ENDIF( CXX_TEST_PATH )


ADD_TEST(itkModifiedTimeTest ${COMMON_TESTS} itkModifiedTimeTest)
ADD_TEST(itkAdaptorComparisonTest ${COMMON_TESTS} itkAdaptorComparisonTest)
ADD_TEST(itkAffineTransformTest ${COMMON_TESTS} itkAffineTransformTest)
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
ADD_TEST(itkConstNeighborhoodIteratorTest ${COMMON_TESTS} itkConstNeighborhoodIteratorTest)
ADD_TEST(itkConstShapedNeighborhoodIteratorTest ${COMMON_TESTS} itkConstShapedNeighborhoodIteratorTest)
ADD_TEST(itkCovariantVectorGeometryTest ${COMMON_TESTS} itkCovariantVectorGeometryTest)
ADD_TEST(itkDataTypeTest ${COMMON_TESTS} itkDataTypeTest)
ADD_TEST(itkDynamicMeshTest ${COMMON_TESTS} itkDynamicMeshTest)
ADD_TEST(itkEuler2DTransformTest ${COMMON_TESTS} itkEuler2DTransformTest)
ADD_TEST(itkEuler3DTransformTest ${COMMON_TESTS} itkEuler3DTransformTest)
ADD_TEST(itkExceptionObjectTest ${COMMON_TESTS} itkExceptionObjectTest)
ADD_TEST(itkCommonHeaderTest ${COMMON_HEADER_TEST})
ADD_TEST(itkHashTableTest ${COMMON_TESTS} itkHashTableTest)
ADD_TEST(itkImageAdaptorTest ${COMMON_TESTS} itkImageAdaptorTest)
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
ADD_TEST(itkMeanImageFunctionTest ${COMMON_TESTS} itkMeanImageFunctionTest)
ADD_TEST(itkMemoryLeakTest ${COMMON_TESTS} itkMemoryLeakTest)
ADD_TEST(itkMeshTest ${COMMON_TESTS} itkMeshTest)
ADD_TEST(itkMeshFstreamTest ${COMMON_TESTS} itkMeshFstreamTest)
ADD_TEST(itkNeighborhoodTest ${COMMON_TESTS} itkNeighborhoodTest)
ADD_TEST(itkNeighborhoodIteratorTest ${COMMON_TESTS} itkNeighborhoodIteratorTest)
ADD_TEST(itkNeighborhoodOperatorTest ${COMMON_TESTS} itkNeighborhoodOperatorTest)
ADD_TEST(itkPixelAccessTest ${COMMON_TESTS} itkPixelAccessTest)
ADD_TEST(itkPointGeometryTest ${COMMON_TESTS} itkPointGeometryTest)
ADD_TEST(itkPointSetTest ${COMMON_TESTS} itkPointSetTest)
ADD_TEST(itkRGBPixelTest ${COMMON_TESTS} itkRGBPixelTest)
ADD_TEST(itkRGBInterpolateImageFunctionTest ${COMMON_TESTS} itkRGBInterpolateImageFunctionTest)
ADD_TEST(itkRGBToVectorImageAdaptorTest ${COMMON_TESTS} itkRGBToVectorImageAdaptorTest)
ADD_TEST(itkRigid2DTransformTest ${COMMON_TESTS} itkRigid2DTransformTest)
ADD_TEST(itkRigid3DTransformTest ${COMMON_TESTS} itkRigid3DTransformTest)
ADD_TEST(itkRigid3DPerspectiveTransformTest ${COMMON_TESTS} itkRigid3DPerspectiveTransformTest)
ADD_TEST(itkScalarToRGBPixelFunctorTest ${COMMON_TESTS} itkScalarToRGBPixelFunctorTest)
ADD_TEST(itkShapedNeighborhoodIteratorTest ${COMMON_TESTS} itkShapedNeighborhoodIteratorTest)
ADD_TEST(itkSimilarity2DTransformTest ${COMMON_TESTS} itkSimilarity2DTransformTest)
ADD_TEST(itkSliceIteratorTest ${COMMON_TESTS} itkSliceIteratorTest)
ADD_TEST(itkSpatialFunctionTest ${COMMON_TESTS} itkSpatialFunctionTest)
ADD_TEST(itkSmartPointerTest ${COMMON_TESTS} itkSmartPointerTest)
ADD_TEST(itkSTLContainerAdaptorTest ${COMMON_TESTS} itkSTLContainerAdaptorTest)
ADD_TEST(itkTimeProbesTest ${COMMON_TESTS} itkTimeProbesTest)
ADD_TEST(itkTransformTest ${COMMON_TESTS} itkTransformTest)
ADD_TEST(itkThreadDefsTest ${COMMON_TESTS} itkThreadDefsTest)
ADD_TEST(itkTranslationTransformTest ${COMMON_TESTS} itkTranslationTransformTest)
ADD_TEST(itkVarianceImageFunctionTest ${COMMON_TESTS} itkVarianceImageFunctionTest)
ADD_TEST(itkVectorGeometryTest ${COMMON_TESTS} itkVectorGeometryTest)
ADD_TEST(itkVersorTest ${COMMON_TESTS} itkVersorTest)
ADD_TEST(itkVersorRigid3DTransformTest ${COMMON_TESTS} itkVersorRigid3DTransformTest)
ADD_TEST(itkVectorTest ${COMMON_TESTS} itkVectorTest)
ADD_TEST(itkVectorInterpolateImageFunctionTest ${COMMON_TESTS} itkVectorInterpolateImageFunctionTest)
ADD_TEST(itkVectorToRGBImageAdaptorTest ${COMMON_TESTS} itkVectorToRGBImageAdaptorTest)
ADD_TEST(itkScaleTransformTest ${COMMON_TESTS} itkScaleTransformTest)
ADD_TEST(itkSplineKernelTransformTest ${COMMON_TESTS} itkSplineKernelTransformTest)
ADD_TEST(itkEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS} itkEllipsoidInteriorExteriorSpatialFunctionTest)
ADD_TEST(itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS} itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)

SET(BasicTests_SRCS
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
itkConstNeighborhoodIteratorTest.cxx
itkConstShapedNeighborhoodIteratorTest.cxx
itkCovariantVectorGeometryTest.cxx
itkDataTypeTest.cxx
itkDynamicMeshTest.cxx
itkEuler2DTransformTest.cxx
itkEuler3DTransformTest.cxx
itkExceptionObjectTest.cxx
itkFixedArrayTest.cxx
itkHashTableTest.cxx
itkImageAdaptorTest.cxx
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
itkMeanImageFunctionTest.cxx 
itkMemoryLeakTest.cxx
itkMeshTest.cxx
itkMeshFstreamTest.cxx
itkNeighborhoodTest.cxx
itkNeighborhoodIteratorTest.cxx
itkNeighborhoodOperatorTest.cxx
itkObjectStoreTest.cxx
itkPixelAccessTest.cxx
itkPointGeometryTest.cxx
itkPointSetTest.cxx
itkRigid2DTransformTest.cxx
itkRigid3DTransformTest.cxx
itkRigid3DPerspectiveTransformTest.cxx
itkRGBPixelTest.cxx
itkRGBInterpolateImageFunctionTest.cxx
itkRGBToVectorImageAdaptorTest.cxx
itkScalarToRGBPixelFunctorTest.cxx
itkScaleTransformTest.cxx
itkShapedNeighborhoodIteratorTest.cxx
itkSimilarity2DTransformTest.cxx
itkSliceIteratorTest.cxx
itkSmartPointerTest.cxx
itkSTLContainerAdaptorTest.cxx
itkSpatialFunctionTest.cxx
itkSplineKernelTransformTest.cxx
itkThreadDefsTest.cxx
itkTimeProbesTest.cxx
itkTransformTest.cxx
itkTranslationTransformTest.cxx
itkVectorGeometryTest.cxx
itkVarianceImageFunctionTest.cxx
itkVersorTest.cxx
itkVersorRigid3DTransformTest.cxx
itkVectorTest.cxx
itkVectorInterpolateImageFunctionTest.cxx
itkVectorToRGBImageAdaptorTest.cxx
itkEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest.cxx
)


ADD_EXECUTABLE(itkCommonHeaderTest itkCommonHeaderTest.cxx)
ADD_EXECUTABLE(itkCommonTests itkCommonTests.cxx ${BasicTests_SRCS})
TARGET_LINK_LIBRARIES(itkCommonHeaderTest ITKCommon)
TARGET_LINK_LIBRARIES(itkCommonTests ITKCommon)



