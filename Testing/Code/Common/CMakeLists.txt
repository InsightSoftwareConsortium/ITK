SET(KIT Common)
CREATE_TEST_SOURCELIST(Tests itk${KIT}Tests.cxx 
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
  itkPeriodicBoundaryConditionTest.cxx
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

ADD_EXECUTABLE(itk${KIT}Tests ${Tests})
TARGET_LINK_LIBRARIES(itk${KIT}Tests ITKCommon)

SET (TestsToRun ${Tests})
REMOVE (TestsToRun itk${KIT}Tests.cxx)

FOREACH (test ${TestsToRun})
  GET_FILENAME_COMPONENT(TName ${test} NAME_WE)
  ADD_TEST(${TName} ${CXX_TEST_PATH}/itk${KIT}Tests ${TName}) 
ENDFOREACH (test)

# Add PrintSelf test.
IF (TCL_TCLSH)
  ADD_TEST(PrintSelf-${KIT} ${TCL_TCLSH}
           ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
           ${ITK_SOURCE_DIR}/Code/${KIT})
ENDIF (TCL_TCLSH)

# Add header test.
ADD_EXECUTABLE(itk${KIT}HeaderTest itk${KIT}HeaderTest.cxx)
TARGET_LINK_LIBRARIES(itk${KIT}HeaderTest ITKCommon)
ADD_TEST(itk${KIT}HeaderTest ${CXX_TEST_PATH}/itk${KIT}HeaderTest)
