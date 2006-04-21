// this file defines the itkCommonTest for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(itkMeanImageFunctionTest );
REGISTER_TEST(itkMedialNodeCorrespondencesTest );
REGISTER_TEST(itkMedianImageFunctionTest );
REGISTER_TEST(itkMemoryLeakTest );
REGISTER_TEST(itkMeshTest );
REGISTER_TEST(itkMeshFstreamTest );
REGISTER_TEST(itkMeshSourceGraftOutputTest );
REGISTER_TEST(itkMetaDataDictionaryTest );
REGISTER_TEST(itkMinimumDecisionRuleTest );
REGISTER_TEST(itkNearestNeighborExtrapolateImageFunctionTest );
REGISTER_TEST(itkNeighborhoodTest );
REGISTER_TEST(itkNeighborhoodIteratorTest );
REGISTER_TEST(itkNeighborhoodOperatorImageFunctionTest);
REGISTER_TEST(itkNeighborhoodOperatorTest );
REGISTER_TEST(itkNumericTraitsTest );
REGISTER_TEST(itkObjectStoreTest );
REGISTER_TEST(itkOctreeTest );
REGISTER_TEST(itkOrthogonallyCorrected2DParametricPathTest );
REGISTER_TEST(itkPathFunctionsTest );
REGISTER_TEST(itkPathIteratorTest );
REGISTER_TEST(itkPCAShapeSignedDistanceFunctionTest );
REGISTER_TEST(itkPeriodicBoundaryConditionTest );
REGISTER_TEST(itkPhasedArray3DSpecialCoordinatesImageTest );
REGISTER_TEST(itkPixelAccessTest );
REGISTER_TEST(itkPointGeometryTest );
REGISTER_TEST(itkPointSetTest );
REGISTER_TEST(itkPolyLineParametricPathTest );
REGISTER_TEST(itkPolygonCellTest );
REGISTER_TEST(itkQuaternionRigidTransformTest );
REGISTER_TEST(itkRealTimeClockTest );
REGISTER_TEST(itkRGBPixelTest );
REGISTER_TEST(itkRGBInterpolateImageFunctionTest );
REGISTER_TEST(itkRGBToVectorImageAdaptorTest );
REGISTER_TEST(itkRigid2DTransformTest );
REGISTER_TEST(itkRigid3DTransformTest );
REGISTER_TEST(itkRigid3DPerspectiveTransformTest );
REGISTER_TEST(itkScalarToRGBPixelFunctorTest );
REGISTER_TEST(itkScatterMatrixImageFunctionTest );
REGISTER_TEST(itkScaleTransformTest );
REGISTER_TEST(itkScaleLogarithmicTransformTest );
REGISTER_TEST(itkSemaphoreTest );
REGISTER_TEST(itkShapedNeighborhoodIteratorTest );
REGISTER_TEST(itkSimilarity2DTransformTest );
REGISTER_TEST(itkSimilarity3DTransformTest );
REGISTER_TEST(itkSimplexMeshTest );
REGISTER_TEST(itkSizeTest );
REGISTER_TEST(itkSliceIteratorTest );
REGISTER_TEST(itkSmartPointerTest );
REGISTER_TEST(itkSparseImageTest );
REGISTER_TEST(itkSpatialFunctionTest );
REGISTER_TEST(itkSphereSignedDistanceFunctionTest );
REGISTER_TEST(itkSplineKernelTransformTest );
REGISTER_TEST(itkSTLContainerAdaptorTest );
REGISTER_TEST(itkStdStreamLogOutputTest );
REGISTER_TEST(itkThreadDefsTest );
REGISTER_TEST(itkThreadLoggerTest );
REGISTER_TEST(itkTimeProbesTest );
REGISTER_TEST(itkTransformTest );
REGISTER_TEST(itkTransformsSetParametersTest );
REGISTER_TEST(itkTranslationTransformTest );
REGISTER_TEST(itkTreeContainerTest );
REGISTER_TEST(itkTreeContainerTest2 );
REGISTER_TEST(itkVarianceImageFunctionTest );
REGISTER_TEST(itkVectorGeometryTest );
REGISTER_TEST(itkVectorImageTest );
REGISTER_TEST(itkVectorMeanImageFunctionTest );
REGISTER_TEST(itkVersorTransformTest );
REGISTER_TEST(itkVersorTest );
REGISTER_TEST(itkVersorRigid3DTransformTest );
REGISTER_TEST(itkVectorTest );
REGISTER_TEST(itkVectorInterpolateImageFunctionTest );
REGISTER_TEST(itkVectorToRGBImageAdaptorTest );
REGISTER_TEST(itkWindowedSincInterpolateImageFunctionTest );
REGISTER_TEST(itkEllipsoidInteriorExteriorSpatialFunctionTest );
REGISTER_TEST(itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest );
REGISTER_TEST(itkSymmetricSecondRankTensorTest );
REGISTER_TEST(itkSymmetricEigenAnalysisTest );
REGISTER_TEST(itkZeroFluxBoundaryConditionTest );
//This is a separate executable REGISTER_TEST(itkSystemInformationTest );
REGISTER_TEST(itkSTLThreadTest );
REGISTER_TEST(itkVariableLengthVectorTest);
// REGISTER_TEST(itkQuaternionOrientationAdapterTest);
}

