// this file defines the itkCommonTest for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(itkImageComputeOffsetAndIndexTest );
REGISTER_TEST(itkCommonPrintTest );
REGISTER_TEST(itkConditionVariableTest );
REGISTER_TEST(itkModifiedTimeTest );
REGISTER_TEST(itkMultipleLogOutputTest );
REGISTER_TEST(itkAdaptorComparisonTest );
REGISTER_TEST(itkAffineTransformTest );
REGISTER_TEST(itkAnnulusOperatorTest );
REGISTER_TEST(itkArrayTest );
REGISTER_TEST(itkArray2DTest );
REGISTER_TEST(itkAutoPointerTest );
REGISTER_TEST(itkAzimuthElevationToCartesianTransformTest );
REGISTER_TEST(itkBarrierTest );
REGISTER_TEST(itkBinaryThresholdImageFunctionTest );
REGISTER_TEST(itkBinaryThresholdSpatialFunctionTest );
REGISTER_TEST(itkBoundingBoxTest );
REGISTER_TEST(itkBSplineDeformableTransformTest );
REGISTER_TEST(itkBSplineInterpolationWeightFunctionTest );
REGISTER_TEST(itkBSplineKernelFunctionTest );
REGISTER_TEST(itkBoundaryConditionTest );
REGISTER_TEST(itkByteSwapTest );
REGISTER_TEST(itkCenteredRigid2DTransformTest );
REGISTER_TEST(itkCenteredAffineTransformTest );
REGISTER_TEST(itkCentralDifferenceImageFunctionTest );
REGISTER_TEST(itkCenteredEuler3DTransformTest );
REGISTER_TEST(itkCenteredTransformInitializerTest   );
REGISTER_TEST(itkCenteredVersorTransformInitializerTest   );
REGISTER_TEST(itkChainCodePath2DTest );
REGISTER_TEST(itkChainCodePathTest );
REGISTER_TEST(itkColorTableTest );
REGISTER_TEST(itkConstNeighborhoodIteratorTest );
REGISTER_TEST(itkConstShapedNeighborhoodIteratorTest );
REGISTER_TEST(itkCovariantVectorGeometryTest );
REGISTER_TEST(itkCovarianceImageFunctionTest );
REGISTER_TEST(itkDataTypeTest );
REGISTER_TEST(itkDecoratorTest ); 
REGISTER_TEST(itkDirectoryTest ); 
REGISTER_TEST(itkDynamicMeshTest );
REGISTER_TEST(itkDiffusionTensor3DTest );
REGISTER_TEST(itkEuler2DTransformTest );
REGISTER_TEST(itkEuler3DTransformTest );
REGISTER_TEST(itkEventObjectTest );
REGISTER_TEST(itkExceptionObjectTest );
REGISTER_TEST(itkFileOutputWindowTest );
REGISTER_TEST(itkFiniteCylinderSpatialFunctionTest );
REGISTER_TEST(itkFixedArrayTest );
REGISTER_TEST(itkFixedCenterOfRotationAffineTransformTest );
REGISTER_TEST(itkFloodFilledSpatialFunctionTest );
REGISTER_TEST(itkFourierSeriesPathTest );
REGISTER_TEST(itkGaussianBlurImageFunctionTest);
REGISTER_TEST(itkGaussianDerivativeImageFunctionTest);
REGISTER_TEST(itkHashTableTest );
REGISTER_TEST(itkIdentityTransformTest );
REGISTER_TEST(itkImageAdaptorTest );
REGISTER_TEST(itkImageDuplicatorTest );
REGISTER_TEST(itkImageIteratorTest );
REGISTER_TEST(itkImageIteratorsForwardBackwardTest );
REGISTER_TEST(itkImageIteratorWithIndexTest );
REGISTER_TEST(itkImageLinearIteratorTest );
REGISTER_TEST(itkImageRandomIteratorTest );
REGISTER_TEST(itkImageRandomNonRepeatingIteratorWithIndexTest );
REGISTER_TEST(itkImageRegionTest );
REGISTER_TEST(itkImageRegionExclusionIteratorWithIndexTest );
REGISTER_TEST(itkImageReverseIteratorTest );
REGISTER_TEST(itkImageSliceIteratorTest );
REGISTER_TEST(itkImageTest );
REGISTER_TEST(itkIteratorTests );
REGISTER_TEST(itkLandmarkBasedTransformInitializerTest );
REGISTER_TEST(itkLevelSetFunctionTest );
REGISTER_TEST(itkLightObjectTest );
REGISTER_TEST(itkLineIteratorTest );
REGISTER_TEST(itkLoggerTest );
//Add this once it is tested under VS6 and VS 7 REGISTER_TEST(itkLoggerThreadWrapperTest );
REGISTER_TEST(itkLoggerOutputTest );
REGISTER_TEST(itkLoggerManagerTest );
REGISTER_TEST(itkMatrixTest );
REGISTER_TEST(itkMapContainerTest );
REGISTER_TEST(itkMahalanobisDistanceThresholdImageFunctionTest );
REGISTER_TEST(itkMaximumDecisionRuleTest );
REGISTER_TEST(itkMaximumRatioDecisionRuleTest );
}

