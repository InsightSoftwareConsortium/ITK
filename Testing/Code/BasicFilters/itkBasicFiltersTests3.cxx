/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h"

void RegisterTests()
{
  REGISTER_TEST(itkAccumulateImageFilterTest );
  REGISTER_TEST(itkApproximateSignedDistanceMapImageFilterTest );
  REGISTER_TEST(itkCheckerBoardImageFilterTest );
  REGISTER_TEST(itkHessianRecursiveGaussianFilterTest );
  REGISTER_TEST(itkHessianRecursiveGaussianFilterScaleSpaceTest );
  REGISTER_TEST(itkLabelStatisticsImageFilterTest );
  REGISTER_TEST(itkSymmetricEigenAnalysisImageFilterTest );
  REGISTER_TEST(itkNormalizedCorrelationImageFilterTest );
  REGISTER_TEST(itkPolylineMaskImageFilterTest );
  REGISTER_TEST(itkPolylineMask2DImageFilterTest );
  REGISTER_TEST(itkTensorFractionalAnisotropyImageFilterTest );
  REGISTER_TEST(itkTensorRelativeAnisotropyImageFilterTest );
  REGISTER_TEST(itkTriangleMeshToBinaryImageFilterTest );
  REGISTER_TEST(itkTriangleMeshToBinaryImageFilterTest2 );
  REGISTER_TEST(itkVectorAnisotropicDiffusionImageFilterTest );
  REGISTER_TEST(itkVectorConfidenceConnectedImageFilterTest );
  REGISTER_TEST(itkVectorExpandImageFilterTest );
  REGISTER_TEST(itkVectorGradientMagnitudeImageFilterTest1 );
  REGISTER_TEST(itkVectorGradientMagnitudeImageFilterTest2 );
  REGISTER_TEST(itkVectorGradientMagnitudeImageFilterTest3 );
  REGISTER_TEST(itkVectorNeighborhoodOperatorImageFilterTest );
  REGISTER_TEST(itkVectorResampleImageFilterTest );
  REGISTER_TEST(itkVectorRescaleIntensityImageFilterTest );
  REGISTER_TEST(itkVotingBinaryHoleFillingImageFilterTest );
  REGISTER_TEST(itkVotingBinaryImageFilterTest );
  REGISTER_TEST(itkVotingBinaryIterativeHoleFillingImageFilterTest );
  REGISTER_TEST(itkWarpImageFilterTest );
  REGISTER_TEST(itkWarpMeshFilterTest );
  REGISTER_TEST(itkWarpVectorImageFilterTest );
  REGISTER_TEST(itkWeightedAddImageFilterTest);
  REGISTER_TEST(itkWrapPadImageTest );
  REGISTER_TEST(itkXorImageFilterTest );
  REGISTER_TEST(itkZeroCrossingBasedEdgeDetectionImageFilterTest );
  REGISTER_TEST(itkZeroCrossingImageFilterTest );
}
