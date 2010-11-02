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
  REGISTER_TEST(itkBasicFiltersPrintTest );
  REGISTER_TEST(itkBasicFiltersPrintTest2 );
  REGISTER_TEST(itkComplexToImaginaryFilterAndAdaptorTest );
  REGISTER_TEST(itkComplexToRealFilterAndAdaptorTest );
  REGISTER_TEST(itkComplexToModulusFilterAndAdaptorTest );
  REGISTER_TEST(itkComplexToPhaseFilterAndAdaptorTest );
  REGISTER_TEST(itkConstrainedValueAdditionImageFilterTest );
  REGISTER_TEST(itkConstrainedValueDifferenceImageFilterTest );
  REGISTER_TEST(itkContourDirectedMeanDistanceImageFilterTest );
  REGISTER_TEST(itkContourMeanDistanceImageFilterTest );
  REGISTER_TEST(itkDeformationFieldJacobianDeterminantFilterTest );
  REGISTER_TEST(itkDisplacementFieldJacobianDeterminantFilterTest );
  REGISTER_TEST(itkGrayscaleMorphologicalClosingImageFilterTest );
  REGISTER_TEST(itkGrayscaleMorphologicalOpeningImageFilterTest );
  REGISTER_TEST(itkJoinSeriesImageFilterPrintTest);
  REGISTER_TEST(itkJoinSeriesImageFilterTest );
  REGISTER_TEST(itkJoinSeriesImageFilterStreamingTest );
  REGISTER_TEST(itkShiftScaleImageFilterTest );
  REGISTER_TEST(itkPushPopTileImageFilterTest );
  REGISTER_TEST(itkShiftScaleInPlaceImageFilterTest );
  REGISTER_TEST(itkShrinkImageTest );
  REGISTER_TEST(itkShrinkImagePreserveObjectPhysicalLocations);
  REGISTER_TEST(itkShrinkImageStreamingTest );
  REGISTER_TEST(itkSigmoidImageFilterTest );
  REGISTER_TEST(itkSignedDanielssonDistanceMapImageFilterTest );
  REGISTER_TEST(itkSimilarityIndexImageFilterTest );
  REGISTER_TEST(itkSimpleContourExtractorImageFilterTest );
  REGISTER_TEST(itkSimplexMeshAdaptTopologyFilterTest);
  REGISTER_TEST(itkSimplexMeshToTriangleMeshFilterTest);
  REGISTER_TEST(itkSinImageFilterAndAdaptorTest );
  REGISTER_TEST(itkSmoothingRecursiveGaussianImageFilterTest );
  REGISTER_TEST(itkSobelEdgeDetectionImageFilterTest );
  REGISTER_TEST(itkSparseFieldFourthOrderLevelSetImageFilterTest );
  REGISTER_TEST(itkSparseFieldLayerTest);
  REGISTER_TEST(itkSpatialFunctionImageEvaluatorFilterTest );
  REGISTER_TEST(itkSpatialObjectToImageFilterTest );
  REGISTER_TEST(itkSpatialObjectToImageStatisticsCalculatorTest );
  REGISTER_TEST(itkSpatialObjectToPointSetFilterTest );
  REGISTER_TEST(itkSqrtImageFilterAndAdaptorTest );
  REGISTER_TEST(itkSquareImageFilterTest );
  REGISTER_TEST(itkSquaredDifferenceImageFilterTest );
  REGISTER_TEST(itkStatisticsImageFilterTest );
  REGISTER_TEST(itkStreamingImageFilterTest );
  REGISTER_TEST(itkStreamingImageFilterTest2 );
  REGISTER_TEST(itkStreamingImageFilterTest3 );
  REGISTER_TEST(itkSubtractImageFilterTest );
  REGISTER_TEST(itkTanImageFilterAndAdaptorTest );
  REGISTER_TEST(itkTernaryMagnitudeImageFilterTest );
  REGISTER_TEST(itkTernaryMagnitudeSquaredImageFilterTest );
  REGISTER_TEST(itkThresholdImageFilterTest );
  REGISTER_TEST(itkThresholdLabelerImageFilterTest );
  REGISTER_TEST(itkTileImageFilterTest );
  REGISTER_TEST(itkTobogganImageFilterTest );
  REGISTER_TEST(itkTopHatImageFilterTest );
  REGISTER_TEST(itkTransformMeshFilterTest );
  REGISTER_TEST(itkTriangleMeshToSimplexMeshFilter2Test);
  REGISTER_TEST(itkTriangleMeshToSimplexMeshFilterTest);
  REGISTER_TEST(itkTwoOutputExampleImageFilterTest );
}

