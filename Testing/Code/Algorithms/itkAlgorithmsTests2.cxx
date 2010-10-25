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

#include "vnl/vnl_sample.h"
#include "itkTestMain.h"


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkNarrowBandThresholdSegmentationLevelSetImageFilterTest );
  REGISTER_TEST(itkThresholdSegmentationLevelSetImageFilterTest );
  REGISTER_TEST(itkCompareHistogramImageToImageMetricTest   );
  REGISTER_TEST(itkDeformableSimplexMesh3DFilterTest   );
  REGISTER_TEST(itkDeformableSimplexMesh3DBalloonForceFilterTest  );
  REGISTER_TEST(itkDeformableSimplexMesh3DGradientConstraintForceFilterTest  );
  REGISTER_TEST(itkGradientDifferenceImageToImageMetricTest );
  REGISTER_TEST(itkKappaStatisticImageToImageMetricTest );
  REGISTER_TEST(itkKullbackLeiblerCompareHistogramImageToImageMetricTest );
  REGISTER_TEST(itkImagePCAShapeModelEstimatorTest);
  REGISTER_TEST(itkImagePCADecompositionCalculatorTest);
  REGISTER_TEST(itkIsolatedWatershedImageFilterTest );
  REGISTER_TEST(itkMIRegistrationFunctionTest );
  REGISTER_TEST(itkMatchCardinalityImageToImageMetricTest );
  REGISTER_TEST(itkOtsuMultipleThresholdsCalculatorTest );
  REGISTER_TEST(itkOtsuMultipleThresholdsImageFilterTest );
  REGISTER_TEST(itkPointSetToPointSetRegistrationTest );
  REGISTER_TEST(itkPointSetToSpatialObjectDemonsRegistrationTest );
  REGISTER_TEST(itkRegularSphereMeshSourceTest );
  REGISTER_TEST(itkRayCastInterpolateImageFunctionTest );
  REGISTER_TEST(itkSimplexMeshVolumeCalculatorTest );
  REGISTER_TEST(itkSTAPLEImageFilterTest );
  REGISTER_TEST(itkSymmetricForcesDemonsRegistrationFilterTest );
  REGISTER_TEST(itkVoronoiDiagram2DTest );
  REGISTER_TEST(itkVoronoiSegmentationImageFilterTest );
}

