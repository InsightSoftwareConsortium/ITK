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
  REGISTER_TEST(itk2DDeformableTest );
  REGISTER_TEST(itkBinaryThinningImageFilterTest );
  REGISTER_TEST(itkDeformableTest );
  REGISTER_TEST(itkGibbsTest );
  REGISTER_TEST(itkMRFImageFilterTest );
  REGISTER_TEST(itkMRIBiasFieldCorrectionFilterTest );
  REGISTER_TEST(itkMattesMutualInformationImageToImageMetricTest );
  REGISTER_TEST(itkMeanReciprocalSquareDifferenceImageMetricTest  );
  REGISTER_TEST(itkMeanSquaresHistogramImageToImageMetricTest );
  REGISTER_TEST(itkMeanSquaresImageMetricTest );
  REGISTER_TEST(itkMeanSquaresPointSetToImageMetricTest );
  REGISTER_TEST(itkMinMaxCurvatureFlowImageFilterTest );
  REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest);
  REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest_1 );
  REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest_2 );
  REGISTER_TEST(itkMultiResolutionPDEDeformableRegistrationTest );
  REGISTER_TEST(itkMultiResolutionPyramidImageFilterTest );
  REGISTER_TEST(itkMutualInformationHistogramImageToImageMetricTest );
  REGISTER_TEST(itkMutualInformationMetricTest );
  REGISTER_TEST(itkNewTest );
  REGISTER_TEST(itkNormalizedCorrelationImageMetricTest );
  REGISTER_TEST(itkNormalizedCorrelationPointSetToImageMetricTest );
  REGISTER_TEST(itkNormalizedMutualInformationHistogramImageToImageMetricTest );
  REGISTER_TEST(itkOrthogonalSwath2DPathFilterTest );
  REGISTER_TEST(itkOtsuThresholdImageCalculatorTest );
  REGISTER_TEST(itkOtsuThresholdImageFilterTest );
  REGISTER_TEST(itkPointSetToImageRegistrationTest_1  );
  REGISTER_TEST(itkRecursiveMultiResolutionPyramidImageFilterTest );
  REGISTER_TEST(itkRegionGrow2DTest );
  REGISTER_TEST(itkReinitializeLevelSetImageFilterTest );
  REGISTER_TEST(itkShapeDetectionLevelSetImageFilterTest );
  REGISTER_TEST(itkShapePriorMAPCostFunctionTest );
  REGISTER_TEST(itkShapePriorSegmentationLevelSetFunctionTest );
  REGISTER_TEST(itkSpatialObjectToImageRegistrationTest );
  REGISTER_TEST(itkSphereMeshSourceTest );
  REGISTER_TEST(itkSupervisedImageClassifierTest);
  REGISTER_TEST(itkUnsharpMaskLevelSetImageFilterTest );
}

