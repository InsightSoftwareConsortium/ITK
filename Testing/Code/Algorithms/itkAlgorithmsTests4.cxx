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

#include "itkConfigure.h"
#include "vnl/vnl_sample.h"
#include "itkTestMain.h"


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkCollidingFrontsImageFilterTest );
  REGISTER_TEST(itkBayesianClassifierImageFilterTest );
  REGISTER_TEST(itkBinaryMedialNodeMetricTest); //Test disabled when centerd coordinates not used
  REGISTER_TEST(itkCurvesLevelSetImageFilterZeroSigmaTest );
  REGISTER_TEST(itkFastMarchingUpwindGradientTest );
  REGISTER_TEST(itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest );
  REGISTER_TEST(itkLabelVotingImageFilterTest );
  REGISTER_TEST(itkNarrowBandCurvesLevelSetImageFilterTest );
  REGISTER_TEST(itkVectorThresholdSegmentationLevelSetImageFilterTest );
  REGISTER_TEST(itkWatershedImageFilterTest );
  REGISTER_TEST(itkVoronoiPartitioningImageFilterTest );
  REGISTER_TEST(itkVnlFFTTest);
#if defined(USE_FFTWF)
  REGISTER_TEST(itkFFTWF_FFTTest);
  REGISTER_TEST(itkVnlFFTWF_FFTTest);
#endif
#if defined(USE_FFTWD)
  REGISTER_TEST(itkFFTWD_FFTTest);
  REGISTER_TEST(itkVnlFFTWD_FFTTest);
#endif
#if defined(USE_FFTWD)
  REGISTER_TEST(itkCurvatureRegistrationFilterTest);
#endif
  REGISTER_TEST(itkMeanReciprocalSquareDifferencePointSetToImageMetricTest );
  REGISTER_TEST(itkMeanReciprocalSquareDifferencePointSetToImageMetricTest );
  REGISTER_TEST(itkThresholdMaximumConnectedComponentsImageFilterTest );
  REGISTER_TEST(itkScalarImageKmeansImageFilter3DTest );
  REGISTER_TEST(itkLevelSetMotionRegistrationFilterTest );
  REGISTER_TEST(itkVoronoiSegmentationRGBImageFilterTest );
  REGISTER_TEST(itkBinaryMask3DQuadEdgeMeshSourceTest);
  REGISTER_TEST(itkAutomaticTopologyQuadEdgeMeshSourceTest);
  REGISTER_TEST(itkRegularSphereQuadEdgeMeshSourceTest);
  REGISTER_TEST(itkQuadEdgeMeshLinearParameterizationTest);
  REGISTER_TEST(itkQuadEdgeMeshCleanFilterTest);
  REGISTER_TEST(itkQuadEdgeMeshLinearParameterizationTest);
  REGISTER_TEST(itkQuadEdgeMeshSquaredEdgeLengthDecimationTest);
  REGISTER_TEST(itkQuadEdgeMeshQuadricDecimationTest);
  REGISTER_TEST(itkQuadEdgeMeshGaussianCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshMeanCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshMaxCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshMinCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshDelaunayConformingFilterTest);
  REGISTER_TEST(itkQuadEdgeMeshNormalFilterTest);
  REGISTER_TEST(itkQuadEdgeMeshSmoothingTest);
}

