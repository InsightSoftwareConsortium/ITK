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
// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h"
#include "itkConfigure.h"

void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkFlatStructuringElementTest);

  REGISTER_TEST(itkLabelToRGBImageFilterTest);
  REGISTER_TEST(itkLabelOverlayImageFilterTest);

  REGISTER_TEST(itkValuedRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkValuedRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest2);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest2);

  REGISTER_TEST(itkNeuralNetworkIOTest);

  REGISTER_TEST(itkConformalFlatteningMeshFilterTest);
  REGISTER_TEST(itkConformalFlatteningQuadEdgeMeshFilterTest);

  REGISTER_TEST(itkMorphologicalWatershedImageFilterTest);
  REGISTER_TEST(itkMorphologicalWatershedFromMarkersImageFilterTest);

  REGISTER_TEST(itkBinaryMorphologicalClosingImageFilterTest);
  REGISTER_TEST(itkBinaryMorphologicalOpeningImageFilterTest);

  REGISTER_TEST(itkOptImageToImageMetricsTest);

  REGISTER_TEST(itkOptImageToImageMetricsTest2);

  REGISTER_TEST(itkOptMattesMutualInformationImageToImageMetricThreadsTest1);
  REGISTER_TEST(itkTimeAndMemoryProbeTest);

  REGISTER_TEST(itkBruker2DSEQImageIOTest);
  REGISTER_TEST(itkPhilipsRECImageIOTest);

  REGISTER_TEST(itkVoxBoCUBImageIOTest);

  REGISTER_TEST(itkSliceBySliceImageFilterTest);

  REGISTER_TEST(itkDiffeomorphicDemonsRegistrationFilterTest);
  REGISTER_TEST(itkDiffeomorphicDemonsRegistrationFilterTest2);

  REGISTER_TEST(itkDivideByConstantImageFilterTest);
  REGISTER_TEST(itkMultiplyByConstantImageFilterTest);
  REGISTER_TEST(itkAddConstantToImageFilterTest);
  REGISTER_TEST(itkSubtractConstantFromImageFilterTest);

  REGISTER_TEST( itkImageReadComplexWriteMagnitudeAndPhaseTest );
  REGISTER_TEST( itkImageReadMagnitudeAndPhaseWriteComplexTest );
  REGISTER_TEST( itkImageReadRealAndImaginaryWriteComplexTest );

#if defined( USE_FFTWF ) || defined( USE_FFTWD )
  REGISTER_TEST( itkFFTComplexToComplexImageFilterTest01 );
  REGISTER_TEST( itkFFTComplexToComplexImageFilterTest02 );
#endif

  REGISTER_TEST(
    itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest );
  REGISTER_TEST( itkVectorCentralDifferenceImageFunctionTest );
  REGISTER_TEST( itkExponentialDeformationFieldImageFilterTest );
  REGISTER_TEST( itkFastSymmetricForcesDemonsRegistrationFilterTest );
  REGISTER_TEST( itkGridForwardWarpImageFilterTest );
  REGISTER_TEST( itkWarpHarmonicEnergyCalculatorTest );

  REGISTER_TEST( itkBSplineScatteredDataPointSetToImageFilterTest );
  REGISTER_TEST( itkBSplineScatteredDataPointSetToImageFilterTest2 );
  REGISTER_TEST( itkBSplineScatteredDataPointSetToImageFilterTest3 );

  REGISTER_TEST( itkCoxDeBoorBSplineKernelFunctionTest );
  REGISTER_TEST( itkCoxDeBoorBSplineKernelFunctionTest2 );

  REGISTER_TEST( itkKappaSigmaThresholdImageCalculatorTest );
  REGISTER_TEST( itkKappaSigmaThresholdImageFilterTest );

  REGISTER_TEST( itkComposeRGBAImageFilterTest );

  REGISTER_TEST( itkTransformToDeformationFieldSourceTest );
  REGISTER_TEST( itkTransformToDeformationFieldSourceTest1 );

  REGISTER_TEST( itkGaussianDerivativeOperatorTest );
  REGISTER_TEST( itkDiscreteGaussianDerivativeImageFunctionTest );
  REGISTER_TEST( itkDiscreteGradientMagnitudeGaussianImageFunctionTest );
  REGISTER_TEST( itkDiscreteHessianGaussianImageFunctionTest );

#ifdef ITK_USE_MINC2
  REGISTER_TEST(itkMINC2ImageIOTest);
#endif


  REGISTER_TEST( itkBoxMeanImageFilterTest );
  REGISTER_TEST( itkBoxSigmaImageFilterTest );
  REGISTER_TEST( itkRankImageFilterTest );
  REGISTER_TEST( itkMapRankImageFilterTest );
  REGISTER_TEST( itkMaskedRankImageFilterTest );
  REGISTER_TEST( itkMapMaskedRankImageFilterTest );
  REGISTER_TEST( itkFastApproximateRankImageFilterTest );

  REGISTER_TEST( itkOptGrayscaleMorphologicalClosingImageFilterTest );
  REGISTER_TEST( itkOptGrayscaleMorphologicalOpeningImageFilterTest );
  REGISTER_TEST( itkOptGrayscaleDilateImageFilterTest );
  REGISTER_TEST( itkOptGrayscaleErodeImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleMorphologicalClosingImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleMorphologicalOpeningImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleDilateImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleErodeImageFilterTest );

  REGISTER_TEST( itkBinaryContourImageFilterTest );
  REGISTER_TEST( itkLabelContourImageFilterTest );

  REGISTER_TEST( itkFFTShiftImageFilterTest );

  REGISTER_TEST( itkConvolutionImageFilterTest );
  REGISTER_TEST( itkConvolutionImageFilterTestInt );

  REGISTER_TEST( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1 );
  REGISTER_TEST( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2 );
  REGISTER_TEST( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3 );

  REGISTER_TEST( itkRobustAutomaticThresholdImageFilterTest );

  REGISTER_TEST( itkAreaOpeningImageFilterTest );
  REGISTER_TEST( itkAreaClosingImageFilterTest );

  REGISTER_TEST( itkHessianToObjectnessMeasureImageFilterTest );
  REGISTER_TEST( itkMultiScaleHessianBasedMeasureImageFilterTest );
}
