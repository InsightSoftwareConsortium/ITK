/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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

  REGISTER_TEST(itkVTKPolyDataIOQuadEdgeMeshTest);
  REGISTER_TEST(itkVTKPolyDataReaderQuadEdgeMeshTest);
  REGISTER_TEST(itkVTKPolyDataReaderTest);
  REGISTER_TEST(itkVTKPolyDataWriterTest);
  REGISTER_TEST(itkVTKPolyDataWriterTest2);

  REGISTER_TEST(itkMorphologicalWatershedImageFilterTest);
  REGISTER_TEST(itkMorphologicalWatershedFromMarkersImageFilterTest);

  REGISTER_TEST(itkBinaryMorphologicalClosingImageFilterTest);
  REGISTER_TEST(itkBinaryMorphologicalOpeningImageFilterTest);

  REGISTER_TEST(itkOptImageToImageMetricsTest);

#ifdef ITK_USE_OPTIMIZED_REGISTRATION_METHODS
  REGISTER_TEST(itkOptImageToImageMetricsTest2);
#endif
  
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

  REGISTER_TEST( itkFFTComplexToComplexImageFilterTest01 );
  REGISTER_TEST( itkFFTComplexToComplexImageFilterTest02 );

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

  REGISTER_TEST( itkDiscreteGaussianDerivativeImageFunctionTest );
  REGISTER_TEST( itkDiscreteGradientMagnitudeGaussianImageFunctionTest );
  REGISTER_TEST( itkDiscreteHessianGaussianImageFunctionTest );

#ifdef ITK_USE_MINC2
  REGISTER_TEST(itkMINC2ImageIOTest);
#endif

#ifdef ITK_USE_TRANSFORM_IO_FACTORIES
  REGISTER_TEST(itkTransformFileReaderWriterTest);
#endif

  REGISTER_TEST( itkBoxMeanImageFilterTest );
  REGISTER_TEST( itkBoxSigmaImageFilterTest );
  REGISTER_TEST( itkRankImageFilterTest );
  REGISTER_TEST( itkMapRankImageFilterTest );
  REGISTER_TEST( itkMaskedRankImageFilterTest );
  REGISTER_TEST( itkMapMaskedRankImageFilterTest );
  REGISTER_TEST( itkFastApproximateRankImageFilterTest );

#ifdef ITK_USE_CONSOLIDATED_MORPHOLOGY
  REGISTER_TEST( itkOptGrayscaleMorphologicalClosingImageFilterTest );
  REGISTER_TEST( itkOptGrayscaleMorphologicalOpeningImageFilterTest );
  REGISTER_TEST( itkOptGrayscaleDilateImageFilterTest );
  REGISTER_TEST( itkOptGrayscaleErodeImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleMorphologicalClosingImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleMorphologicalOpeningImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleDilateImageFilterTest );
  REGISTER_TEST( itkMapOptGrayscaleErodeImageFilterTest );
#endif

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
