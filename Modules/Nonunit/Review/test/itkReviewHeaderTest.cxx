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

#include <iostream>

#include "itkAreaClosingImageFilter.h"
#include "itkAreaOpeningImageFilter.h"
#include "itkAutumnColormapFunction.hxx"
#include "itkBlueColormapFunction.hxx"
#include "itkBoxMeanImageFilter.hxx"
#include "itkBoxSigmaImageFilter.hxx"
#include "itkBoxUtilities.h"
#include "itkBruker2DSEQImageIO.h"
#include "itkBruker2DSEQImageIOFactory.h"
#include "itkBSplineControlPointImageFilter.hxx"
#include "itkBSplineControlPointImageFunction.hxx"
#include "itkBSplineDeformableTransformInitializer.hxx"
#include "itkBSplineScatteredDataPointSetToImageFilter.hxx"
#include "itkColormapFunction.h"
#include "itkComplexBSplineInterpolateImageFunction.hxx"
#include "itkCompositeTransform.hxx"
#include "itkConformalFlatteningMeshFilter.hxx"
#include "itkConstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkContourExtractor2DImageFilter.hxx"
#include "itkConvolutionImageFilter.hxx"
#include "itkCoolColormapFunction.hxx"
#include "itkCopperColormapFunction.hxx"
#include "itkCoxDeBoorBSplineKernelFunction.hxx"
#include "itkCustomColormapFunction.hxx"
#include "itkDeformationFieldTransform.hxx"
#include "itkDiffeomorphicDemonsRegistrationFilter.hxx"
#include "itkDirectFourierReconstructionImageToImageFilter.hxx"
#include "itkDiscreteGaussianDerivativeImageFilter.hxx"
#include "itkDiscreteGaussianDerivativeImageFunction.hxx"
#include "itkDiscreteGradientMagnitudeGaussianImageFunction.hxx"
#include "itkDiscreteHessianGaussianImageFunction.hxx"
#include "itkESMDemonsRegistrationFunction.hxx"
#include "itkExponentialDeformationFieldImageFilter.hxx"
#include "itkFastApproximateRankImageFilter.h"
#include "itkFastSymmetricForcesDemonsRegistrationFilter.hxx"
#include "itkGaborImageSource.hxx"
#include "itkGaborKernelFunction.h"
#include "itkGaussianDerivativeOperator.hxx"
#include "itkGenericUtilities.h"
#include "itkGreenColormapFunction.hxx"
#include "itkGreyColormapFunction.hxx"
#include "itkGridForwardWarpImageFilter.hxx"
#include "itkGridImageSource.hxx"
#include "itkHessianToObjectnessMeasureImageFilter.hxx"
#include "itkHotColormapFunction.hxx"
#include "itkHSVColormapFunction.hxx"
#include "itkImageKernelOperator.hxx"
#include "itkImageToPathFilter.hxx"
#include "itkJPEG2000ImageIO.h"
#include "itkJPEG2000ImageIOFactory.h"
#include "itkKappaSigmaThresholdImageCalculator.hxx"
#include "itkKappaSigmaThresholdImageFilter.hxx"
#include "itkLabelGeometryImageFilter.hxx"
#include "itkMagnitudeAndPhaseToComplexImageFilter.h"
#include "itkMaskedMovingHistogramImageFilter.hxx"
#include "itkMaskedRankImageFilter.hxx"
#include "itkMiniPipelineSeparableImageFilter.hxx"
#include "itkMorphologicalWatershedFromMarkersImageFilter.hxx"
#include "itkMorphologicalWatershedImageFilter.hxx"
#include "itkMRCHeaderObject.h"
#include "itkMRCImageIO.h"
#include "itkMRCImageIOFactory.h"
#include "itkMultiphaseDenseFiniteDifferenceImageFilter.hxx"
#include "itkMultiphaseFiniteDifferenceImageFilter.hxx"
#include "itkMultiphaseSparseFiniteDifferenceImageFilter.hxx"
#include "itkMultiScaleHessianBasedMeasureImageFilter.hxx"
#include "itkN4MRIBiasFieldCorrectionImageFilter.hxx"
#include "itkNeuralNetworkFileReader.hxx"
#include "itkNeuralNetworkFileWriter.hxx"
#include "itkObjectToObjectMetric.hxx"
#include "itkOverUnderColormapFunction.hxx"
#include "itkRankHistogram.h"
#include "itkRankImageFilter.hxx"
#include "itkRedColormapFunction.hxx"
#include "itkRegionalMaximaImageFilter.hxx"
#include "itkRegionalMinimaImageFilter.hxx"
#include "itkRegionBasedLevelSetFunction.hxx"
#include "itkRegionBasedLevelSetFunctionData.hxx"
#include "itkRegionBasedLevelSetFunctionSharedData.h"
#include "itkRobustAutomaticThresholdCalculator.hxx"
#include "itkRobustAutomaticThresholdImageFilter.hxx"
#include "itkScalarChanAndVeseDenseLevelSetImageFilter.hxx"
#include "itkScalarChanAndVeseLevelSetFunction.hxx"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkScalarChanAndVeseSparseLevelSetImageFilter.hxx"
#include "itkScalarRegionBasedLevelSetFunction.hxx"
#include "itkScalarToRGBColormapImageFilter.hxx"
#include "itkShapedFloodFilledFunctionConditionalConstIterator.hxx"
#include "itkShapedFloodFilledImageFunctionConditionalConstIterator.hxx"
#include "itkShapedFloodFilledImageFunctionConditionalIterator.h"
#include "itkSpringColormapFunction.hxx"
#include "itkStochasticFractalDimensionImageFilter.hxx"
#include "itkSummerColormapFunction.hxx"
#include "itkTransformToDeformationFieldSource.hxx"
#include "itkUnconstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkValuedRegionalExtremaImageFilter.hxx"
#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkVectorCentralDifferenceImageFunction.hxx"
#include "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction.hxx"
#include "itkVoxBoCUBImageIO.h"
#include "itkVoxBoCUBImageIOFactory.h"
#include "itkWarpHarmonicEnergyCalculator.hxx"
#include "itkWinterColormapFunction.hxx"

int itkReviewHeaderTest ( int , char * [] )
{
  return EXIT_SUCCESS;
}
