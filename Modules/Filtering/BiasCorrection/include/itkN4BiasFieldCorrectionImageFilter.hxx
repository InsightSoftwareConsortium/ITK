/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkN4BiasFieldCorrectionImageFilter_hxx
#define itkN4BiasFieldCorrectionImageFilter_hxx


#include "itkAddImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkImageBufferRange.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkIterationReporter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"

ITK_GCC_PRAGMA_PUSH
ITK_GCC_SUPPRESS_Wfloat_equal
#include "itkPocketFFTCommon.h"
#include "vnl/vnl_complex_traits.h"
#include "complex"
#include "itkPrintHelper.h"
#include <algorithm> // For clamp and copy.
#include <utility>
ITK_GCC_PRAGMA_POP

namespace itk
{

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
template <typename TImage>
auto
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::MakeInputView(const TImage * image) ->
  typename TImage::Pointer
{
  auto view = TImage::New();
  view->Graft(image);
  return view;
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::N4BiasFieldCorrectionImageFilter()
  : m_MaskLabel(NumericTraits<MaskPixelType>::OneValue())
  , m_CurrentConvergenceMeasurement(RealType{})
  , m_LogBiasFieldControlPointLattice(nullptr)
{
  // implicit:
  // #0 "Primary" required

  // #1 "MaskImage" optional
  Self::AddOptionalInputName("MaskImage", 1);

  // #2 "ConfidenceImage" optional
  Self::AddOptionalInputName("ConfidenceImage", 2);

  this->SetNumberOfRequiredInputs(1);
  this->m_NumberOfFittingLevels.Fill(1);
  this->m_NumberOfControlPoints.Fill(4);

  this->m_MaximumNumberOfIterations.SetSize(1);
  this->m_MaximumNumberOfIterations.Fill(50);
}


template <typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::EnlargeOutputRequestedRegion(
  DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);

  if (output != nullptr)
  {
    output->SetRequestedRegionToLargestPossibleRegion();
  }
}


template <typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::GenerateData()
{
  this->AllocateOutputs();

  const InputImageType * inputImage = this->GetInput();
  using RegionType = typename InputImageType::RegionType;
  const RegionType inputRegion = inputImage->GetBufferedRegion();

  const typename InputImageType::SizeType inputImageSize = inputRegion.GetSize();

  const MaskImageType * const maskImage = GetMaskImage();

  if ((maskImage != nullptr) && (maskImage->GetBufferedRegion().GetSize() != inputImageSize))
  {
    itkExceptionStringMacro("If a mask image is specified, its size should be equal to the input image size");
  }

  const RealImageType * const confidenceImage = GetConfidenceImage();

  if ((confidenceImage != nullptr) && (confidenceImage->GetBufferedRegion().GetSize() != inputImageSize))
  {
    itkExceptionStringMacro("If a confidence image is specified, its size should be equal to the input image size");
  }

  // Calculate the log of the input image.
  const RealImagePointer logInputImage = RealImageType::New();
  logInputImage->CopyInformation(inputImage);
  logInputImage->SetRegions(inputRegion);
  logInputImage->Allocate(false);

  ImageAlgorithm::Copy(inputImage, logInputImage.GetPointer(), inputRegion, inputRegion);

  const auto          maskImageBufferRange = MakeImageBufferRange(maskImage);
  const auto          confidenceImageBufferRange = MakeImageBufferRange(confidenceImage);
  const MaskPixelType maskLabel = this->GetMaskLabel();
  const bool          useMaskLabel = this->GetUseMaskLabel();

  const ImageBufferRange logInputImageBufferRange{ *logInputImage };
  const size_t           numberOfPixels = logInputImageBufferRange.size();

  FitPoints fitPoints;

  for (size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
  {
    if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
         (!useMaskLabel && maskImageBufferRange[indexValue] != MaskPixelType{})) &&
        (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
    {
      fitPoints.bufferIndices.push_back(indexValue);
      auto && logInputPixel = logInputImageBufferRange[indexValue];

      if (logInputPixel > typename InputImageType::PixelType{})
      {
        logInputPixel = std::log(static_cast<RealType>(logInputPixel));
      }
    }
  }

  // The B-spline fit works in parametric space, hence the identity direction.
  typename RealImageType::DirectionType identity;
  identity.SetIdentity();

  const RealImagePointer parametricImage = RealImageType::New();
  parametricImage->SetRegions(inputRegion);
  parametricImage->SetOrigin(inputImage->GetOrigin());
  parametricImage->SetSpacing(inputImage->GetSpacing());
  parametricImage->SetDirection(identity);

  const size_t numberOfIncludedPixels = fitPoints.bufferIndices.size();

  fitPoints.points = PointSetType::PointsContainer::New();
  auto & pointSTLContainer = fitPoints.points->CastToSTLContainer();
  pointSTLContainer.resize(numberOfIncludedPixels);
  fitPoints.values = PointSetType::PointDataContainer::New();
  fitPoints.values->CastToSTLContainer().resize(numberOfIncludedPixels);

  fitPoints.weights = BSplineFilterType::WeightsContainerType::New();
  auto & weightSTLContainer = fitPoints.weights->CastToSTLContainer();
  weightSTLContainer.resize(numberOfIncludedPixels);

  for (size_t k = 0; k < numberOfIncludedPixels; ++k)
  {
    const SizeValueType indexValue = fitPoints.bufferIndices[k];
    parametricImage->TransformIndexToPhysicalPoint(parametricImage->ComputeIndex(indexValue), pointSTLContainer[k]);

    RealType confidenceWeight = 1.0;
    if (!confidenceImageBufferRange.empty())
    {
      confidenceWeight = confidenceImageBufferRange[indexValue];
    }
    weightSTLContainer[k] = confidenceWeight;
  }

  // Duplicate logInputImage since we reuse the original at each iteration.

  using DuplicatorType = ImageDuplicator<RealImageType>;
  auto duplicator = DuplicatorType::New();
  duplicator->SetInputImage(logInputImage);
  duplicator->Update();

  const RealImagePointer logUncorrectedImage = duplicator->GetOutput();

  const RealImagePointer residualBiasField = RealImageType::New();
  residualBiasField->CopyInformation(inputImage);
  residualBiasField->SetRegions(inputRegion);
  residualBiasField->Allocate(false);

  // The new estimate is reconstructed into the buffer that held the estimate before the previous one.
  RealImagePointer newLogBiasField = RealImageType::New();
  newLogBiasField->CopyInformation(inputImage);
  newLogBiasField->SetRegions(inputRegion);
  newLogBiasField->Allocate(false);

  // Provide an initial log bias field of zeros

  RealImagePointer logBiasField = RealImageType::New();
  logBiasField->CopyInformation(inputImage);
  logBiasField->SetRegions(inputRegion);
  logBiasField->AllocateInitialized();

  const RealImagePointer logSharpenedImage = RealImageType::New();
  logSharpenedImage->CopyInformation(inputImage);
  logSharpenedImage->SetRegions(inputRegion);
  logSharpenedImage->Allocate(false);

  // Iterate until convergence or iterative exhaustion.
  unsigned int maximumNumberOfLevels = 1;
  for (unsigned int d = 0; d < this->m_NumberOfFittingLevels.Size(); ++d)
  {
    if (this->m_NumberOfFittingLevels[d] > maximumNumberOfLevels)
    {
      maximumNumberOfLevels = this->m_NumberOfFittingLevels[d];
    }
  }
  if (this->m_MaximumNumberOfIterations.Size() != maximumNumberOfLevels)
  {
    itkExceptionStringMacro("Number of iteration levels is not equal to the max number of levels.");
  }

  for (this->m_CurrentLevel = 0; this->m_CurrentLevel < maximumNumberOfLevels; this->m_CurrentLevel++)
  {
    IterationReporter reporter(this, 0, 1);

    this->m_ElapsedIterations = 0;
    this->m_CurrentConvergenceMeasurement = NumericTraits<RealType>::max();
    while (this->m_ElapsedIterations++ < this->m_MaximumNumberOfIterations[this->m_CurrentLevel] &&
           this->m_CurrentConvergenceMeasurement > this->m_ConvergenceThreshold)
    {
      // Sharpen the current estimate of the uncorrected image.
      this->SharpenImage(logUncorrectedImage, logSharpenedImage);

      {
        const RealType * const uncorrected = logUncorrectedImage->GetBufferPointer();
        const RealType * const sharpened = logSharpenedImage->GetBufferPointer();
        RealType * const       residual = residualBiasField->GetBufferPointer();
        this->GetMultiThreader()->ParallelizeArray(
          0,
          numberOfPixels,
          [uncorrected, sharpened, residual](SizeValueType k) { residual[k] = uncorrected[k] - sharpened[k]; },
          nullptr);
      }

      // Smooth the residual bias field estimate and add the resulting
      // control point grid to get the new total bias field estimate.

      this->UpdateBiasFieldEstimate(residualBiasField, fitPoints, newLogBiasField);

      this->m_CurrentConvergenceMeasurement = this->CalculateConvergenceMeasurement(logBiasField, newLogBiasField);
      std::swap(logBiasField, newLogBiasField);

      {
        const RealType * const logInput = logInputImage->GetBufferPointer();
        const RealType * const bias = logBiasField->GetBufferPointer();
        RealType * const       uncorrected = logUncorrectedImage->GetBufferPointer();
        this->GetMultiThreader()->ParallelizeArray(
          0,
          numberOfPixels,
          [logInput, bias, uncorrected](SizeValueType k) { uncorrected[k] = logInput[k] - bias[k]; },
          nullptr);
      }

      reporter.CompletedStep();
    }

    const auto reconstructer = this->MakeBiasFieldReconstructer(this->m_LogBiasFieldControlPointLattice);

    auto numberOfLevels = MakeFilled<typename BiasFieldReconstructerType::ArrayType>(1);
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      if (this->m_NumberOfFittingLevels[d] + 1 >= this->m_CurrentLevel &&
          this->m_CurrentLevel != maximumNumberOfLevels - 1)
      {
        numberOfLevels[d] = 2;
      }
    }
    this->m_LogBiasFieldControlPointLattice = reconstructer->RefineControlPointLattice(numberOfLevels);
  }

  using CustomBinaryFilter = itk::BinaryGeneratorImageFilter<InputImageType, RealImageType, OutputImageType>;
  auto expAndDivFilter = CustomBinaryFilter::New();
  auto expAndDivLambda = [](const typename InputImageType::PixelType & input,
                            const typename RealImageType::PixelType &  biasField) ->
    typename OutputImageType::PixelType {
      return static_cast<typename OutputImageType::PixelType>(input / std::exp(biasField));
    };
  expAndDivFilter->SetFunctor(expAndDivLambda);
  expAndDivFilter->SetInput1(inputImage);
  expAndDivFilter->SetInput2(logBiasField);
  expAndDivFilter->Update();

  this->GraftOutput(expAndDivFilter->GetOutput());
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::SharpenImage(
  const RealImageType * unsharpenedImage,
  RealImageType *       sharpenedImage) const
{
  const auto          maskImageBufferRange = MakeImageBufferRange(this->GetMaskImage());
  const auto          confidenceImageBufferRange = MakeImageBufferRange(this->GetConfidenceImage());
  const MaskPixelType maskLabel = this->GetMaskLabel();
  const bool          useMaskLabel = this->GetUseMaskLabel();

  // Build the histogram for the uncorrected image.  Store copy
  // in a vnl_vector to utilize vnl FFT routines.  Note that variables
  // in real space are denoted by a single uppercase letter whereas their
  // frequency counterparts are indicated by a trailing lowercase 'f'.

  RealType binMaximum = NumericTraits<RealType>::NonpositiveMin();
  RealType binMinimum = NumericTraits<RealType>::max();

  const auto   unsharpenedImageBufferRange = MakeImageBufferRange(unsharpenedImage);
  const size_t numberOfPixels = unsharpenedImageBufferRange.size();

  for (size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
  {
    if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
         (!useMaskLabel && maskImageBufferRange[indexValue] != MaskPixelType{})) &&
        (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
    {
      const RealType pixel = unsharpenedImageBufferRange[indexValue];
      if (pixel > binMaximum)
      {
        binMaximum = pixel;
      }
      if (pixel < binMinimum)
      {
        binMinimum = pixel;
      }
    }
  }
  if (binMaximum <= binMinimum)
  {
    // Degenerate intensity range: sharpening is an identity mapping.
    const ImageBufferRange identityImageBufferRange{ *sharpenedImage };
    std::copy(
      unsharpenedImageBufferRange.cbegin(), unsharpenedImageBufferRange.cend(), identityImageBufferRange.begin());
    return;
  }
  const RealType histogramSlope = (binMaximum - binMinimum) / static_cast<RealType>(this->m_NumberOfHistogramBins - 1);

  // Create the intensity profile (within the masked region, if applicable)
  // using a triangular parzen windowing scheme.

  vnl_vector<RealType> H(this->m_NumberOfHistogramBins, 0.0);

  for (size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
  {
    if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
         (!useMaskLabel && maskImageBufferRange[indexValue] != MaskPixelType{})) &&
        (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
    {
      const RealType pixel = unsharpenedImageBufferRange[indexValue];

      const RealType     cidx = (static_cast<RealType>(pixel) - binMinimum) / histogramSlope;
      const unsigned int idx = itk::Math::floor(cidx);
      const RealType     offset = cidx - static_cast<RealType>(idx);

      if (offset == 0.0)
      {
        H[idx] += 1.0;
      }
      else if (idx < this->m_NumberOfHistogramBins - 1)
      {
        H[idx] += 1.0 - offset;
        H[idx + 1] += offset;
      }
    }
  }

  // Determine information about the intensity histogram and zero-pad
  // histogram to a power of 2.

  const RealType exponent =
    std::ceil(std::log(static_cast<RealType>(this->m_NumberOfHistogramBins)) / std::log(2.0)) + 1;
  auto paddedHistogramSize = static_cast<unsigned int>(std::pow(static_cast<RealType>(2.0), exponent) + 0.5);
  auto histogramOffset = static_cast<unsigned int>(0.5 * (paddedHistogramSize - this->m_NumberOfHistogramBins));

  using FFTComputationType = double;
  using FFTComplexType = std::complex<FFTComputationType>;

  vnl_vector<FFTComplexType> V(paddedHistogramSize, FFTComplexType(0.0, 0.0));

  for (unsigned int n = 0; n < this->m_NumberOfHistogramBins; ++n)
  {
    V[n + histogramOffset] = H[n];
  }

  vnl_vector<FFTComplexType> Vf(V);

  PocketFFTCommon::Transform1D(Vf.data_block(), paddedHistogramSize, false, FFTComputationType{ 1 });

  // Create the Gaussian filter.

  const RealType scaledFWHM = this->m_BiasFieldFullWidthAtHalfMaximum / histogramSlope;
  const RealType expFactor = 4.0 * std::log(2.0) / itk::Math::sqr(scaledFWHM);
  const RealType scaleFactor = 2.0 * std::sqrt(std::log(2.0) / itk::Math::pi) / scaledFWHM;

  vnl_vector<FFTComplexType> F(paddedHistogramSize, FFTComplexType(0.0, 0.0));

  F[0] = FFTComplexType(scaleFactor, 0.0);
  auto halfSize = static_cast<unsigned int>(0.5 * paddedHistogramSize);
  for (unsigned int n = 1; n <= halfSize; ++n)
  {
    F[n] = F[paddedHistogramSize - n] =
      FFTComplexType(scaleFactor * std::exp(-itk::Math::sqr(static_cast<RealType>(n)) * expFactor), 0.0);
  }
  if (paddedHistogramSize % 2 == 0)
  {
    F[halfSize] = FFTComplexType(
      scaleFactor * std::exp(0.25 * -itk::Math::sqr(static_cast<RealType>(paddedHistogramSize)) * expFactor), 0.0);
  }

  vnl_vector<FFTComplexType> Ff(F);

  PocketFFTCommon::Transform1D(Ff.data_block(), paddedHistogramSize, false, FFTComputationType{ 1 });

  // Create the Wiener deconvolution filter.

  vnl_vector<FFTComplexType> Gf(paddedHistogramSize);

  const auto wienerNoiseValue = static_cast<FFTComputationType>(this->m_WienerFilterNoise);
  for (unsigned int n = 0; n < paddedHistogramSize; ++n)
  {
    const FFTComplexType c = vnl_complex_traits<FFTComplexType>::conjugate(Ff[n]);
    Gf[n] = c / (c * Ff[n] + wienerNoiseValue);
  }

  vnl_vector<FFTComplexType> Uf(paddedHistogramSize);

  for (unsigned int n = 0; n < paddedHistogramSize; ++n)
  {
    Uf[n] = Vf[n] * Gf[n].real();
  }

  vnl_vector<FFTComplexType> U(Uf);

  PocketFFTCommon::Transform1D(U.data_block(), paddedHistogramSize, true, FFTComputationType{ 1 });
  for (unsigned int n = 0; n < paddedHistogramSize; ++n)
  {
    U[n] = FFTComplexType(std::max(U[n].real(), static_cast<FFTComputationType>(0.0)), 0.0);
  }

  // Compute mapping E(u|v).

  vnl_vector<FFTComplexType> numerator(paddedHistogramSize);

  for (unsigned int n = 0; n < paddedHistogramSize; ++n)
  {
    numerator[n] =
      FFTComplexType((binMinimum + (static_cast<RealType>(n) - histogramOffset) * histogramSlope) * U[n].real(), 0.0);
  }
  PocketFFTCommon::Transform1D(numerator.data_block(), paddedHistogramSize, false, FFTComputationType{ 1 });
  for (unsigned int n = 0; n < paddedHistogramSize; ++n)
  {
    numerator[n] *= Ff[n];
  }
  PocketFFTCommon::Transform1D(numerator.data_block(), paddedHistogramSize, true, FFTComputationType{ 1 });

  vnl_vector<FFTComplexType> denominator(U);

  PocketFFTCommon::Transform1D(denominator.data_block(), paddedHistogramSize, false, FFTComputationType{ 1 });
  for (unsigned int n = 0; n < paddedHistogramSize; ++n)
  {
    denominator[n] *= Ff[n];
  }
  PocketFFTCommon::Transform1D(denominator.data_block(), paddedHistogramSize, true, FFTComputationType{ 1 });

  vnl_vector<RealType> E(paddedHistogramSize);

  for (unsigned int n = 0; n < paddedHistogramSize; ++n)
  {
    if (denominator[n].real() != 0.0)
    {
      E[n] = numerator[n].real() / denominator[n].real();
    }
    else
    {
      E[n] = 0.0;
    }
  }

  // Remove the zero-padding from the mapping.

  E = E.extract(this->m_NumberOfHistogramBins, histogramOffset);

  // Sharpen the image with the new mapping, E(u|v)
  sharpenedImage->FillBuffer(0);

  const ImageBufferRange sharpenedImageBufferRange{ *sharpenedImage };

  for (size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
  {
    if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
         (!useMaskLabel && maskImageBufferRange[indexValue] != MaskPixelType{})) &&
        (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
    {
      const RealType     cidx = (unsharpenedImageBufferRange[indexValue] - binMinimum) / histogramSlope;
      const unsigned int idx = itk::Math::floor(cidx);

      RealType correctedPixel = 0;
      if (idx < E.size() - 1)
      {
        correctedPixel = E[idx] + (E[idx + 1] - E[idx]) * (cidx - static_cast<RealType>(idx));
      }
      else
      {
        correctedPixel = E.back();
      }
      sharpenedImageBufferRange[indexValue] = correctedPixel;
    }
  }
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::UpdateBiasFieldEstimate(
  RealImageType * fieldEstimate,
  FitPoints &     fitPoints,
  RealImageType * smoothField)
{
  auto &       values = fitPoints.values->CastToSTLContainer();
  const auto & bufferIndices = fitPoints.bufferIndices;
  const auto   fieldRange = MakeImageBufferRange(fieldEstimate);
  this->GetMultiThreader()->ParallelizeArray(
    0,
    bufferIndices.size(),
    [&values, &bufferIndices, &fieldRange](SizeValueType k) { values[k][0] = fieldRange[bufferIndices[k]]; },
    nullptr);

  // A fresh point set per fit, since releasing the fit's input drops only its container pointers.
  const PointSetPointer fieldPoints = PointSetType::New();
  fieldPoints->SetPoints(fitPoints.points);
  fieldPoints->SetPointData(fitPoints.values);

  auto bspliner = BSplineFilterType::New();

  typename BSplineFilterType::ArrayType numberOfControlPoints;
  auto                                  numberOfFittingLevels = MakeFilled<typename BSplineFilterType::ArrayType>(1);
  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    if (!this->m_LogBiasFieldControlPointLattice)
    {
      numberOfControlPoints[d] = this->m_NumberOfControlPoints[d];
    }
    else
    {
      numberOfControlPoints[d] = this->m_LogBiasFieldControlPointLattice->GetLargestPossibleRegion().GetSize()[d];
    }
  }

  typename ScalarImageType::PointType parametricOrigin = fieldEstimate->GetOrigin();
  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    parametricOrigin[d] += (fieldEstimate->GetSpacing()[d] * fieldEstimate->GetLargestPossibleRegion().GetIndex()[d]);
  }
  bspliner->SetOrigin(parametricOrigin);
  bspliner->SetSpacing(fieldEstimate->GetSpacing());
  bspliner->SetSize(fieldEstimate->GetLargestPossibleRegion().GetSize());
  bspliner->SetDirection(fieldEstimate->GetDirection());
  bspliner->SetGenerateOutputImage(false);
  bspliner->SetNumberOfLevels(numberOfFittingLevels);
  bspliner->SetSplineOrder(this->m_SplineOrder);
  bspliner->SetNumberOfControlPoints(numberOfControlPoints);
  bspliner->SetInput(fieldPoints);
  bspliner->SetPointWeights(fitPoints.weights);
  bspliner->Update();

  const typename BiasFieldControlPointLatticeType::Pointer phiLattice = bspliner->GetPhiLattice();

  // Add the bias field control points to the current estimate.

  if (!this->m_LogBiasFieldControlPointLattice)
  {
    this->m_LogBiasFieldControlPointLattice = phiLattice;
  }
  else
  {
    // Ensure that the two lattices occupy the same physical space.  Not
    // necessary for performance since the parameters of the reconstructed
    // bias field are specified later in this function in the reconstructer.
    phiLattice->CopyInformation(this->m_LogBiasFieldControlPointLattice);

    using AdderType = AddImageFilter<BiasFieldControlPointLatticeType,
                                     BiasFieldControlPointLatticeType,
                                     BiasFieldControlPointLatticeType>;
    auto adder = AdderType::New();
    adder->SetInput1(this->m_LogBiasFieldControlPointLattice);
    adder->SetInput2(phiLattice);
    adder->Update();

    this->m_LogBiasFieldControlPointLattice = adder->GetOutput();
  }

  this->ReconstructBiasFieldInto(this->m_LogBiasFieldControlPointLattice, smoothField);
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
typename N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealImagePointer
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::ReconstructBiasField(
  const BiasFieldControlPointLatticeType * controlPointLattice)
{
  // The view made below has no source, so bring a pipeline output up to date first.
  controlPointLattice->UpdateSource();
  const auto reconstructer = this->MakeBiasFieldReconstructer(controlPointLattice);

  const typename ScalarImageType::Pointer biasFieldBsplineImage = reconstructer->GetOutput();
  biasFieldBsplineImage->Update();

  using SelectorType = VectorIndexSelectionCastImageFilter<ScalarImageType, RealImageType>;
  auto selector = SelectorType::New();
  selector->SetInput(biasFieldBsplineImage);
  selector->SetIndex(0);

  RealImagePointer biasField = selector->GetOutput();
  biasField->Update();

  biasField->DisconnectPipeline();

  return biasField;
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
auto
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::MakeBiasFieldReconstructer(
  const BiasFieldControlPointLatticeType * controlPointLattice) const -> typename BiasFieldReconstructerType::Pointer
{
  const InputImageType * inputImage = this->GetInput();

  auto reconstructer = BiasFieldReconstructerType::New();
  // A view, so that releasing the reconstructer's input leaves the lattice buffered.
  reconstructer->SetInput(MakeInputView(controlPointLattice));
  reconstructer->SetOrigin(inputImage->GetOrigin());
  reconstructer->SetSpacing(inputImage->GetSpacing());
  reconstructer->SetDirection(inputImage->GetDirection());
  reconstructer->SetSplineOrder(this->m_SplineOrder);
  reconstructer->SetSize(inputImage->GetLargestPossibleRegion().GetSize());
  return reconstructer;
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::ReconstructBiasFieldInto(
  const BiasFieldControlPointLatticeType * controlPointLattice,
  RealImageType *                          biasField)
{
  const auto reconstructer = this->MakeBiasFieldReconstructer(controlPointLattice);
  reconstructer->Update();

  const ScalarType * const reconstructed = reconstructer->GetOutput()->GetBufferPointer();
  RealType * const         field = biasField->GetBufferPointer();
  this->GetMultiThreader()->ParallelizeArray(
    0,
    biasField->GetBufferedRegion().GetNumberOfPixels(),
    [reconstructed, field](SizeValueType k) { field[k] = reconstructed[k][0]; },
    nullptr);
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
typename N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealType
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::CalculateConvergenceMeasurement(
  const RealImageType * fieldEstimate1,
  const RealImageType * fieldEstimate2) const
{
  // Calculate statistics over the mask region

  RealType mu = 0.0;
  RealType sigma = 0.0;
  RealType N = 0.0;

  const auto          maskImageBufferRange = MakeImageBufferRange(this->GetMaskImage());
  const auto          confidenceImageBufferRange = MakeImageBufferRange(this->GetConfidenceImage());
  const MaskPixelType maskLabel = this->GetMaskLabel();
  const bool          useMaskLabel = this->GetUseMaskLabel();

  const auto   fieldEstimate1BufferRange = MakeImageBufferRange(fieldEstimate1);
  const auto   fieldEstimate2BufferRange = MakeImageBufferRange(fieldEstimate2);
  const size_t numberOfPixels = fieldEstimate1BufferRange.size();

  for (size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
  {
    if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
         (!useMaskLabel && maskImageBufferRange[indexValue] != MaskPixelType{})) &&
        (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
    {
      const RealType pixel =
        std::exp(static_cast<RealType>(fieldEstimate1BufferRange[indexValue] - fieldEstimate2BufferRange[indexValue]));
      N += 1.0;

      if (N > 1.0)
      {
        sigma = sigma + itk::Math::sqr(pixel - mu) * (N - 1.0) / N;
      }
      mu = mu * (1.0 - 1.0 / N) + pixel / N;
    }
  }
  sigma = std::sqrt(sigma / (N - 1.0));

  return sigma / mu;
}

template <typename TInputImage, typename TMaskImage, typename TOutputImage>
void
N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                   Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Use Mask Label: " << m_UseMaskLabel << std::endl;
  print_helper::PrintNumericTrait(os, indent, "Mask label", this->m_MaskLabel);
  os << indent << "Number of histogram bins: " << this->m_NumberOfHistogramBins << std::endl;
  os << indent << "Wiener filter noise: " << this->m_WienerFilterNoise << std::endl;
  os << indent << "Bias field FWHM: " << this->m_BiasFieldFullWidthAtHalfMaximum << std::endl;
  os << indent << "Maximum number of iterations: " << this->m_MaximumNumberOfIterations << std::endl;
  os << indent << "Convergence threshold: " << this->m_ConvergenceThreshold << std::endl;
  os << indent << "Spline order: " << this->m_SplineOrder << std::endl;
  os << indent << "Number of fitting levels: " << this->m_NumberOfFittingLevels << std::endl;
  os << indent << "Number of control points: " << this->m_NumberOfControlPoints << std::endl;
  os << indent << "CurrentConvergenceMeasurement: " << this->m_CurrentConvergenceMeasurement << std::endl;
  os << indent << "CurrentLevel: " << this->m_CurrentLevel << std::endl;
  os << indent << "ElapsedIterations: " << this->m_ElapsedIterations << std::endl;
  itkPrintSelfObjectMacro(LogBiasFieldControlPointLattice);
}

} // end namespace itk

#endif
