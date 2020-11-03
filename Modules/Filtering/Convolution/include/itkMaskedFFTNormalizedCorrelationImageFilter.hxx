/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMaskedFFTNormalizedCorrelationImageFilter_hxx
#define itkMaskedFFTNormalizedCorrelationImageFilter_hxx

#include "itkMaskedFFTNormalizedCorrelationImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkMultiplyImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkSqrtImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkConstantPadImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkRoundImageFilter.h"
#include "itkTernaryGeneratorImageFilter.h"

namespace itk
{

namespace Functor
{
/**
 * \class PostProcessCorrelation
 * \brief Functor to post-process the NCC result.
 * The correlation must be between -1 and 1 by definition.  But
 * numerical errors can cause the values to be large values (for
 * example, when dividing by zero).  So, we loop through the matrix
 * and set to zero all values outside of this range.
 * Also, zero-out the correlation values that arise from too few voxels
 * since they are statistically unreliable.
 */
template <typename TImage>
class PostProcessCorrelation
{
public:
  PostProcessCorrelation() { m_RequiredNumberOfOverlappingPixels = 0; }
  ~PostProcessCorrelation() = default;

  void
  SetRequiredNumberOfOverlappingPixels(SizeValueType value)
  {
    m_RequiredNumberOfOverlappingPixels = value;
  }

  void
  SetPrecisionTolerance(double value)
  {
    m_PrecisionTolerance = value;
  }

  bool
  operator!=(const PostProcessCorrelation &) const
  {
    return false;
  }

  bool
  operator==(const PostProcessCorrelation & other) const
  {
    return !(*this != other);
  }

  inline TImage
  operator()(const TImage & NCC, const TImage & denominator, const TImage & numberOfOverlapPixels) const
  {
    TImage outputValue;
    if (denominator < m_PrecisionTolerance || numberOfOverlapPixels == 0.0 ||
        numberOfOverlapPixels < m_RequiredNumberOfOverlappingPixels)
    {
      outputValue = 0.0;
    }
    else if (NCC < -1)
    {
      outputValue = -1.0;
    }
    else if (NCC > 1)
    {
      outputValue = 1.0;
    }
    else
    {
      outputValue = NCC;
    }
    return outputValue;
  }

private:
  SizeValueType m_RequiredNumberOfOverlappingPixels;
  double        m_PrecisionTolerance;
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateData()
{
  // Store the input images.
  InputImagePointer fixedImage = InputImageType::New();
  fixedImage->Graft(this->GetFixedImage());

  InputImagePointer movingImage = InputImageType::New();
  movingImage->Graft(this->GetMovingImage());

  MaskImagePointer fixedMask = nullptr;
  if (this->GetFixedImageMask())
  {
    fixedMask = MaskImageType::New();
    fixedMask->Graft(this->GetFixedImageMask());
  }

  MaskImagePointer movingMask = nullptr;
  if (this->GetMovingImageMask())
  {
    movingMask = MaskImageType::New();
    movingMask->Graft(this->GetMovingImageMask());
  }

  this->UpdateProgress(m_AccumulatedProgress);
  OutputImagePointer outputImage = this->GetOutput();

  fixedMask = this->PreProcessMask(fixedImage, fixedMask);
  movingMask = this->PreProcessMask(movingImage, movingMask);

  // The fixed and moving images need to be masked for the equations
  // below to work correctly.  The masks need to be pre-processed
  // before this step.
  fixedImage = this->PreProcessImage(fixedImage, fixedMask);
  movingImage = this->PreProcessImage(movingImage, movingMask);

  InputImagePointer rotatedMovingImage = this->RotateImage<InputImageType>(movingImage);
  movingImage = nullptr;
  MaskImagePointer rotatedMovingMask = this->RotateImage<MaskImageType>(movingMask);
  movingMask = nullptr;

  // The combinedImageSize is the size resulting from the correlation of the two images.
  RealSizeType combinedImageSize;
  // The FFTImageSize is the closest valid dimension each dimension.
  // The dimension must be divisible by a combination of 2, 3, and 5.
  InputSizeType FFTImageSize;
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    combinedImageSize[i] = fixedImage->GetLargestPossibleRegion().GetSize()[i] +
                           rotatedMovingImage->GetLargestPossibleRegion().GetSize()[i] - 1;
    FFTImageSize[i] = this->FindClosestValidDimension(combinedImageSize[i]);
  }

  // Only 6 FFTs are needed.
  // Calculate them in stages to reduce memory.
  // For the numerator, only 4 FFTs are required.
  // We could potentially calculate two forward FFTs at a time by using odd-even
  // separation.
  FFTImagePointer fixedFFT = this->CalculateForwardFFT<InputImageType, FFTImageType>(fixedImage, FFTImageSize);
  FFTImagePointer fixedMaskFFT = this->CalculateForwardFFT<MaskImageType, FFTImageType>(fixedMask, FFTImageSize);
  fixedMask = nullptr;
  FFTImagePointer rotatedMovingFFT =
    this->CalculateForwardFFT<InputImageType, FFTImageType>(rotatedMovingImage, FFTImageSize);
  FFTImagePointer rotatedMovingMaskFFT =
    this->CalculateForwardFFT<MaskImageType, FFTImageType>(rotatedMovingMask, FFTImageSize);
  rotatedMovingMask = nullptr;

  // Only 6 IFFTs are needed.
  // Compute and save some of these rather than computing them multiple times.
  // The numberOfOverlapPixels image tells how many voxels are overlapping at each location of the correlation image.
  RealImagePointer numberOfOverlapPixels = this->CalculateInverseFFT<FFTImageType, RealImageType>(
    this->ElementProduct<FFTImageType, FFTImageType>(fixedMaskFFT, rotatedMovingMaskFFT), combinedImageSize);
  // Ensure that the result is positive.
  numberOfOverlapPixels = this->ElementRound<RealImageType, RealImageType>(numberOfOverlapPixels);
  numberOfOverlapPixels = this->ElementPositive<RealImageType>(numberOfOverlapPixels);

  // Calculate the numerator of the masked FFT NCC equation.
  RealImagePointer fixedCumulativeSumImage = this->CalculateInverseFFT<FFTImageType, RealImageType>(
    this->ElementProduct<FFTImageType, FFTImageType>(fixedFFT, rotatedMovingMaskFFT), combinedImageSize);
  RealImagePointer rotatedMovingCumulativeSumImage = this->CalculateInverseFFT<FFTImageType, RealImageType>(
    this->ElementProduct<FFTImageType, FFTImageType>(fixedMaskFFT, rotatedMovingFFT), combinedImageSize);
  RealImagePointer numerator = this->ElementSubtraction<RealImageType>(
    this->CalculateInverseFFT<FFTImageType, RealImageType>(
      this->ElementProduct<FFTImageType, FFTImageType>(fixedFFT, rotatedMovingFFT), combinedImageSize),
    this->ElementQuotient<RealImageType>(
      this->ElementProduct<RealImageType, RealImageType>(fixedCumulativeSumImage, rotatedMovingCumulativeSumImage),
      numberOfOverlapPixels));
  fixedFFT = nullptr;         // No longer needed
  rotatedMovingFFT = nullptr; // No longer needed

  // Calculate the fixed part of the masked FFT NCC denominator.
  FFTImagePointer fixedSquaredFFT = this->CalculateForwardFFT<RealImageType, FFTImageType>(
    this->ElementProduct<InputImageType, RealImageType>(fixedImage, fixedImage), FFTImageSize);
  fixedImage = nullptr; // No longer needed
  RealImagePointer fixedDenom = this->ElementSubtraction<RealImageType>(
    this->CalculateInverseFFT<FFTImageType, RealImageType>(
      this->ElementProduct<FFTImageType, FFTImageType>(fixedSquaredFFT, rotatedMovingMaskFFT), combinedImageSize),
    this->ElementQuotient<RealImageType>(
      this->ElementProduct<RealImageType, RealImageType>(fixedCumulativeSumImage, fixedCumulativeSumImage),
      numberOfOverlapPixels));
  fixedSquaredFFT = nullptr;         // No longer needed
  rotatedMovingMaskFFT = nullptr;    // No longer needed
  fixedCumulativeSumImage = nullptr; // No longer needed
  // Ensure that the result is positive.
  fixedDenom = this->ElementPositive<RealImageType>(fixedDenom);

  // Calculate the moving part of the masked FFT NCC denominator.
  FFTImagePointer rotatedMovingSquaredFFT = this->CalculateForwardFFT<RealImageType, FFTImageType>(
    this->ElementProduct<InputImageType, RealImageType>(rotatedMovingImage, rotatedMovingImage), FFTImageSize);
  rotatedMovingImage = nullptr; // No longer needed
  RealImagePointer rotatedMovingDenom = this->ElementSubtraction<RealImageType>(
    this->CalculateInverseFFT<FFTImageType, RealImageType>(
      this->ElementProduct<FFTImageType, FFTImageType>(fixedMaskFFT, rotatedMovingSquaredFFT), combinedImageSize),
    this->ElementQuotient<RealImageType>(this->ElementProduct<RealImageType, RealImageType>(
                                           rotatedMovingCumulativeSumImage, rotatedMovingCumulativeSumImage),
                                         numberOfOverlapPixels));
  rotatedMovingSquaredFFT = nullptr;         // No longer needed
  fixedMaskFFT = nullptr;                    // No longer needed
  rotatedMovingCumulativeSumImage = nullptr; // No longer needed
  // Ensure that the result is positive.
  rotatedMovingDenom = this->ElementPositive<RealImageType>(rotatedMovingDenom);

  using SqrtType = itk::SqrtImageFilter<RealImageType, RealImageType>;
  typename SqrtType::Pointer sqrtFilter = SqrtType::New();
  sqrtFilter->SetInput(this->ElementProduct<RealImageType, RealImageType>(fixedDenom, rotatedMovingDenom));
  sqrtFilter->Update();
  RealImagePointer denominator = sqrtFilter->GetOutput();
  fixedDenom = nullptr;         // No longer needed
  rotatedMovingDenom = nullptr; // No longer needed

  // Determine a tolerance on the precision of the denominator values.
  const double precisionTolerance = CalculatePrecisionTolerance<RealImageType>(denominator);

  RealImagePointer NCC = this->ElementQuotient<RealImageType>(numerator, denominator);
  numerator = nullptr; // No longer needed

  // Given the numberOfOverlapPixels, we can check that the m_RequiredNumberOfOverlappingPixels is not set higher than
  // the actual maximum overlap voxels.  If it is, we set m_RequiredNumberOfOverlappingPixels to be this maximum.
  using CalculatorType = itk::MinimumMaximumImageCalculator<RealImageType>;
  typename CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage(numberOfOverlapPixels);
  calculator->ComputeMaximum();
  m_MaximumNumberOfOverlappingPixels = static_cast<SizeValueType>(calculator->GetMaximum());
  if (m_RequiredNumberOfOverlappingPixels > m_MaximumNumberOfOverlappingPixels)
  {
    m_RequiredNumberOfOverlappingPixels = (SizeValueType)m_MaximumNumberOfOverlappingPixels;
  }

  // The user can either specify the required number of overlapping pixels or the required fraction of overlapping
  // pixels (or both). Here, we calculate the number of required pixels resulting from both of these methods and choose
  // the one that gives the largest number of pixels. These both default to 0 so that if a user only sets one, the other
  // is ignored.
  SizeValueType requiredNumberOfOverlappingPixels =
    std::max((SizeValueType)(m_RequiredFractionOfOverlappingPixels * m_MaximumNumberOfOverlappingPixels),
             m_RequiredNumberOfOverlappingPixels);

  // The correlation must be between -1 and 1 by definition.  But
  // numerical errors can cause the values to be large values (for
  // example, when dividing by zero).  So, we loop through the matrix
  // and set to zero all values outside of this range.
  // Also, zero-out the correlation values that arise from too few voxels since they are statistically unreliable.
  using PostProcessType = itk::TernaryGeneratorImageFilter<RealImageType, RealImageType, RealImageType, RealImageType>;
  typename PostProcessType::Pointer postProcessor = PostProcessType::New();

  auto functor = Functor::PostProcessCorrelation<RealPixelType>();
  functor.SetRequiredNumberOfOverlappingPixels(requiredNumberOfOverlappingPixels);
  functor.SetPrecisionTolerance(precisionTolerance);
  postProcessor->SetFunctor(functor);

  postProcessor->SetInput1(NCC);
  postProcessor->SetInput2(denominator);
  postProcessor->SetInput3(numberOfOverlapPixels);
  postProcessor->SetInPlace(true); // Save some memory
  postProcessor->Update();

  // Store the output origin computed in GenerateOutputInformation so that it can be reset after the Graft.
  RealPointType outputOrigin = this->GetOutput()->GetOrigin();
  outputImage->Graft(postProcessor->GetOutput());
  outputImage->SetOrigin(outputOrigin);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType>
typename LocalInputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::RotateImage(
  LocalInputImageType * inputImage)
{
  // Store the original origin of the image.
  typename LocalInputImageType::PointType inputOrigin = inputImage->GetOrigin();

  // Flip the moving images along all dimensions so that the correlation can be more easily handled.
  using FlipperType = itk::FlipImageFilter<LocalInputImageType>;
  typename FlipperType::FlipAxesArrayType flipAxes;
  flipAxes.Fill(true);
  typename FlipperType::Pointer rotater = FlipperType::New();
  rotater->SetFlipAxes(flipAxes);
  rotater->SetInput(inputImage);
  rotater->Update();

  typename LocalInputImageType::Pointer outputImage = rotater->GetOutput();
  outputImage->DisconnectPipeline();
  outputImage->SetOrigin(inputOrigin);

  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
typename TMaskImage::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::PreProcessMask(
  const InputImageType * inputImage,
  const MaskImageType *  inputMask)
{
  typename MaskImageType::Pointer outputMask;
  if (inputMask)
  {
    // The mask must have only 0 and 1 values.
    // Threshold the mask.  All voxels less than or equal to 0 are set to 0, and all others are set to 1.
    using ThresholdType = itk::BinaryThresholdImageFilter<MaskImageType, MaskImageType>;
    typename ThresholdType::Pointer thresholder = ThresholdType::New();
    thresholder->SetInput(inputMask);
    thresholder->SetUpperThreshold(0);
    thresholder->SetInsideValue(0);
    thresholder->SetOutsideValue(1);
    thresholder->Update();

    outputMask = thresholder->GetOutput();
    outputMask->DisconnectPipeline();
  }
  else
  {
    // If the mask has not been set, we set it to an image of ones the same
    // size as the image.  This has the effect of not masking the image.
    outputMask = MaskImageType::New();
    outputMask->CopyInformation(inputImage);
    outputMask->SetRegions(inputImage->GetLargestPossibleRegion());

    outputMask->Allocate();
    outputMask->FillBuffer(1);
  }

  return outputMask;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
typename TInputImage::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::PreProcessImage(
  const InputImageType * inputImage,
  const MaskImageType *  inputMask)
{
  // Wherever the mask is 0, the intensity image must also be 0.
  // We achieve this by multiplying the image with the mask, since the mask now contains
  // only values 0 and 1.
  using MultiplyType = itk::MultiplyImageFilter<InputImageType, MaskImageType, InputImageType>;
  typename MultiplyType::Pointer multiplier = MultiplyType::New();
  multiplier->SetInput1(inputImage);
  multiplier->SetInput2(inputMask);
  multiplier->Update();

  typename InputImageType::Pointer outputImage = multiplier->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType, typename LocalOutputImageType>
typename LocalOutputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::CalculateForwardFFT(
  LocalInputImageType * inputImage,
  InputSizeType &       FFTImageSize)
{
  typename LocalInputImageType::PixelType constantPixel = 0;
  typename LocalInputImageType::SizeType  upperPad;
  upperPad = FFTImageSize - inputImage->GetLargestPossibleRegion().GetSize();

  using PadType = itk::ConstantPadImageFilter<LocalInputImageType, RealImageType>;
  typename PadType::Pointer padder = PadType::New();
  padder->SetInput(inputImage);
  padder->SetConstant(constantPixel);
  padder->SetPadUpperBound(upperPad);

  // The input type must be real or else the code will not compile.
  using FFTFilterType = itk::ForwardFFTImageFilter<RealImageType, LocalOutputImageType>;
  typename FFTFilterType::Pointer FFTFilter = FFTFilterType::New();
  FFTFilter->SetInput(padder->GetOutput());
  FFTFilter->Update();

  // The main computation time of this filter is the computation of the FFTs.
  // So we compute our progress based on these FFT computations.
  m_AccumulatedProgress += 1.0 / m_TotalForwardAndInverseFFTs;
  this->UpdateProgress(m_AccumulatedProgress);

  typename LocalOutputImageType::Pointer outputImage = FFTFilter->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType, typename LocalOutputImageType>
typename LocalOutputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::CalculateInverseFFT(
  LocalInputImageType * inputImage,
  RealSizeType &        combinedImageSize)
{
  // The inverse Fourier transform normalizes by the number of voxels in the Fourier image.
  // It also converts the image from complex (with small imaginary values since
  // the input to the original FFTs was real) to real.
  using FFTFilterType = itk::InverseFFTImageFilter<LocalInputImageType, LocalOutputImageType>;
  typename FFTFilterType::Pointer FFTFilter = FFTFilterType::New();
  FFTFilter->SetInput(inputImage);

  // Extract the relevant part out of the image.
  // The input FFT image may be bigger than the desired output image
  // because specific sizes are required for the FFT calculation.
  typename LocalOutputImageType::RegionType imageRegion;
  typename LocalOutputImageType::IndexType  imageIndex;
  imageIndex.Fill(0);
  imageRegion.SetIndex(imageIndex);
  imageRegion.SetSize(combinedImageSize);
  using ExtractType = itk::RegionOfInterestImageFilter<LocalOutputImageType, LocalOutputImageType>;
  typename ExtractType::Pointer extracter = ExtractType::New();
  extracter->SetInput(FFTFilter->GetOutput());
  extracter->SetRegionOfInterest(imageRegion);
  extracter->Update();

  // The main computation time of this filter is the computation of the FFTs.
  // So we compute our progress based on these FFT computations.
  m_AccumulatedProgress += 1.0 / m_TotalForwardAndInverseFFTs;
  this->UpdateProgress(m_AccumulatedProgress);

  typename LocalOutputImageType::Pointer outputImage = extracter->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType, typename LocalOutputImageType>
typename LocalOutputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::ElementProduct(
  LocalInputImageType * inputImage1,
  LocalInputImageType * inputImage2)
{
  using MultiplyType = itk::MultiplyImageFilter<LocalInputImageType, LocalInputImageType, LocalOutputImageType>;
  typename MultiplyType::Pointer multiplier = MultiplyType::New();
  multiplier->SetInput1(inputImage1);
  multiplier->SetInput2(inputImage2);
  multiplier->Update();
  typename LocalOutputImageType::Pointer outputImage = multiplier->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType>
typename LocalInputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::ElementQuotient(
  LocalInputImageType * inputImage1,
  LocalInputImageType * inputImage2)
{
  using DivideType = itk::DivideImageFilter<LocalInputImageType, LocalInputImageType, LocalInputImageType>;
  typename DivideType::Pointer divider = DivideType::New();
  divider->SetInput1(inputImage1);
  divider->SetInput2(inputImage2);
  divider->Update();
  typename LocalInputImageType::Pointer outputImage = divider->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType>
typename LocalInputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::ElementSubtraction(
  LocalInputImageType * inputImage1,
  LocalInputImageType * inputImage2)
{
  using SubtractType = itk::SubtractImageFilter<LocalInputImageType, LocalInputImageType, LocalInputImageType>;
  typename SubtractType::Pointer subtracter = SubtractType::New();
  subtracter->SetInput1(inputImage1);
  subtracter->SetInput2(inputImage2);
  subtracter->Update();
  typename LocalInputImageType::Pointer outputImage = subtracter->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType>
typename LocalInputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::ElementPositive(
  LocalInputImageType * inputImage)
{
  // Set all negative values to 0.
  using ThresholdType = itk::ThresholdImageFilter<LocalInputImageType>;
  typename ThresholdType::Pointer thresholder = ThresholdType::New();
  thresholder->SetInput(inputImage);
  thresholder->ThresholdBelow(0);
  thresholder->SetOutsideValue(0);
  thresholder->Update();
  typename LocalInputImageType::Pointer outputImage = thresholder->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType, typename LocalOutputImageType>
typename LocalOutputImageType::Pointer
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::ElementRound(
  LocalInputImageType * inputImage)
{
  using RoundType = itk::RoundImageFilter<LocalInputImageType, LocalOutputImageType>;
  typename RoundType::Pointer rounder = RoundType::New();
  rounder->SetInput(inputImage);
  rounder->Update();
  typename LocalOutputImageType::Pointer outputImage = rounder->GetOutput();
  outputImage->DisconnectPipeline();
  return outputImage;
}

// This function factorizes the image size uses factors of 2, 3, and
// 5.  After this factorization, if there are any remaining values,
// the function returns this value.
template <typename TInputImage, typename TOutputImage, typename TMaskImage>
int
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::FactorizeNumber(int n)
{
  int ifac = 2;
  // This loop is just a convenient way of ensuring that ifac assumes
  // values of 2, 3, and 5 and then quits.  These are the only factors
  // that are valid for the FFT calculation.
  for (int offset = 1; offset <= 3; offset++)
  {
    // Using the given factor, factor the image continuously until it
    // can no longer be factored with this value.
    for (; n % ifac == 0;)
    {
      n /= ifac;
    }
    ifac += offset;
  }
  return n;
}

// Find the closest valid dimension above the desired dimension.  This
// will be a combination of 2s, 3s, and 5s.
template <typename TInputImage, typename TOutputImage, typename TMaskImage>
int
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::FindClosestValidDimension(int n)
{
  // Incrementally add 1 to the size until
  // we reach a size that can be properly factored.
  int newNumber = n;
  int result = 0;
  newNumber -= 1;
  while (result != 1)
  {
    newNumber += 1;
    result = this->FactorizeNumber(newNumber);
  }
  return newNumber;
}

// Find the precision tolerance.
template <typename TInputImage, typename TOutputImage, typename TMaskImage>
template <typename LocalInputImageType>
double
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::CalculatePrecisionTolerance(
  LocalInputImageType * inputImage)
{
  // First find the maximum of the inputImage.
  using CalculatorType = itk::MinimumMaximumImageCalculator<LocalInputImageType>;
  typename CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage(inputImage);
  calculator->ComputeMaximum();

  typename LocalInputImageType::IndexType index;
  index.Fill(0);

  double precisionTolerance = 0.0F;
  if (typeid(inputImage->GetPixel(index)) == typeid(double))
  {
    precisionTolerance =
      1000.0 * std::pow(2.0, -52) * std::pow(2, std::floor(std::log(calculator->GetMaximum()) / std::log(2.0)));
  }
  else if (typeid(inputImage->GetPixel(index)) == typeid(float))
  {
    precisionTolerance =
      1000.0 * std::pow(2.0, -23) * std::pow(2, std::floor(std::log(calculator->GetMaximum()) / std::log(2.0)));
  }
  else
  {
    itkExceptionMacro(<< "Precision tolerance not defined for the input image pixel type.");
  }

  return precisionTolerance;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::VerifyInputInformation() ITKv5_CONST
{
  // Call the superclass' implementation of this method.
  Superclass::VerifyInputInformation();

  // The superclass method checks origin, spacing, and direction.
  // We need a few additional checks.
  // Check that the image sizes are the same as their corresponding
  // masks.
  std::ostringstream fixedSizeString, movingSizeString;
  if (this->GetFixedImageMask() && this->GetFixedImage()->GetLargestPossibleRegion().GetSize() !=
                                     this->GetFixedImageMask()->GetLargestPossibleRegion().GetSize())
  {
    fixedSizeString << std::endl
                    << "The fixed image must be the same size as the fixed mask.  " << std::endl
                    << "FixedImage Size: " << this->GetFixedImage()->GetLargestPossibleRegion().GetSize()
                    << ", FixedMask Size: " << this->GetFixedImageMask()->GetLargestPossibleRegion().GetSize()
                    << std::endl;
    itkExceptionMacro(<< fixedSizeString.str());
  }
  if (this->GetMovingImageMask() && this->GetMovingImage()->GetLargestPossibleRegion().GetSize() !=
                                      this->GetMovingImageMask()->GetLargestPossibleRegion().GetSize())
  {
    movingSizeString << std::endl
                     << "The moving image must be the same size as the moving mask.  " << std::endl
                     << "MovingImage Size: " << this->GetMovingImage()->GetLargestPossibleRegion().GetSize()
                     << ", MovingMask Size: " << this->GetMovingImageMask()->GetLargestPossibleRegion().GetSize()
                     << std::endl;
    itkExceptionMacro(<< movingSizeString.str());
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Here we need to set the RequestedRegion to the LargestPossibleRegion
  // for all of the inputs.
  // Even though the ProcessObject implements exactly the same thing,
  // the ImageToImageFilter overrides this by setting the RequestedRegion
  // to the largest possible region of the output image.
  // Therefore, we need to re-override this behavior since our output
  // is bigger than our input.

  // Cast away the constness so we can set the requested region.
  InputRegionType   inputRegion;
  InputImagePointer inputPtr;
  inputPtr = const_cast<InputImageType *>(this->GetFixedImage());
  inputPtr->SetRequestedRegion(this->GetFixedImage()->GetLargestPossibleRegion());

  inputPtr = const_cast<InputImageType *>(this->GetMovingImage());
  inputPtr->SetRequestedRegion(this->GetMovingImage()->GetLargestPossibleRegion());

  MaskImagePointer maskPtr;
  maskPtr = const_cast<MaskImageType *>(this->GetFixedImageMask());
  if (maskPtr)
  {
    maskPtr->SetRequestedRegion(this->GetFixedImageMask()->GetLargestPossibleRegion());
  }

  maskPtr = const_cast<MaskImageType *>(this->GetMovingImageMask());
  if (maskPtr)
  {
    maskPtr->SetRequestedRegion(this->GetMovingImageMask()->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer fixedImage = this->GetFixedImage();
  InputImageConstPointer movingImage = this->GetMovingImage();
  OutputImagePointer     output = this->GetOutput();

  // Compute the size of the output image.
  typename OutputImageType::RegionType region;
  typename OutputImageType::SizeType   size;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    size[i] =
      fixedImage->GetLargestPossibleRegion().GetSize()[i] + movingImage->GetLargestPossibleRegion().GetSize()[i] - 1;
  }
  region.SetSize(size);
  region.SetIndex(fixedImage->GetLargestPossibleRegion().GetIndex());

  output->SetLargestPossibleRegion(region);

  // Corrected the output origin of the MaskedFFTNCC algorithm so that the
  // fixed image falls directly in the middle of the output correlation map
  // in physical space.  With this alignment, each point in the output NCC
  // map overlaps at the location in the fixed image at which the
  // moving image provides that NCC score when centered at that
  // location.
  itk::ContinuousIndex<typename RealPointType::ValueType, ImageDimension> movingImageOffset;
  RealPointType                                                           outputOrigin;
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    movingImageOffset[i] = -(float)(movingImage->GetLargestPossibleRegion().GetSize()[i] - 1) / 2.0;
  }
  fixedImage->TransformContinuousIndexToPhysicalPoint(movingImageOffset, outputOrigin);
  output->SetOrigin(outputOrigin);
}


template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::EnlargeOutputRequestedRegion(
  DataObject * output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // get pointers to the input and output
  InputImageConstPointer fixedImage = this->GetFixedImage();
  InputImageConstPointer movingImage = this->GetMovingImage();

  // Compute the size of the output image.
  typename OutputImageType::RegionType region;
  typename OutputImageType::SizeType   size;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    size[i] =
      fixedImage->GetLargestPossibleRegion().GetSize()[i] + movingImage->GetLargestPossibleRegion().GetSize()[i] - 1;
  }
  region.SetSize(size);
  region.SetIndex(fixedImage->GetLargestPossibleRegion().GetIndex());

  auto * optr = dynamic_cast<OutputImageType *>(output);
  if (optr)
  {
    optr->SetRequestedRegion(region);
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
MaskedFFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage, TMaskImage>::PrintSelf(std::ostream & os,
                                                                                            Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
