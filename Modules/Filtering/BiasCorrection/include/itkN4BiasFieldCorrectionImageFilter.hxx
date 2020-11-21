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
#ifndef itkN4BiasFieldCorrectionImageFilter_hxx
#define itkN4BiasFieldCorrectionImageFilter_hxx

#include "itkN4BiasFieldCorrectionImageFilter.h"

#include "itkAddImageFilter.h"
#include "itkBSplineControlPointImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkImageBufferRange.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImportImageFilter.h"
#include "itkIterationReporter.h"
#include "itkSubtractImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"

CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal
#include "vnl/algo/vnl_fft_1d.h"
#include "vnl/vnl_complex_traits.h"
#include "complex"
  CLANG_PRAGMA_POP

  namespace itk
{

  template <typename TInputImage, typename TMaskImage, typename TOutputImage>
  N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::N4BiasFieldCorrectionImageFilter()
    : m_MaskLabel(NumericTraits<MaskPixelType>::OneValue())
    , m_CurrentConvergenceMeasurement(NumericTraits<RealType>::ZeroValue())

  {
    // implicit:
    // #0 "Primary" required

    // #1 "MaskImage" optional
    Self::AddOptionalInputName("MaskImage", 1);

    // #2 "ConfidenceImage" optional
    Self::AddOptionalInputName("ConfidenceImage", 2);

    this->SetNumberOfRequiredInputs(1);

    this->m_LogBiasFieldControlPointLattice = nullptr;

    this->m_NumberOfFittingLevels.Fill(1);
    this->m_NumberOfControlPoints.Fill(4);

    this->m_MaximumNumberOfIterations.SetSize(1);
    this->m_MaximumNumberOfIterations.Fill(50);
  }


  template <typename TInputImage, typename TMaskImage, typename TOutputImage>
  void N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::EnlargeOutputRequestedRegion(
    DataObject * output)
  {
    Superclass::EnlargeOutputRequestedRegion(output);

    if (output != nullptr)
    {
      output->SetRequestedRegionToLargestPossibleRegion();
    }
  }


  template <typename TInputImage, typename TMaskImage, typename TOutputImage>
  void N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::GenerateData()
  {
    this->AllocateOutputs();

    const InputImageType * inputImage = this->GetInput();
    using RegionType = typename InputImageType::RegionType;
    const RegionType inputRegion = inputImage->GetBufferedRegion();

    const typename InputImageType::SizeType inputImageSize = inputRegion.GetSize();

    const MaskImageType * const maskImage = GetMaskImage();

    if ((maskImage != nullptr) && (maskImage->GetBufferedRegion().GetSize() != inputImageSize))
    {
      itkExceptionMacro("If a mask image is specified, its size should be equal to the input image size");
    }

    const RealImageType * const confidenceImage = GetConfidenceImage();

    if ((confidenceImage != nullptr) && (confidenceImage->GetBufferedRegion().GetSize() != inputImageSize))
    {
      itkExceptionMacro("If a confidence image is specified, its size should be equal to the input image size");
    }

    // Calculate the log of the input image.
    RealImagePointer logInputImage = RealImageType::New();
    logInputImage->CopyInformation(inputImage);
    logInputImage->SetRegions(inputRegion);
    logInputImage->Allocate(false);

    ImageAlgorithm::Copy(inputImage, logInputImage.GetPointer(), inputRegion, inputRegion);

    const auto          maskImageBufferRange = MakeImageBufferRange(maskImage);
    const auto          confidenceImageBufferRange = MakeImageBufferRange(confidenceImage);
    const MaskPixelType maskLabel = this->GetMaskLabel();
    const bool          useMaskLabel = this->GetUseMaskLabel();

    const ImageBufferRange<RealImageType> logInputImageBufferRange{ *logInputImage };
    const std::size_t                     numberOfPixels = logInputImageBufferRange.size();

    // Number of pixels of the input image that are included with the filter.
    std::size_t numberOfIncludedPixels = 0;

    for (std::size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
    {
      if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
           (!useMaskLabel && maskImageBufferRange[indexValue] != NumericTraits<MaskPixelType>::ZeroValue())) &&
          (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
      {
        ++numberOfIncludedPixels;
        auto && logInputPixel = logInputImageBufferRange[indexValue];

        if (logInputPixel > NumericTraits<typename InputImageType::PixelType>::ZeroValue())
        {
          logInputPixel = std::log(static_cast<RealType>(logInputPixel));
        }
      }
    }

    // Duplicate logInputImage since we reuse the original at each iteration.

    using DuplicatorType = ImageDuplicator<RealImageType>;
    typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
    duplicator->SetInputImage(logInputImage);
    duplicator->Update();

    RealImagePointer logUncorrectedImage = duplicator->GetOutput();

    // Provide an initial log bias field of zeros

    RealImagePointer logBiasField = RealImageType::New();
    logBiasField->CopyInformation(inputImage);
    logBiasField->SetRegions(inputImage->GetLargestPossibleRegion());
    logBiasField->Allocate(true); // initialize buffer to zero

    RealImagePointer logSharpenedImage = RealImageType::New();
    logSharpenedImage->CopyInformation(inputImage);
    logSharpenedImage->SetRegions(inputImage->GetLargestPossibleRegion());
    logSharpenedImage->Allocate(false);

    // Iterate until convergence or iterative exhaustion.
    unsigned int maximumNumberOfLevels = 1;
    for (unsigned int d = 0; d < this->m_NumberOfFittingLevels.Size(); d++)
    {
      if (this->m_NumberOfFittingLevels[d] > maximumNumberOfLevels)
      {
        maximumNumberOfLevels = this->m_NumberOfFittingLevels[d];
      }
    }
    if (this->m_MaximumNumberOfIterations.Size() != maximumNumberOfLevels)
    {
      itkExceptionMacro("Number of iteration levels is not equal to the max number of levels.");
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

        using SubtracterType = SubtractImageFilter<RealImageType, RealImageType, RealImageType>;
        typename SubtracterType::Pointer subtracter1 = SubtracterType::New();
        subtracter1->SetInput1(logUncorrectedImage);
        subtracter1->SetInput2(logSharpenedImage);

        RealImagePointer residualBiasField = subtracter1->GetOutput();
        residualBiasField->Update();

        // Smooth the residual bias field estimate and add the resulting
        // control point grid to get the new total bias field estimate.

        RealImagePointer newLogBiasField = this->UpdateBiasFieldEstimate(residualBiasField, numberOfIncludedPixels);

        this->m_CurrentConvergenceMeasurement = this->CalculateConvergenceMeasurement(logBiasField, newLogBiasField);
        logBiasField = newLogBiasField;

        typename SubtracterType::Pointer subtracter2 = SubtracterType::New();
        subtracter2->SetInput1(logInputImage);
        subtracter2->SetInput2(logBiasField);

        logUncorrectedImage = subtracter2->GetOutput();
        logUncorrectedImage->Update();

        reporter.CompletedStep();
      }

      using BSplineReconstructerType =
        BSplineControlPointImageFilter<BiasFieldControlPointLatticeType, ScalarImageType>;
      typename BSplineReconstructerType::Pointer reconstructer = BSplineReconstructerType::New();
      reconstructer->SetInput(this->m_LogBiasFieldControlPointLattice);
      reconstructer->SetOrigin(logBiasField->GetOrigin());
      reconstructer->SetSpacing(logBiasField->GetSpacing());
      reconstructer->SetDirection(logBiasField->GetDirection());
      reconstructer->SetSize(logBiasField->GetLargestPossibleRegion().GetSize());
      reconstructer->SetSplineOrder(this->m_SplineOrder);
      reconstructer->Update();

      typename BSplineReconstructerType::ArrayType numberOfLevels;
      numberOfLevels.Fill(1);
      for (unsigned int d = 0; d < ImageDimension; d++)
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
    typename CustomBinaryFilter::Pointer expAndDivFilter = CustomBinaryFilter::New();
    auto                                 expAndDivLambda =
      [](const typename InputImageType::PixelType & input, const typename RealImageType::PixelType & biasField) ->
      typename OutputImageType::PixelType
    {
      return static_cast<typename OutputImageType::PixelType>(input / std::exp(biasField));
    };
    expAndDivFilter->SetFunctor(expAndDivLambda);
    expAndDivFilter->SetInput1(inputImage);
    expAndDivFilter->SetInput2(logBiasField);
    expAndDivFilter->Update();

    this->GraftOutput(expAndDivFilter->GetOutput());
  }

  template <typename TInputImage, typename TMaskImage, typename TOutputImage>
  void N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::SharpenImage(
    const RealImageType * unsharpenedImage, RealImageType * sharpenedImage) const
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

    const auto        unsharpenedImageBufferRange = MakeImageBufferRange(unsharpenedImage);
    const std::size_t numberOfPixels = unsharpenedImageBufferRange.size();

    for (std::size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
    {
      if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
           (!useMaskLabel && maskImageBufferRange[indexValue] != NumericTraits<MaskPixelType>::ZeroValue())) &&
          (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
      {
        RealType pixel = unsharpenedImageBufferRange[indexValue];
        if (pixel > binMaximum)
        {
          binMaximum = pixel;
        }
        else if (pixel < binMinimum)
        {
          binMinimum = pixel;
        }
      }
    }
    RealType histogramSlope = (binMaximum - binMinimum) / static_cast<RealType>(this->m_NumberOfHistogramBins - 1);

    // Create the intensity profile (within the masked region, if applicable)
    // using a triangular parzen windowing scheme.

    vnl_vector<RealType> H(this->m_NumberOfHistogramBins, 0.0);

    for (std::size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
    {
      if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
           (!useMaskLabel && maskImageBufferRange[indexValue] != NumericTraits<MaskPixelType>::ZeroValue())) &&
          (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
      {
        RealType pixel = unsharpenedImageBufferRange[indexValue];

        RealType     cidx = (static_cast<RealType>(pixel) - binMinimum) / histogramSlope;
        unsigned int idx = itk::Math::floor(cidx);
        RealType     offset = cidx - static_cast<RealType>(idx);

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

    RealType exponent = std::ceil(std::log(static_cast<RealType>(this->m_NumberOfHistogramBins)) / std::log(2.0)) + 1;
    auto     paddedHistogramSize = static_cast<unsigned int>(std::pow(static_cast<RealType>(2.0), exponent) + 0.5);
    auto     histogramOffset = static_cast<unsigned int>(0.5 * (paddedHistogramSize - this->m_NumberOfHistogramBins));

    using FFTComputationType = double;
    using FFTComplexType = std::complex<FFTComputationType>;

    vnl_vector<FFTComplexType> V(paddedHistogramSize, FFTComplexType(0.0, 0.0));

    for (unsigned int n = 0; n < this->m_NumberOfHistogramBins; n++)
    {
      V[n + histogramOffset] = H[n];
    }

    // Instantiate the 1-d vnl fft routine.

    vnl_fft_1d<FFTComputationType> fft(paddedHistogramSize);

    vnl_vector<FFTComplexType> Vf(V);

    fft.fwd_transform(Vf);

    // Create the Gaussian filter.

    RealType scaledFWHM = this->m_BiasFieldFullWidthAtHalfMaximum / histogramSlope;
    RealType expFactor = 4.0 * std::log(2.0) / itk::Math::sqr(scaledFWHM);
    RealType scaleFactor = 2.0 * std::sqrt(std::log(2.0) / itk::Math::pi) / scaledFWHM;

    vnl_vector<FFTComplexType> F(paddedHistogramSize, FFTComplexType(0.0, 0.0));

    F[0] = FFTComplexType(scaleFactor, 0.0);
    auto halfSize = static_cast<unsigned int>(0.5 * paddedHistogramSize);
    for (unsigned int n = 1; n <= halfSize; n++)
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

    fft.fwd_transform(Ff);

    // Create the Wiener deconvolution filter.

    vnl_vector<FFTComplexType> Gf(paddedHistogramSize);

    const auto wienerNoiseValue = static_cast<FFTComputationType>(this->m_WienerFilterNoise);
    for (unsigned int n = 0; n < paddedHistogramSize; n++)
    {
      FFTComplexType c = vnl_complex_traits<FFTComplexType>::conjugate(Ff[n]);
      Gf[n] = c / (c * Ff[n] + wienerNoiseValue);
    }

    vnl_vector<FFTComplexType> Uf(paddedHistogramSize);

    for (unsigned int n = 0; n < paddedHistogramSize; n++)
    {
      Uf[n] = Vf[n] * Gf[n].real();
    }

    vnl_vector<FFTComplexType> U(Uf);

    fft.bwd_transform(U);
    for (unsigned int n = 0; n < paddedHistogramSize; n++)
    {
      U[n] = FFTComplexType(std::max(U[n].real(), static_cast<FFTComputationType>(0.0)), 0.0);
    }

    // Compute mapping E(u|v).

    vnl_vector<FFTComplexType> numerator(paddedHistogramSize);

    for (unsigned int n = 0; n < paddedHistogramSize; n++)
    {
      numerator[n] =
        FFTComplexType((binMinimum + (static_cast<RealType>(n) - histogramOffset) * histogramSlope) * U[n].real(), 0.0);
    }
    fft.fwd_transform(numerator);
    for (unsigned int n = 0; n < paddedHistogramSize; n++)
    {
      numerator[n] *= Ff[n];
    }
    fft.bwd_transform(numerator);

    vnl_vector<FFTComplexType> denominator(U);

    fft.fwd_transform(denominator);
    for (unsigned int n = 0; n < paddedHistogramSize; n++)
    {
      denominator[n] *= Ff[n];
    }
    fft.bwd_transform(denominator);

    vnl_vector<RealType> E(paddedHistogramSize);

    for (unsigned int n = 0; n < paddedHistogramSize; n++)
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

    const ImageBufferRange<RealImageType> sharpenedImageBufferRange{ *sharpenedImage };

    for (std::size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
    {
      if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
           (!useMaskLabel && maskImageBufferRange[indexValue] != NumericTraits<MaskPixelType>::ZeroValue())) &&
          (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
      {
        RealType     cidx = (unsharpenedImageBufferRange[indexValue] - binMinimum) / histogramSlope;
        unsigned int idx = itk::Math::floor(cidx);

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
  typename N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealImagePointer
  N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::UpdateBiasFieldEstimate(
    RealImageType * fieldEstimate, const std::size_t numberOfIncludedPixels)
  {
    // Temporarily set the direction cosine to identity since the B-spline
    // approximation algorithm works in parametric space and not physical
    // space.
    typename ScalarImageType::DirectionType identity;
    identity.SetIdentity();

    const typename ScalarImageType::RegionType & bufferedRegion = fieldEstimate->GetBufferedRegion();
    const SizeValueType                          numberOfPixels = bufferedRegion.GetNumberOfPixels();
    const bool                                   filterHandlesMemory = false;

    using ImporterType = ImportImageFilter<RealType, ImageDimension>;
    typename ImporterType::Pointer importer = ImporterType::New();
    importer->SetImportPointer(fieldEstimate->GetBufferPointer(), numberOfPixels, filterHandlesMemory);
    importer->SetRegion(fieldEstimate->GetBufferedRegion());
    importer->SetOrigin(fieldEstimate->GetOrigin());
    importer->SetSpacing(fieldEstimate->GetSpacing());
    importer->SetDirection(identity);
    importer->Update();

    const typename ImporterType::OutputImageType * parametricFieldEstimate = importer->GetOutput();

    PointSetPointer fieldPoints = PointSetType::New();
    fieldPoints->Initialize();
    auto & pointSTLContainer = fieldPoints->GetPoints()->CastToSTLContainer();
    pointSTLContainer.reserve(numberOfIncludedPixels);
    auto & pointDataSTLContainer = fieldPoints->GetPointData()->CastToSTLContainer();
    pointDataSTLContainer.reserve(numberOfIncludedPixels);

    typename BSplineFilterType::WeightsContainerType::Pointer weights = BSplineFilterType::WeightsContainerType::New();
    weights->Initialize();
    auto & weightSTLContainer = weights->CastToSTLContainer();
    weightSTLContainer.reserve(numberOfIncludedPixels);

    const auto          maskImageBufferRange = MakeImageBufferRange(this->GetMaskImage());
    const auto          confidenceImageBufferRange = MakeImageBufferRange(this->GetConfidenceImage());
    const MaskPixelType maskLabel = this->GetMaskLabel();
    const bool          useMaskLabel = this->GetUseMaskLabel();

    ImageRegionConstIteratorWithIndex<RealImageType> It(parametricFieldEstimate,
                                                        parametricFieldEstimate->GetRequestedRegion());

    for (std::size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue, ++It)
    {
      if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
           (!useMaskLabel && maskImageBufferRange[indexValue] != NumericTraits<MaskPixelType>::ZeroValue())) &&
          (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
      {
        PointType point;
        parametricFieldEstimate->TransformIndexToPhysicalPoint(It.GetIndex(), point);

        ScalarType scalar;
        scalar[0] = It.Get();

        pointDataSTLContainer.push_back(scalar);
        pointSTLContainer.push_back(point);

        RealType confidenceWeight = 1.0;
        if (!confidenceImageBufferRange.empty())
        {
          confidenceWeight = confidenceImageBufferRange[indexValue];
        }
        weightSTLContainer.push_back(confidenceWeight);
      }
    }

    typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();

    typename BSplineFilterType::ArrayType numberOfControlPoints;
    typename BSplineFilterType::ArrayType numberOfFittingLevels;
    numberOfFittingLevels.Fill(1);
    for (unsigned int d = 0; d < ImageDimension; d++)
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
    for (unsigned int d = 0; d < ImageDimension; d++)
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
    bspliner->SetPointWeights(weights);
    bspliner->Update();

    typename BiasFieldControlPointLatticeType::Pointer phiLattice = bspliner->GetPhiLattice();

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
      typename AdderType::Pointer adder = AdderType::New();
      adder->SetInput1(this->m_LogBiasFieldControlPointLattice);
      adder->SetInput2(phiLattice);
      adder->Update();

      this->m_LogBiasFieldControlPointLattice = adder->GetOutput();
    }

    RealImagePointer smoothField = this->ReconstructBiasField(this->m_LogBiasFieldControlPointLattice);

    return smoothField;
  }

  template <typename TInputImage, typename TMaskImage, typename TOutputImage>
  typename N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealImagePointer
  N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::ReconstructBiasField(
    const BiasFieldControlPointLatticeType * controlPointLattice)
  {
    const InputImageType * inputImage = this->GetInput();

    using BSplineReconstructerType = BSplineControlPointImageFilter<BiasFieldControlPointLatticeType, ScalarImageType>;
    typename BSplineReconstructerType::Pointer reconstructer = BSplineReconstructerType::New();
    reconstructer->SetInput(controlPointLattice);
    reconstructer->SetOrigin(inputImage->GetOrigin());
    reconstructer->SetSpacing(inputImage->GetSpacing());
    reconstructer->SetDirection(inputImage->GetDirection());
    reconstructer->SetSplineOrder(this->m_SplineOrder);
    reconstructer->SetSize(inputImage->GetLargestPossibleRegion().GetSize());

    typename ScalarImageType::Pointer biasFieldBsplineImage = reconstructer->GetOutput();
    biasFieldBsplineImage->Update();

    using SelectorType = VectorIndexSelectionCastImageFilter<ScalarImageType, RealImageType>;
    typename SelectorType::Pointer selector = SelectorType::New();
    selector->SetInput(biasFieldBsplineImage);
    selector->SetIndex(0);

    RealImagePointer biasField = selector->GetOutput();
    biasField->Update();

    biasField->DisconnectPipeline();
    biasField->SetRegions(inputImage->GetRequestedRegion());

    return biasField;
  }

  template <typename TInputImage, typename TMaskImage, typename TOutputImage>
  typename N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::RealType
  N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::CalculateConvergenceMeasurement(
    const RealImageType * fieldEstimate1, const RealImageType * fieldEstimate2) const
  {
    using SubtracterType = SubtractImageFilter<RealImageType, RealImageType, RealImageType>;
    typename SubtracterType::Pointer subtracter = SubtracterType::New();
    subtracter->SetInput1(fieldEstimate1);
    subtracter->SetInput2(fieldEstimate2);
    subtracter->Update();

    // Calculate statistics over the mask region

    RealType mu = 0.0;
    RealType sigma = 0.0;
    RealType N = 0.0;

    const auto          maskImageBufferRange = MakeImageBufferRange(this->GetMaskImage());
    const auto          confidenceImageBufferRange = MakeImageBufferRange(this->GetConfidenceImage());
    const MaskPixelType maskLabel = this->GetMaskLabel();
    const bool          useMaskLabel = this->GetUseMaskLabel();

    const auto        subtracterImageBufferRange = MakeImageBufferRange(subtracter->GetOutput());
    const std::size_t numberOfPixels = subtracterImageBufferRange.size();

    for (std::size_t indexValue = 0; indexValue < numberOfPixels; ++indexValue)
    {
      if ((maskImageBufferRange.empty() || (useMaskLabel && maskImageBufferRange[indexValue] == maskLabel) ||
           (!useMaskLabel && maskImageBufferRange[indexValue] != NumericTraits<MaskPixelType>::ZeroValue())) &&
          (confidenceImageBufferRange.empty() || confidenceImageBufferRange[indexValue] > 0.0))
      {
        RealType pixel = std::exp(subtracterImageBufferRange[indexValue]);
        N += 1.0;

        if (N > 1.0)
        {
          sigma = sigma + itk::Math::sqr(pixel - mu) * (N - 1.0) / N;
        }
        mu = mu * (1.0 - 1.0 / N) + pixel / N;
      }
    }
    sigma = std::sqrt(sigma / (N - 1.0));

    return (sigma / mu);
  }

  template <typename TInputImage, typename TMaskImage, typename TOutputImage>
  void N4BiasFieldCorrectionImageFilter<TInputImage, TMaskImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                          Indent indent) const
  {
    Superclass::PrintSelf(os, indent);

    os << indent << "Use Mask Label: " << m_UseMaskLabel << std::endl;
    os << indent << "Mask label: " << static_cast<typename NumericTraits<MaskPixelType>::PrintType>(this->m_MaskLabel)
       << std::endl;
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
