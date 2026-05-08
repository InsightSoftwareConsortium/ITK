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
#ifndef itkAdaptiveNonLocalMeansDenoisingImageFilter_hxx
#define itkAdaptiveNonLocalMeansDenoisingImageFilter_hxx


#include "itkArray.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkMeanImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkProgressReporter.h"
#include "itkStatisticsImageFilter.h"
#include "itkVarianceImageFilter.h"

#include <numeric>

namespace itk
{

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::
  AdaptiveNonLocalMeansDenoisingImageFilter()
  : m_UseRicianNoiseModel(true)
  , m_Epsilon(0.00001)
  , m_MeanThreshold(0.95)
  , m_VarianceThreshold(0.5)
  , m_SmoothingFactor(1.0)
  , m_SmoothingVariance(2.0)
  , m_MaximumInputPixelIntensity(NumericTraits<RealType>::NonpositiveMin())
  , m_MinimumInputPixelIntensity(NumericTraits<RealType>::max())
{
  this->SetNumberOfRequiredInputs(1);

  this->m_MeanImage = nullptr;
  this->m_VarianceImage = nullptr;
  this->m_IntensitySquaredDistanceImage = nullptr;
  this->m_ThreadContributionCountImage = nullptr;

  this->m_RicianBiasImage = nullptr;

  this->m_NeighborhoodRadiusForLocalMeanAndVariance.Fill(1);
  this->DynamicMultiThreadingOff();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  const InputImageType * inputImage = this->GetInput();

  typedef MeanImageFilter<InputImageType, RealImageType> MeanImageFilterType;
  typename MeanImageFilterType::Pointer                  meanImageFilter = MeanImageFilterType::New();
  meanImageFilter->SetInput(inputImage);
  meanImageFilter->SetRadius(this->GetNeighborhoodRadiusForLocalMeanAndVariance());

  this->m_MeanImage = meanImageFilter->GetOutput();
  this->m_MeanImage->Update();
  this->m_MeanImage->DisconnectPipeline();

  typedef VarianceImageFilter<InputImageType, RealImageType> VarianceImageFilterType;
  typename VarianceImageFilterType::Pointer                  varianceImageFilter = VarianceImageFilterType::New();
  varianceImageFilter->SetInput(inputImage);
  varianceImageFilter->SetRadius(this->GetNeighborhoodRadiusForLocalMeanAndVariance());

  this->m_VarianceImage = varianceImageFilter->GetOutput();
  this->m_VarianceImage->Update();
  this->m_VarianceImage->DisconnectPipeline();

  typedef StatisticsImageFilter<InputImageType> StatsFilterType;
  typename StatsFilterType::Pointer             statsFilter = StatsFilterType::New();
  statsFilter->SetInput(inputImage);
  statsFilter->Update();

  this->m_MaximumInputPixelIntensity = static_cast<RealType>(statsFilter->GetMaximum());
  this->m_MinimumInputPixelIntensity = static_cast<RealType>(statsFilter->GetMinimum());

  this->m_ThreadContributionCountImage = RealImageType::New();
  this->m_ThreadContributionCountImage->CopyInformation(inputImage);
  this->m_ThreadContributionCountImage->SetRegions(inputImage->GetRequestedRegion());
  this->m_ThreadContributionCountImage->Allocate(true);

  if (this->m_UseRicianNoiseModel)
  {
    this->m_RicianBiasImage = RealImageType::New();
    this->m_RicianBiasImage->CopyInformation(inputImage);
    this->m_RicianBiasImage->SetRegions(inputImage->GetRequestedRegion());
    this->m_RicianBiasImage->Allocate(true);
  }

  this->AllocateOutputs();
  // Output buffer needs to be zero initialized
  this->GetOutput()->FillBuffer(0.0);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::ThreadedGenerateData(
  const RegionType & region,
  ThreadIdType       threadId)
{
  ProgressReporter progress(this, threadId, region.GetNumberOfPixels(), 100);

  const InputImageType * inputImage = this->GetInput();
  const MaskImageType *  maskImage = this->GetMaskImage();

  OutputImageType * outputImage = this->GetOutput();
  RegionType        targetImageRegion = this->GetTargetImageRegion();

  NeighborhoodOffsetListType neighborhoodPatchOffsetList = this->GetNeighborhoodPatchOffsetList();

  NeighborhoodRadiusType neighborhoodSearchRadius = this->GetNeighborhoodSearchRadius();

  ConstNeighborhoodIterator<RealImageType> ItV(neighborhoodSearchRadius, this->m_VarianceImage, region);
  ConstNeighborhoodIterator<RealImageType> ItM(neighborhoodSearchRadius, this->m_MeanImage, region);

  const unsigned int neighborhoodSearchSize = this->GetNeighborhoodSearchSize();
  const unsigned int neighborhoodPatchSize = this->GetNeighborhoodPatchSize();

  Array<RealType> weightedAverageIntensities(neighborhoodPatchSize);

  ItM.GoToBegin();
  ItV.GoToBegin();

  while (!ItM.IsAtEnd())
  {
    typename InputImageType::IndexType centerIndex = ItM.GetIndex();

    InputPixelType inputCenterPixel = inputImage->GetPixel(centerIndex);
    RealType       meanCenterPixel = this->m_MeanImage->GetPixel(centerIndex);
    RealType       varianceCenterPixel = this->m_VarianceImage->GetPixel(centerIndex);

    RealType maxWeight = NumericTraits<RealType>::ZeroValue();
    RealType sumOfWeights = NumericTraits<RealType>::ZeroValue();

    weightedAverageIntensities.Fill(NumericTraits<RealType>::ZeroValue());

    RealType meanNeighborhoodPixel = NumericTraits<RealType>::ZeroValue();
    RealType varianceNeighborhoodPixel = NumericTraits<RealType>::ZeroValue();

    if (inputCenterPixel > 0 && meanCenterPixel > this->m_Epsilon && varianceCenterPixel > this->m_Epsilon &&
        (!maskImage || maskImage->GetPixel(centerIndex) != NumericTraits<MaskPixelType>::ZeroValue()))
    {
      // Calculate the minimum distance

      RealType minimumDistance = NumericTraits<RealType>::max();
      for (unsigned int m = 0; m < neighborhoodSearchSize; m++)
      {
        if (!ItM.IndexInBounds(m) || m == static_cast<unsigned int>(0.5 * neighborhoodSearchSize))
        {
          continue;
        }

        IndexType neighborhoodIndex = ItM.GetIndex(m);

        if (inputImage->GetPixel(neighborhoodIndex) <= 0)
        {
          continue;
        }

        meanNeighborhoodPixel = this->m_MeanImage->GetPixel(neighborhoodIndex);
        varianceNeighborhoodPixel = this->m_VarianceImage->GetPixel(neighborhoodIndex);

        if (meanNeighborhoodPixel <= this->m_Epsilon || varianceNeighborhoodPixel <= this->m_Epsilon)
        {
          continue;
        }

        const RealType meanRatio = meanCenterPixel / meanNeighborhoodPixel;
        const RealType meanRatioInverse = (this->m_MaximumInputPixelIntensity - meanCenterPixel) /
                                          (this->m_MaximumInputPixelIntensity - meanNeighborhoodPixel);

        const RealType varianceRatio = varianceCenterPixel / varianceNeighborhoodPixel;

        if (((meanRatio > this->m_MeanThreshold &&
              meanRatio < itk::NumericTraits<RealType>::OneValue() / this->m_MeanThreshold) ||
             (meanRatioInverse > this->m_MeanThreshold &&
              meanRatioInverse < itk::NumericTraits<RealType>::OneValue() / this->m_MeanThreshold)) &&
            varianceRatio > this->m_VarianceThreshold &&
            varianceRatio < itk::NumericTraits<RealType>::OneValue() / this->m_VarianceThreshold)
        {

          RealType averageDistance = itk::NumericTraits<RealType>::ZeroValue();
          RealType count = itk::NumericTraits<RealType>::ZeroValue();

          for (unsigned int n = 0; n < neighborhoodPatchSize; n++)
          {
            IndexType neighborhoodPatchIndex = neighborhoodIndex + neighborhoodPatchOffsetList[n];

            if (!targetImageRegion.IsInside(neighborhoodPatchIndex))
            {
              continue;
            }
            RealType neighborhoodInputImagePixel = static_cast<RealType>(inputImage->GetPixel(neighborhoodPatchIndex));
            RealType neighborhoodMeanImagePixel = this->m_MeanImage->GetPixel(neighborhoodPatchIndex);
            averageDistance += itk::Math::sqr(neighborhoodInputImagePixel - neighborhoodMeanImagePixel);

            count += itk::NumericTraits<RealType>::OneValue();
          }
          averageDistance /= count;
          minimumDistance = std::min(averageDistance, minimumDistance);
        }
      }

      if (itk::Math::AlmostEquals(minimumDistance, NumericTraits<RealType>::ZeroValue()))
      {
        minimumDistance = NumericTraits<RealType>::OneValue();
      }

      // Rician correction

      if (this->m_UseRicianNoiseModel)
      {
        for (unsigned int n = 0; n < neighborhoodPatchSize; n++)
        {
          IndexType neighborhoodPatchIndex = centerIndex + neighborhoodPatchOffsetList[n];
          if (!targetImageRegion.IsInside(neighborhoodPatchIndex))
          {
            continue;
          }

          if (itk::Math::AlmostEquals(minimumDistance, NumericTraits<RealType>::max()))
          {
            this->m_RicianBiasImage->SetPixel(neighborhoodPatchIndex, 0.0);
          }
          else
          {
            this->m_RicianBiasImage->SetPixel(neighborhoodPatchIndex, minimumDistance);
          }
        }
      }

      // Patch filtering

      for (unsigned int m = 0; m < neighborhoodSearchSize; m++)
      {
        if (!ItM.IndexInBounds(m) || m == static_cast<unsigned int>(0.5 * neighborhoodSearchSize))
        {
          continue;
        }

        IndexType neighborhoodIndex = ItM.GetIndex(m);

        if (inputImage->GetPixel(neighborhoodIndex) <= 0)
        {
          continue;
        }

        meanNeighborhoodPixel = this->m_MeanImage->GetPixel(neighborhoodIndex);
        varianceNeighborhoodPixel = this->m_VarianceImage->GetPixel(neighborhoodIndex);

        if (meanNeighborhoodPixel <= this->m_Epsilon || varianceNeighborhoodPixel <= this->m_Epsilon)
        {
          continue;
        }

        const RealType meanRatio = meanCenterPixel / meanNeighborhoodPixel;
        const RealType meanRatioInverse = (this->m_MaximumInputPixelIntensity - meanCenterPixel) /
                                          (this->m_MaximumInputPixelIntensity - meanNeighborhoodPixel);

        const RealType varianceRatio = varianceCenterPixel / varianceNeighborhoodPixel;

        if (((meanRatio > this->m_MeanThreshold &&
              meanRatio < itk::NumericTraits<RealType>::OneValue() / this->m_MeanThreshold) ||
             (meanRatioInverse > this->m_MeanThreshold &&
              meanRatioInverse < itk::NumericTraits<RealType>::OneValue() / this->m_MeanThreshold)) &&
            varianceRatio > this->m_VarianceThreshold &&
            varianceRatio < itk::NumericTraits<RealType>::OneValue() / this->m_VarianceThreshold)
        {

          RealType averageDistance = 0.0;
          RealType count = 0.0;
          for (unsigned int n = 0; n < neighborhoodPatchSize; n++)
          {
            IndexType searchNeighborhoodPatchIndex = neighborhoodIndex + neighborhoodPatchOffsetList[n];
            IndexType centerNeighborhoodPatchIndex = centerIndex + neighborhoodPatchOffsetList[n];
            if (!targetImageRegion.IsInside(searchNeighborhoodPatchIndex) ||
                !targetImageRegion.IsInside(centerNeighborhoodPatchIndex))
            {
              continue;
            }
            RealType distance1 = inputImage->GetPixel(searchNeighborhoodPatchIndex) -
                                 this->m_MeanImage->GetPixel(searchNeighborhoodPatchIndex);
            RealType distance2 = inputImage->GetPixel(centerNeighborhoodPatchIndex) -
                                 this->m_MeanImage->GetPixel(centerNeighborhoodPatchIndex);
            averageDistance += itk::Math::sqr(distance1 - distance2);
            count += itk::NumericTraits<RealType>::OneValue();
          }
          averageDistance /= count;

          RealType weight = itk::NumericTraits<RealType>::ZeroValue();
          if (averageDistance <= static_cast<RealType>(3.0) * minimumDistance)
          {
            weight = std::exp(-averageDistance / minimumDistance);
          }
          if (weight > maxWeight)
          {
            maxWeight = weight;
          }

          if (weight > itk::NumericTraits<RealType>::ZeroValue())
          {
            for (unsigned int n = 0; n < neighborhoodPatchSize; n++)
            {
              IndexType neighborhoodPatchIndex = neighborhoodIndex + neighborhoodPatchOffsetList[n];
              if (!targetImageRegion.IsInside(neighborhoodPatchIndex))
              {
                continue;
              }
              if (this->m_UseRicianNoiseModel)
              {
                weightedAverageIntensities[n] += weight * itk::Math::sqr(inputImage->GetPixel(neighborhoodPatchIndex));
              }
              else
              {
                weightedAverageIntensities[n] += weight * inputImage->GetPixel(neighborhoodPatchIndex);
              }
            }
            sumOfWeights += weight;
          }
        }
      }

      if (itk::Math::AlmostEquals(maxWeight, NumericTraits<RealType>::ZeroValue()))
      {
        maxWeight = NumericTraits<RealType>::OneValue();
      }
    }
    else
    {
      maxWeight = NumericTraits<RealType>::OneValue();
    }

    for (unsigned int n = 0; n < neighborhoodPatchSize; n++)
    {
      IndexType neighborhoodPatchIndex = centerIndex + neighborhoodPatchOffsetList[n];
      if (!targetImageRegion.IsInside(neighborhoodPatchIndex))
      {
        continue;
      }
      if (this->m_UseRicianNoiseModel)
      {
        weightedAverageIntensities[n] += maxWeight * itk::Math::sqr(inputImage->GetPixel(neighborhoodPatchIndex));
      }
      else
      {
        weightedAverageIntensities[n] += maxWeight * inputImage->GetPixel(neighborhoodPatchIndex);
      }
    }
    sumOfWeights += maxWeight;

    if (sumOfWeights > itk::NumericTraits<RealType>::ZeroValue())
    {
      for (unsigned int n = 0; n < neighborhoodPatchSize; n++)
      {
        IndexType neighborhoodPatchIndex = centerIndex + neighborhoodPatchOffsetList[n];
        if (!targetImageRegion.IsInside(neighborhoodPatchIndex))
        {
          continue;
        }
        typename OutputImageType::PixelType estimate = outputImage->GetPixel(neighborhoodPatchIndex);
        estimate += (weightedAverageIntensities[n] / sumOfWeights);

        outputImage->SetPixel(neighborhoodPatchIndex, estimate);
        this->m_ThreadContributionCountImage->SetPixel(
          neighborhoodPatchIndex, this->m_ThreadContributionCountImage->GetPixel(neighborhoodPatchIndex) + 1);
      }
    }

    ++ItM;
    ++ItV;

    progress.CompletedPixel();
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::AfterThreadedGenerateData()
{
  const MaskImageType * maskImage = this->GetMaskImage();

  if (this->m_UseRicianNoiseModel)
  {
    typedef DiscreteGaussianImageFilter<RealImageType, RealImageType> SmootherType;
    typename SmootherType::Pointer                                    smoother = SmootherType::New();
    smoother->SetInput(this->m_RicianBiasImage);
    smoother->SetVariance(this->m_SmoothingVariance);
    smoother->SetUseImageSpacing(true);
    smoother->Update();

    ImageRegionConstIterator<RealImageType> ItS(smoother->GetOutput(), smoother->GetOutput()->GetRequestedRegion());
    ImageRegionConstIteratorWithIndex<RealImageType> ItM(this->m_MeanImage, this->m_MeanImage->GetRequestedRegion());
    ImageRegionIterator<RealImageType> ItB(this->m_RicianBiasImage, this->m_RicianBiasImage->GetRequestedRegion());
    ItS.GoToBegin();
    ItM.GoToBegin();
    ItB.GoToBegin();

    while (!ItS.IsAtEnd())
    {
      if (ItS.Get() > itk::NumericTraits<RealType>::ZeroValue() &&
          (!maskImage || maskImage->GetPixel(ItM.GetIndex()) != NumericTraits<MaskPixelType>::ZeroValue()))
      {
        const RealType snr = ItM.Get() / std::sqrt(ItS.Get());

        RealType bias = static_cast<RealType>(2.0) * ItS.Get() / this->CalculateCorrectionFactor(snr);

        if (std::isnan(bias) || std::isinf(bias))
        {
          bias = itk::NumericTraits<RealType>::ZeroValue();
        }
        ItB.Set(bias);
      }

      ++ItS;
      ++ItM;
      ++ItB;
    }
  }

  ImageRegionIteratorWithIndex<OutputImageType> ItO(this->GetOutput(), this->GetOutput()->GetRequestedRegion());
  ImageRegionConstIterator<RealImageType>       ItL(this->m_ThreadContributionCountImage,
                                              this->m_ThreadContributionCountImage->GetRequestedRegion());

  for (ItO.GoToBegin(), ItL.GoToBegin(); !ItO.IsAtEnd(); ++ItO, ++ItL)
  {
    RealType estimate = ItO.Get();

    if (itk::Math::FloatAlmostEqual(ItL.Get(), itk::NumericTraits<RealType>::ZeroValue()))
    {
      continue;
    }

    estimate /= ItL.Get();

    if (this->m_UseRicianNoiseModel)
    {
      RealType bias = this->m_RicianBiasImage->GetPixel(ItO.GetIndex());

      estimate -= bias;
      if (estimate < itk::NumericTraits<RealType>::ZeroValue())
      {
        estimate = itk::NumericTraits<RealType>::ZeroValue();
      }
      estimate = std::sqrt(estimate);
    }

    ItO.Set(estimate);
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
typename AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::RealType
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::CalculateCorrectionFactor(
  RealType snr)
{
  const RealType snrSquared = itk::Math::sqr(snr);

  RealType value =
    static_cast<RealType>(2.0) + snrSquared -
    static_cast<RealType>(0.125) * static_cast<RealType>(Math::pi) *
      static_cast<RealType>(std::exp(static_cast<RealType>(-0.5) * snrSquared)) *
      itk::Math::sqr((static_cast<RealType>(2.0) + snrSquared) *
                       static_cast<RealType>(
                         this->m_ModifiedBesselCalculator.ModifiedBesselI0(static_cast<RealType>(0.25) * snrSquared)) +
                     snrSquared * static_cast<RealType>(this->m_ModifiedBesselCalculator.ModifiedBesselI1(
                                    static_cast<RealType>(0.25) * snrSquared)));

  if (value < static_cast<RealType>(0.001) || value > static_cast<RealType>(10.0))
  {
    value = itk::NumericTraits<RealType>::OneValue();
  }
  return value;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::PrintSelf(std::ostream & os,
                                                                                            Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  if (this->m_UseRicianNoiseModel)
  {
    os << indent << "Using Rician noise model." << std::endl;
  }
  else
  {
    os << indent << "Using Gaussian noise model." << std::endl;
  }

  os << indent << "Epsilon = " << this->m_Epsilon << std::endl;
  os << indent << "Mean threshold = " << this->m_MeanThreshold << std::endl;
  os << indent << "Variance threshold = " << this->m_VarianceThreshold << std::endl;
  os << indent << "Smoothing variance = " << this->m_SmoothingVariance << std::endl;

  os << indent
     << "Neighborhood radius for local mean and variance = " << this->m_NeighborhoodRadiusForLocalMeanAndVariance
     << std::endl;
}

} // end namespace itk

#endif
