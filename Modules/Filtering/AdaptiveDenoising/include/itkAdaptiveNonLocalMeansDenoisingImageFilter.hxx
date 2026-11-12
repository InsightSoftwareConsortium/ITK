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
#include "itkImageBufferRange.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkMeanImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkStatisticsImageFilter.h"
#include "itkTotalProgressReporter.h"
#include "itkVarianceImageFilter.h"

#include <algorithm>
#include <atomic>
#include <numeric>
#include <optional>
#include <vector>

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
  this->m_CenterMinimumDistanceImage = nullptr;

  this->m_RicianBiasImage = nullptr;

  this->m_NeighborhoodRadiusForLocalMeanAndVariance.Fill(1);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  const InputImageType * inputImage = this->GetInput();

  // Pad the requested region so that a partial region gets the same values as the same pixels of a full run.
  const RegionType       largestRegion = inputImage->GetLargestPossibleRegion();
  const RegionType       outputRegion = this->GetOutput()->GetRequestedRegion();
  NeighborhoodRadiusType biasRadius{};
  if (this->m_UseRicianNoiseModel)
  {
    const auto kernelRadius = this->MakeRicianBiasSmoother()->GetKernelRadius();
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      biasRadius[d] = kernelRadius[d];
    }
  }
  const auto padAndCrop = [&largestRegion](RegionType region, const NeighborhoodRadiusType & radius) {
    region.PadByRadius(radius);
    region.Crop(largestRegion);
    return region;
  };
  const RegionType biasRegion = padAndCrop(outputRegion, biasRadius);
  this->m_CenterRegion = padAndCrop(biasRegion, this->GetNeighborhoodPatchRadius());
  this->SetTargetImageRegion(
    padAndCrop(this->m_CenterRegion, this->GetNeighborhoodSearchRadius() + this->GetNeighborhoodPatchRadius()));

  typedef MeanImageFilter<InputImageType, RealImageType> MeanImageFilterType;
  typename MeanImageFilterType::Pointer                  meanImageFilter = MeanImageFilterType::New();
  meanImageFilter->SetInput(inputImage);
  meanImageFilter->SetRadius(this->GetNeighborhoodRadiusForLocalMeanAndVariance());

  this->m_MeanImage = meanImageFilter->GetOutput();
  this->m_MeanImage->Update();
  this->m_MeanImage->DisconnectPipeline();

  const RegionType targetImageRegion = this->GetTargetImageRegion();

  typedef VarianceImageFilter<InputImageType, RealImageType> VarianceImageFilterType;
  typename VarianceImageFilterType::Pointer                  varianceImageFilter = VarianceImageFilterType::New();
  varianceImageFilter->SetInput(inputImage);
  varianceImageFilter->SetRadius(this->GetNeighborhoodRadiusForLocalMeanAndVariance());

  this->m_VarianceImage = varianceImageFilter->GetOutput();
  this->m_VarianceImage->Update();
  this->m_VarianceImage->DisconnectPipeline();

  // Linear offsets need the target region and every image buffer to coincide; otherwise read through indices.
  const RegionType & bufferedRegion = inputImage->GetBufferedRegion();
  const bool         useLinearOffsets = bufferedRegion == targetImageRegion &&
                                this->m_MeanImage->GetBufferedRegion() == bufferedRegion &&
                                this->m_VarianceImage->GetBufferedRegion() == bufferedRegion;
  this->m_ResidualImage = nullptr;
  this->m_SquaredInputImage = nullptr;
  this->m_NumberOfLinearOffsetCenters = 0;
  if (useLinearOffsets)
  {
    this->m_ResidualImage = RealImageType::New();
    this->m_ResidualImage->CopyInformation(inputImage);
    this->m_ResidualImage->SetRegions(bufferedRegion);
    this->m_ResidualImage->Allocate();
    if (this->m_UseRicianNoiseModel)
    {
      this->m_SquaredInputImage = InputImageType::New();
      this->m_SquaredInputImage->CopyInformation(inputImage);
      this->m_SquaredInputImage->SetRegions(bufferedRegion);
      this->m_SquaredInputImage->Allocate();
    }
    const auto input = MakeImageBufferRange(inputImage);
    const auto mean = MakeImageBufferRange(this->m_MeanImage.GetPointer());
    const auto residual = MakeImageBufferRange(this->m_ResidualImage.GetPointer());
    const auto squared = MakeImageBufferRange(this->m_SquaredInputImage.GetPointer());
    this->GetMultiThreader()->ParallelizeArray(
      0,
      bufferedRegion.GetNumberOfPixels(),
      [&input, &mean, &residual, &squared](SizeValueType k) {
        residual[k] = input[k] - mean[k];
        if (!squared.empty())
        {
          squared[k] = Math::sqr(input[k]);
        }
      },
      nullptr);
  }


  // The patch mean squared residual does not depend on the patch center.
  RegionType searchRegion = this->m_CenterRegion;
  searchRegion.PadByRadius(this->GetNeighborhoodSearchRadius());
  searchRegion.Crop(this->m_MeanImage->GetBufferedRegion());
  const NeighborhoodOffsetListType neighborhoodPatchOffsetList = this->GetNeighborhoodPatchOffsetList();

  this->m_IntensitySquaredDistanceImage = RealImageType::New();
  this->m_IntensitySquaredDistanceImage->CopyInformation(inputImage);
  // On the linear path this image shares the input's offset table, so it covers the buffered region; only
  // searchRegion is filled, and the readers only read searchRegion.
  this->m_IntensitySquaredDistanceImage->SetRegions(useLinearOffsets ? bufferedRegion : searchRegion);
  this->m_IntensitySquaredDistanceImage->Allocate();

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
    searchRegion,
    [this, inputImage, &targetImageRegion, &searchRegion, &neighborhoodPatchOffsetList](const RegionType & region) {
      TotalProgressReporter progress(this, searchRegion.GetNumberOfPixels(), 100, 0.05f);
      for (ImageRegionIteratorWithIndex<RealImageType> It(this->m_IntensitySquaredDistanceImage, region); !It.IsAtEnd();
           ++It)
      {
        RealType averageDistance = NumericTraits<RealType>::ZeroValue();
        RealType count = NumericTraits<RealType>::ZeroValue();

        for (const auto & offset : neighborhoodPatchOffsetList)
        {
          const IndexType neighborhoodPatchIndex = It.GetIndex() + offset;

          if (!targetImageRegion.IsInside(neighborhoodPatchIndex))
          {
            continue;
          }
          const auto neighborhoodInputImagePixel = static_cast<RealType>(inputImage->GetPixel(neighborhoodPatchIndex));
          const RealType neighborhoodMeanImagePixel = this->m_MeanImage->GetPixel(neighborhoodPatchIndex);
          averageDistance += Math::sqr(neighborhoodInputImagePixel - neighborhoodMeanImagePixel);

          count += NumericTraits<RealType>::OneValue();
        }
        // A patch with no voxel in the target region has residual 0/0; store NaN without dividing.
        It.Set(count > 0 ? averageDistance / count : NumericTraits<RealType>::quiet_NaN());
        progress.CompletedPixel();
      }
    },
    nullptr);

  typedef StatisticsImageFilter<InputImageType> StatsFilterType;
  typename StatsFilterType::Pointer             statsFilter = StatsFilterType::New();
  statsFilter->SetInput(inputImage);
  statsFilter->Update();

  this->m_MaximumInputPixelIntensity = static_cast<RealType>(statsFilter->GetMaximum());
  this->m_MinimumInputPixelIntensity = static_cast<RealType>(statsFilter->GetMinimum());

  if (this->m_UseRicianNoiseModel)
  {
    this->m_RicianBiasImage = RealImageType::New();
    this->m_RicianBiasImage->CopyInformation(inputImage);
    this->m_RicianBiasImage->SetRegions(biasRegion);
    this->m_RicianBiasImage->Allocate(true);

    this->m_CenterMinimumDistanceImage = RealImageType::New();
    this->m_CenterMinimumDistanceImage->CopyInformation(inputImage);
    this->m_CenterMinimumDistanceImage->SetRegions(this->m_CenterRegion);
    this->m_CenterMinimumDistanceImage->Allocate();
    this->m_CenterMinimumDistanceImage->FillBuffer(Self::NoMinimumDistance);
  }

  this->AllocateOutputs();
  // Output buffer needs to be zero initialized
  this->GetOutput()->FillBuffer(0.0);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateData()
{
  this->BeforeThreadedGenerateData();

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());

  const RegionType              centerRegion = this->m_CenterRegion;
  const RegionType              targetImageRegion = this->GetTargetImageRegion();
  static constexpr unsigned int slabAxis = ImageDimension - 1;

  // Fixed slabs on the grid of the whole image make the summation order independent of the threads and the region.
  static constexpr SizeValueType slabThickness = 4;
  const IndexValueType           slabOrigin = this->GetInput()->GetLargestPossibleRegion().GetIndex(slabAxis);
  const SizeValueType            firstSlab =
    static_cast<SizeValueType>(centerRegion.GetIndex(slabAxis) - slabOrigin) / slabThickness;
  const SizeValueType lastSlab =
    static_cast<SizeValueType>(centerRegion.GetUpperIndex()[slabAxis] - slabOrigin) / slabThickness;
  const SizeValueType numberOfSlabs = lastSlab - firstSlab + 1;

  std::vector<typename OutputImageType::Pointer> slabEstimates(numberOfSlabs);

  // Each work unit takes the next unprocessed slab, so slabs of different cost are balanced across work units.
  std::atomic<SizeValueType> nextSlab{ 0 };
  this->GetMultiThreader()->ParallelizeArray(
    0,
    std::min<SizeValueType>(this->GetNumberOfWorkUnits(), numberOfSlabs),
    [this, &centerRegion, &targetImageRegion, &slabEstimates, &nextSlab, numberOfSlabs, slabOrigin, firstSlab](
      SizeValueType) {
      // One reporter across this work unit's slabs, which can each be smaller than its update interval.
      TotalProgressReporter progress(this, centerRegion.GetNumberOfPixels(), 100, 0.95f);
      for (SizeValueType slab = nextSlab++; slab < numberOfSlabs; slab = nextSlab++)
      {
        progress.CheckAbortGenerateData();
        RegionType slabRegion = centerRegion;
        slabRegion.SetIndex(slabAxis, slabOrigin + static_cast<IndexValueType>((firstSlab + slab) * slabThickness));
        slabRegion.SetSize(slabAxis, slabThickness);
        slabRegion.Crop(centerRegion);

        RegionType estimateRegion = slabRegion;
        estimateRegion.PadByRadius(this->GetNeighborhoodPatchRadius());
        estimateRegion.Crop(targetImageRegion);

        auto estimateImage = OutputImageType::New();
        estimateImage->CopyInformation(this->GetOutput());
        estimateImage->SetRegions(estimateRegion);
        estimateImage->AllocateInitialized();

        this->AccumulatePatchEstimates(slabRegion, estimateImage, progress);
        slabEstimates[slab] = estimateImage;
      }
    },
    nullptr);

  OutputImageType * outputImage = this->GetOutput();
  this->GetMultiThreader()->ParallelizeArray(
    0,
    targetImageRegion.GetSize(slabAxis),
    [&targetImageRegion, &slabEstimates, outputImage](SizeValueType k) {
      const IndexValueType sliceIndex = targetImageRegion.GetIndex(slabAxis) + static_cast<IndexValueType>(k);
      for (const auto & estimateImage : slabEstimates)
      {
        RegionType sliceRegion = estimateImage->GetBufferedRegion();
        if (sliceIndex < sliceRegion.GetIndex(slabAxis) ||
            sliceIndex >= sliceRegion.GetIndex(slabAxis) + static_cast<IndexValueType>(sliceRegion.GetSize(slabAxis)))
        {
          continue;
        }
        sliceRegion.SetIndex(slabAxis, sliceIndex);
        sliceRegion.SetSize(slabAxis, 1);
        if (!sliceRegion.Crop(outputImage->GetBufferedRegion()))
        {
          continue;
        }
        ImageRegionConstIterator<OutputImageType> ItE(estimateImage, sliceRegion);
        ImageRegionIterator<OutputImageType>      ItO(outputImage, sliceRegion);
        for (; !ItE.IsAtEnd(); ++ItE, ++ItO)
        {
          ItO.Set(ItO.Get() + ItE.Get());
        }
      }
    },
    nullptr);
  slabEstimates.clear();

  this->AfterThreadedGenerateData();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::AccumulatePatchEstimates(
  const RegionType &      region,
  OutputImageType *       outputImage,
  TotalProgressReporter & progress)
{
  const InputImageType * inputImage = this->GetInput();
  const MaskImageType *  maskImage = this->GetMaskImage();

  const RegionType targetImageRegion = this->GetTargetImageRegion();

  const NeighborhoodOffsetListType neighborhoodPatchOffsetList = this->GetNeighborhoodPatchOffsetList();

  const NeighborhoodRadiusType neighborhoodSearchRadius = this->GetNeighborhoodSearchRadius();

  ConstNeighborhoodIterator<RealImageType> ItV(neighborhoodSearchRadius, this->m_VarianceImage, region);
  ConstNeighborhoodIterator<RealImageType> ItM(neighborhoodSearchRadius, this->m_MeanImage, region);

  const unsigned int neighborhoodSearchSize = this->GetNeighborhoodSearchSize();
  const unsigned int neighborhoodPatchSize = this->GetNeighborhoodPatchSize();
  const unsigned int centerSearchPosition = static_cast<unsigned int>(0.5 * neighborhoodSearchSize);

  Array<RealType> weightedAverageIntensities(neighborhoodPatchSize);

  struct IndexedReader
  {
    const InputImageType *             input;
    const RealImageType *              mean;
    const RealImageType *              variance;
    const RealImageType *              patchResidual;
    bool                               useRicianNoiseModel;
    const RegionType &                 target;
    const NeighborhoodOffsetListType & patchOffsets;
    IndexType                          center;

    InputPixelType
    Input(const IndexType & position) const
    {
      return input->GetPixel(position);
    }
    RealType
    Mean(const IndexType & position) const
    {
      return mean->GetPixel(position);
    }
    RealType
    Variance(const IndexType & position) const
    {
      return variance->GetPixel(position);
    }
    RealType
    PatchResidual(const IndexType & position) const
    {
      return patchResidual->GetPixel(position);
    }
    RealType
    PatchDistance(const IndexType & position) const
    {
      RealType averageDistance = 0.0;
      RealType count = 0.0;
      for (const auto & offset : patchOffsets)
      {
        const IndexType searchPatchIndex = position + offset;
        const IndexType centerPatchIndex = center + offset;
        if (!target.IsInside(searchPatchIndex) || !target.IsInside(centerPatchIndex))
        {
          continue;
        }
        const RealType distance1 = input->GetPixel(searchPatchIndex) - mean->GetPixel(searchPatchIndex);
        const RealType distance2 = input->GetPixel(centerPatchIndex) - mean->GetPixel(centerPatchIndex);
        averageDistance += Math::sqr(distance1 - distance2);
        count += NumericTraits<RealType>::OneValue();
      }
      return averageDistance / count;
    }
    void
    AddPatch(const IndexType & position, RealType weight, Array<RealType> & weightedAverageIntensities) const
    {
      for (unsigned int n = 0; n < patchOffsets.size(); ++n)
      {
        const IndexType patchIndex = position + patchOffsets[n];
        if (!target.IsInside(patchIndex))
        {
          continue;
        }
        if (useRicianNoiseModel)
        {
          weightedAverageIntensities[n] += weight * Math::sqr(input->GetPixel(patchIndex));
        }
        else
        {
          weightedAverageIntensities[n] += weight * input->GetPixel(patchIndex);
        }
      }
    }
  };

  // Every image read here shares one offset table, so one linear offset serves all of them. Interior centers keep
  // every offset non-negative and inside the buffer.
  using InputRangeType = ImageBufferRange<const InputImageType>;
  using RealRangeType = ImageBufferRange<const RealImageType>;
  struct LinearReader
  {
    InputRangeType                       input;
    RealRangeType                        mean;
    RealRangeType                        variance;
    RealRangeType                        patchResidual;
    RealRangeType                        residual;
    InputRangeType                       patchIntensity;
    const std::vector<OffsetValueType> & patchOffsets;
    OffsetValueType                      center;

    InputPixelType
    Input(OffsetValueType position) const
    {
      return input[static_cast<size_t>(position)];
    }
    RealType
    Mean(OffsetValueType position) const
    {
      return mean[static_cast<size_t>(position)];
    }
    RealType
    Variance(OffsetValueType position) const
    {
      return variance[static_cast<size_t>(position)];
    }
    RealType
    PatchResidual(OffsetValueType position) const
    {
      return patchResidual[static_cast<size_t>(position)];
    }
    RealType
    PatchDistance(OffsetValueType position) const
    {
      RealType averageDistance = 0.0;
      for (const OffsetValueType offset : patchOffsets)
      {
        averageDistance +=
          Math::sqr(residual[static_cast<size_t>(position + offset)] - residual[static_cast<size_t>(center + offset)]);
      }
      return averageDistance / static_cast<RealType>(patchOffsets.size());
    }
    void
    AddPatch(OffsetValueType position, RealType weight, Array<RealType> & weightedAverageIntensities) const
    {
      for (unsigned int n = 0; n < patchOffsets.size(); ++n)
      {
        weightedAverageIntensities[n] += weight * patchIntensity[static_cast<size_t>(position + patchOffsets[n])];
      }
    }
  };

  RegionType                   interiorRegion = targetImageRegion;
  std::vector<OffsetValueType> searchOffsets(neighborhoodSearchSize);
  std::vector<OffsetValueType> patchOffsets(neighborhoodPatchSize);
  bool                         hasLinearInterior = false;
  if (this->m_ResidualImage)
  {
    const OffsetValueType * offsetTable = this->m_ResidualImage->GetOffsetTable();
    for (unsigned int m = 0; m < neighborhoodSearchSize; ++m)
    {
      const auto offset = ItM.GetOffset(m);
      searchOffsets[m] = 0;
      for (unsigned int d = 0; d < ImageDimension; ++d)
      {
        searchOffsets[m] += offset[d] * offsetTable[d];
      }
    }
    for (unsigned int n = 0; n < neighborhoodPatchSize; ++n)
    {
      patchOffsets[n] = 0;
      for (unsigned int d = 0; d < ImageDimension; ++d)
      {
        patchOffsets[n] += neighborhoodPatchOffsetList[n][d] * offsetTable[d];
      }
    }
    hasLinearInterior = interiorRegion.ShrinkByRadius(neighborhoodSearchRadius + this->GetNeighborhoodPatchRadius());
  }
  if (hasLinearInterior)
  {
    RegionType linearCenters = region;
    if (linearCenters.Crop(interiorRegion))
    {
      this->m_NumberOfLinearOffsetCenters += linearCenters.GetNumberOfPixels();
    }
  }
  const RealImageType *  meanImage = this->m_MeanImage;
  const RealImageType *  varianceImage = this->m_VarianceImage;
  const RealImageType *  patchResidualImage = this->m_IntensitySquaredDistanceImage;
  const RealImageType *  residualImage = nullptr;
  const InputImageType * patchIntensityImage = inputImage;
  if (hasLinearInterior)
  {
    residualImage = this->m_ResidualImage;
    if (this->m_UseRicianNoiseModel)
    {
      patchIntensityImage = this->m_SquaredInputImage;
    }
  }
  LinearReader  linearReader{ MakeImageBufferRange(inputImage),
                             MakeImageBufferRange(meanImage),
                             MakeImageBufferRange(varianceImage),
                             MakeImageBufferRange(patchResidualImage),
                             MakeImageBufferRange(residualImage),
                             MakeImageBufferRange(patchIntensityImage),
                             patchOffsets,
                             0 };
  IndexedReader indexedReader{ inputImage,
                               meanImage,
                               varianceImage,
                               patchResidualImage,
                               this->m_UseRicianNoiseModel,
                               targetImageRegion,
                               neighborhoodPatchOffsetList,
                               IndexType() };

  struct PatchWeights
  {
    RealType maxWeight;
    RealType sumOfWeights;
  };
  const auto weighPatches = [&](const auto &   reader,
                                const auto &   searchPosition,
                                const RealType meanCenterPixel,
                                const RealType varianceCenterPixel) {
    RealType   maxWeight = NumericTraits<RealType>::ZeroValue();
    RealType   sumOfWeights = NumericTraits<RealType>::ZeroValue();
    const auto isSimilarSearchPosition = [&](const auto & position) {
      if (reader.Input(position) <= 0)
      {
        return false;
      }
      const RealType meanNeighborhoodPixel = reader.Mean(position);
      const RealType varianceNeighborhoodPixel = reader.Variance(position);
      return meanNeighborhoodPixel > this->m_Epsilon && varianceNeighborhoodPixel > this->m_Epsilon &&
             this->IsSimilarSearchPosition(
               meanCenterPixel, varianceCenterPixel, meanNeighborhoodPixel, varianceNeighborhoodPixel);
    };

    RealType minimumDistance = NumericTraits<RealType>::max();
    for (unsigned int m = 0; m < neighborhoodSearchSize; m++)
    {
      const auto position = searchPosition(m);
      if (position && isSimilarSearchPosition(*position))
      {
        minimumDistance = std::min(reader.PatchResidual(*position), minimumDistance);
      }
    }

    if (Math::AlmostEquals(minimumDistance, NumericTraits<RealType>::ZeroValue()))
    {
      minimumDistance = NumericTraits<RealType>::OneValue();
    }

    if (this->m_UseRicianNoiseModel)
    {
      this->m_CenterMinimumDistanceImage->SetPixel(
        ItM.GetIndex(), Math::AlmostEquals(minimumDistance, NumericTraits<RealType>::max()) ? 0.0 : minimumDistance);
    }

    for (unsigned int m = 0; m < neighborhoodSearchSize; m++)
    {
      const auto position = searchPosition(m);
      if (!position || !isSimilarSearchPosition(*position))
      {
        continue;
      }
      const RealType averageDistance = reader.PatchDistance(*position);

      RealType weight = NumericTraits<RealType>::ZeroValue();
      if (averageDistance <= static_cast<RealType>(3.0) * minimumDistance)
      {
        weight = std::exp(-averageDistance / minimumDistance);
      }
      maxWeight = std::max(weight, maxWeight);

      if (weight > NumericTraits<RealType>::ZeroValue())
      {
        reader.AddPatch(*position, weight, weightedAverageIntensities);
        sumOfWeights += weight;
      }
    }

    if (Math::AlmostEquals(maxWeight, NumericTraits<RealType>::ZeroValue()))
    {
      maxWeight = NumericTraits<RealType>::OneValue();
    }
    return PatchWeights{ maxWeight, sumOfWeights };
  };

  for (ItM.GoToBegin(), ItV.GoToBegin(); !ItM.IsAtEnd(); ++ItM, ++ItV)
  {
    const IndexType centerIndex = ItM.GetIndex();

    const InputPixelType inputCenterPixel = inputImage->GetPixel(centerIndex);
    const RealType       meanCenterPixel = this->m_MeanImage->GetPixel(centerIndex);
    const RealType       varianceCenterPixel = this->m_VarianceImage->GetPixel(centerIndex);

    PatchWeights weights{ NumericTraits<RealType>::OneValue(), NumericTraits<RealType>::ZeroValue() };
    weightedAverageIntensities.Fill(NumericTraits<RealType>::ZeroValue());

    const bool isInteriorCenter = hasLinearInterior && interiorRegion.IsInside(centerIndex);
    linearReader.center = isInteriorCenter ? this->m_ResidualImage->ComputeOffset(centerIndex) : 0;
    indexedReader.center = centerIndex;

    const bool isWeightedCenter =
      inputCenterPixel > 0 && meanCenterPixel > this->m_Epsilon && varianceCenterPixel > this->m_Epsilon &&
      (!maskImage || maskImage->GetPixel(centerIndex) != NumericTraits<MaskPixelType>::ZeroValue());
    const auto processCenter = [&](const auto & reader, const auto & center, const auto & searchPosition) {
      if (isWeightedCenter)
      {
        weights = weighPatches(reader, searchPosition, meanCenterPixel, varianceCenterPixel);
      }
      reader.AddPatch(center, weights.maxWeight, weightedAverageIntensities);
    };
    if (isInteriorCenter)
    {
      processCenter(linearReader, linearReader.center, [&](unsigned int m) {
        return m == centerSearchPosition ? std::optional<OffsetValueType>()
                                         : std::optional<OffsetValueType>(linearReader.center + searchOffsets[m]);
      });
    }
    else
    {
      processCenter(indexedReader, centerIndex, [&](unsigned int m) {
        return !ItM.IndexInBounds(m) || m == centerSearchPosition ? std::optional<IndexType>()
                                                                  : std::optional<IndexType>(ItM.GetIndex(m));
      });
    }
    const RealType sumOfWeights = weights.sumOfWeights + weights.maxWeight;

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
      }
    }

    progress.CompletedPixel();
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::AfterThreadedGenerateData()
{
  const MaskImageType *            maskImage = this->GetMaskImage();
  const RegionType                 centerRegion = this->m_CenterRegion;
  const NeighborhoodOffsetListType neighborhoodPatchOffsetList = this->GetNeighborhoodPatchOffsetList();

  if (this->m_UseRicianNoiseModel)
  {
    // Raster-order patch offsets find first the last center, in raster order, whose patch covers the pixel.
    this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
      this->m_RicianBiasImage->GetBufferedRegion(),
      [this, &centerRegion, &neighborhoodPatchOffsetList](const RegionType & region) {
        for (ImageRegionIteratorWithIndex<RealImageType> It(this->m_RicianBiasImage, region); !It.IsAtEnd(); ++It)
        {
          for (const auto & offset : neighborhoodPatchOffsetList)
          {
            const IndexType centerIndex = It.GetIndex() - offset;
            if (centerRegion.IsInside(centerIndex))
            {
              const RealType minimumDistance = this->m_CenterMinimumDistanceImage->GetPixel(centerIndex);
              if (itk::Math::NotExactlyEquals(minimumDistance, Self::NoMinimumDistance))
              {
                It.Set(minimumDistance);
                break;
              }
            }
          }
        }
      },
      nullptr);
    this->m_CenterMinimumDistanceImage = nullptr;

    const auto smoother = this->MakeRicianBiasSmoother();
    smoother->SetInput(this->m_RicianBiasImage);
    smoother->Update();

    const RegionType                                 biasRegion = this->m_RicianBiasImage->GetBufferedRegion();
    ImageRegionConstIterator<RealImageType>          ItS(smoother->GetOutput(), biasRegion);
    ImageRegionConstIteratorWithIndex<RealImageType> ItM(this->m_MeanImage, biasRegion);
    ImageRegionIterator<RealImageType>               ItB(this->m_RicianBiasImage, biasRegion);
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

  // Every center adds its full patch, so a pixel's contribution count is the number of centers whose patch covers it.
  const NeighborhoodRadiusType patchRadius = this->GetNeighborhoodPatchRadius();

  ImageRegionIteratorWithIndex<OutputImageType> ItO(this->GetOutput(), this->GetOutput()->GetRequestedRegion());

  for (ItO.GoToBegin(); !ItO.IsAtEnd(); ++ItO)
  {
    RealType estimate = ItO.Get();

    const IndexType index = ItO.GetIndex();
    RealType        count = itk::NumericTraits<RealType>::OneValue();
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      const IndexValueType first =
        std::max(index[d] - static_cast<IndexValueType>(patchRadius[d]), centerRegion.GetIndex(d));
      const IndexValueType last =
        std::min(index[d] + static_cast<IndexValueType>(patchRadius[d]),
                 centerRegion.GetIndex(d) + static_cast<IndexValueType>(centerRegion.GetSize(d)) - 1);
      count *= static_cast<RealType>(std::max<IndexValueType>(last - first + 1, 0));
    }

    if (itk::Math::FloatAlmostEqual(count, itk::NumericTraits<RealType>::ZeroValue()))
    {
      continue;
    }

    estimate /= count;

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

  // Only the threaded pass reads these.
  this->m_IntensitySquaredDistanceImage = nullptr;
  this->m_ResidualImage = nullptr;
  this->m_SquaredInputImage = nullptr;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  const_cast<InputImageType *>(this->GetInput())->SetRequestedRegionToLargestPossibleRegion();
  {
    auto * mask = const_cast<MaskImageType *>(this->GetMaskImage());
    if (mask != nullptr)
    {
      mask->SetRequestedRegionToLargestPossibleRegion();
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
auto
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::MakeRicianBiasSmoother() const ->
  typename RicianBiasSmootherType::Pointer
{
  // The kernel depends on the spacing, so a geometry-only image stands in for the bias image.
  auto geometry = RealImageType::New();
  geometry->CopyInformation(this->GetInput());

  auto smoother = RicianBiasSmootherType::New();
  smoother->SetInput(geometry);
  smoother->SetVariance(this->m_SmoothingVariance);
  smoother->SetUseImageSpacing(true);
  return smoother;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
bool
AdaptiveNonLocalMeansDenoisingImageFilter<TInputImage, TOutputImage, TMaskImage>::IsSimilarSearchPosition(
  RealType meanCenterPixel,
  RealType varianceCenterPixel,
  RealType meanNeighborhoodPixel,
  RealType varianceNeighborhoodPixel) const
{
  const RealType meanRatio = meanCenterPixel / meanNeighborhoodPixel;
  const RealType meanRatioInverse = (this->m_MaximumInputPixelIntensity - meanCenterPixel) /
                                    (this->m_MaximumInputPixelIntensity - meanNeighborhoodPixel);

  const RealType varianceRatio = varianceCenterPixel / varianceNeighborhoodPixel;

  return ((meanRatio > this->m_MeanThreshold &&
           meanRatio < NumericTraits<RealType>::OneValue() / this->m_MeanThreshold) ||
          (meanRatioInverse > this->m_MeanThreshold &&
           meanRatioInverse < NumericTraits<RealType>::OneValue() / this->m_MeanThreshold)) &&
         varianceRatio > this->m_VarianceThreshold &&
         varianceRatio < NumericTraits<RealType>::OneValue() / this->m_VarianceThreshold;
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
