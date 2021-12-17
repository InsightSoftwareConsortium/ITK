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

#ifndef itkTileMergeImageFilter_hxx
#define itkTileMergeImageFilter_hxx


#include "itkMultiThreaderBase.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <functional>

namespace itk
{
template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::TileMergeImageFilter()
{
  this->SetMontageSize(this->m_MontageSize); // initialize the rest of arrays

  // required for GenerateOutputInformation to be called
  this->SetNthOutput(0, this->MakeOutput(0).GetPointer());
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "CropToFill: " << (m_CropToFill ? "Yes" : "No") << std::endl;
  os << indent << "Background: " << m_Background << std::endl;
  os << indent << "RegionsSize: " << m_Regions.size() << std::endl;

  auto nullCount = std::count(m_Transforms.begin(), m_Transforms.end(), nullptr);
  os << indent << "Transforms (filled/capacity): " << m_Transforms.size() - nullCount << "/" << m_Transforms.size()
     << std::endl;

  auto fullCount = std::count_if(m_Tiles.begin(), m_Tiles.end(), [](ImagePointer im) {
    return im.IsNotNull() && im->GetBufferedRegion().GetNumberOfPixels() > 0;
  });
  os << indent << "InputTiles (filled/capacity): " << fullCount << "/" << m_Tiles.size() << std::endl;

  os << indent << "Montage: " << m_Montage.GetPointer() << std::endl;
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::SetMontage(const Superclass * montage)
{
  if (m_Montage != montage)
  {
    m_Montage = montage;
    this->SetMontageSize(montage->m_MontageSize);
    this->m_FinishedPairs.store(montage->m_FinishedPairs);
    this->m_OriginAdjustment = montage->m_OriginAdjustment;
    this->m_ForcedSpacing = montage->m_ForcedSpacing;

    for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
    {
      if (!std::equal_to<const void *>{}(montage->GetInput(i), montage->m_Dummy.GetPointer()))
      {
        this->SetNthInput(i, const_cast<DataObject *>(montage->GetInput(i)));
        this->m_Filenames[i] = montage->m_Filenames[i]; // might still be set
      }
      else
      {
        this->SetInputTile(this->LinearIndexTonDIndex(i), montage->m_Filenames[i]);
      }
      using TransformCOT = const typename Superclass::TransformOutputType;
      this->m_Transforms[i] = static_cast<TransformCOT *>(m_Montage->GetOutput(i))->Get();
    }

    this->m_MinInner = montage->m_MinInner;
    this->m_MaxInner = montage->m_MaxInner;
    this->m_MinOuter = montage->m_MinOuter;
    this->m_MaxOuter = montage->m_MaxOuter;

    this->Modified();
  }
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
TImageType *
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::GetOutput()
{
  return itkDynamicCastInDebugMode<ImageType *>(this->GetPrimaryOutput());
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
const TImageType *
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::GetOutput() const
{
  return itkDynamicCastInDebugMode<const ImageType *>(this->GetPrimaryOutput());
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
TImageType *
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::GetOutput(unsigned int idx)
{
  auto * out = dynamic_cast<ImageType *>(this->ProcessObject::GetOutput(idx));
  if (out == nullptr && this->ProcessObject::GetOutput(idx) != nullptr)
  {
    itkWarningMacro(<< "Unable to convert output number " << idx << " to type " << typeid(ImageType).name());
  }
  return out;
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::SetMontageSize(SizeType montageSize)
{
  Superclass::SetMontageSize(montageSize);
  m_Transforms.resize(this->m_LinearMontageSize);
  m_Tiles.resize(this->m_LinearMontageSize);
  this->SetNumberOfRequiredOutputs(1);
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::SetTileTransform(TileIndexType         position,
                                                                                        const TransformType * transform)
{
  SizeValueType linInd = this->nDIndexToLinearIndex(position);
  if (m_Transforms[linInd].IsNull() || m_Transforms[linInd]->GetParameters() != transform->GetParameters() ||
      m_Transforms[linInd]->GetFixedParameters() != transform->GetFixedParameters())
  {
    m_Transforms[linInd] = transform;
    this->Modified();
  }
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::SplitRegionAndCopyContributions(
  std::vector<RegionType> &        regions,
  std::vector<ContributingTiles> & regionContributors,
  RegionType                       newRegion,
  size_t                           oldRegionIndex,
  SizeValueType                    tileIndex)
{
  for (int d = ImageDimension - 1; d >= 0; d--)
  {
    SizeValueType  newRegionSize = newRegion.GetSize(d);
    IndexValueType newRegionIndex = newRegion.GetIndex(d);
    IndexValueType newRegionEnd = newRegionIndex + IndexValueType(newRegionSize);

    SizeValueType  originalRegionSize = regions[oldRegionIndex].GetSize(d);
    IndexValueType originalRegionIndex = regions[oldRegionIndex].GetIndex(d);
    IndexValueType originalRegionEnd = originalRegionIndex + IndexValueType(originalRegionSize);

    if (newRegionIndex < originalRegionEnd && originalRegionIndex < newRegionIndex)
    {
      RegionType remnant = regions[oldRegionIndex];
      remnant.SetSize(d, newRegionIndex - originalRegionIndex);
      regions.push_back(remnant);
      regionContributors.push_back(regionContributors[oldRegionIndex]);

      regions[oldRegionIndex].SetSize(d, originalRegionSize - (newRegionIndex - originalRegionIndex));
      regions[oldRegionIndex].SetIndex(d, newRegionIndex);

      // update original region's size and index
      originalRegionSize = regions[oldRegionIndex].GetSize(d);
      originalRegionIndex = regions[oldRegionIndex].GetIndex(d);
      assert(originalRegionEnd == originalRegionIndex + IndexValueType(originalRegionSize));
    }

    if (originalRegionIndex < newRegionEnd && newRegionEnd < originalRegionEnd)
    {
      RegionType remnant = regions[oldRegionIndex];
      regions[oldRegionIndex].SetSize(d, newRegionEnd - originalRegionIndex);

      remnant.SetSize(d, originalRegionSize - (newRegionEnd - originalRegionIndex));
      remnant.SetIndex(d, newRegionEnd);
      regions.push_back(remnant);
      regionContributors.push_back(regionContributors[oldRegionIndex]);
    }
  }
  regionContributors[oldRegionIndex].insert(tileIndex);
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
typename TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::RegionType
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::ConstructRegion(ContinuousIndexType minIndex,
                                                                                       ContinuousIndexType maxIndex)
{
  ImageIndexType ind;
  SizeType       size;
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    ind[d] = std::round(minIndex[d]);
    size[d] = std::round(maxIndex[d] - ind[d]);
  }
  RegionType region;
  region.SetIndex(ind);
  region.SetSize(size);
  return region;
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
SizeValueType
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::DistanceFromEdge(ImageIndexType index,
                                                                                        RegionType     region)
{
  SizeValueType dist = NumericTraits<SizeValueType>::max();
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    SizeValueType dimDist =
      std::min<IndexValueType>(index[d] - region.GetIndex(d), region.GetIndex(d) + region.GetSize(d) - index[d]);
    dist = std::min(dist, dimDist);
  }
  return 1 + dist;
}


template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
typename TImageType::ConstPointer
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::GetImage(TileIndexType nDIndex,
                                                                                RegionType    wantedRegion)
{
  SizeValueType linearIndex = this->nDIndexToLinearIndex(nDIndex);

  ImagePointer                outputImage = this->GetOutput();
  RegionType                  reqR = outputImage->GetRequestedRegion();
  std::lock_guard<std::mutex> lockGuard(this->m_TileReadLocks[linearIndex]);
  if (m_Tiles[linearIndex].IsNotNull())
  {
    RegionType r = m_Tiles[linearIndex]->GetBufferedRegion();
    if (r.Crop(reqR) && r.IsInside(wantedRegion))
    {
      return m_Tiles[linearIndex];
    }
  }

  bool onlyMetadata = (wantedRegion.GetNumberOfPixels() == 0);
  m_Tiles[linearIndex] = Superclass::template GetImageHelper<ImageType>(nDIndex, onlyMetadata, reqR);
  return m_Tiles[linearIndex];
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  TileIndexType     nDIndex0 = { 0 };
  RegionType        reg0;
  ImageConstPointer input0 = this->GetImage(nDIndex0, reg0);

  if (m_Montage.IsNull())
  {
    // initialize mosaic bounds
    this->m_MinInner.Fill(NumericTraits<typename TInterpolator::CoordRepType>::NonpositiveMin());
    this->m_MinOuter.Fill(NumericTraits<typename TInterpolator::CoordRepType>::max());
    this->m_MaxOuter.Fill(NumericTraits<typename TInterpolator::CoordRepType>::NonpositiveMin());
    this->m_MaxInner.Fill(NumericTraits<typename TInterpolator::CoordRepType>::max());

    for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
    {
      TileIndexType     nDIndex = this->LinearIndexTonDIndex(i);
      ImageConstPointer input = this->GetImage(nDIndex, reg0);
      this->UpdateMosaicBounds(nDIndex,
                               m_Transforms[i],
                               reinterpret_cast<const typename Superclass::ImageType *>(input.GetPointer()),
                               reinterpret_cast<const typename Superclass::ImageType *>(input0.GetPointer()));
    }
  }

  // clean up internal variables
  m_InputMappings.clear();
  m_InputsContinuousIndices.clear();
  m_Regions.clear();
  m_RegionContributors.clear();

  ImagePointer outputImage = this->GetOutput();
  outputImage->CopyInformation(input0); // origin, spacing, direction

  // determine output region
  RegionType totalRegion;
  if (m_CropToFill)
  {
    totalRegion = this->ConstructRegion(this->m_MinInner, this->m_MaxInner);
  }
  else
  {
    totalRegion = this->ConstructRegion(this->m_MinOuter, this->m_MaxOuter);
  }
  outputImage->SetRegions(totalRegion);

  // determine where does each input tile map into the output image
  m_InputMappings.resize(this->m_LinearMontageSize);
  m_InputsContinuousIndices.resize(this->m_LinearMontageSize);
  for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
  {
    TransformPointer inverseT = TransformType::New();
    m_Transforms[i]->GetInverse(inverseT);
    TileIndexType     nDIndex = this->LinearIndexTonDIndex(i);
    ImageConstPointer input = this->GetImage(nDIndex, reg0);
    PointType         iOrigin = input->GetOrigin();
    iOrigin = inverseT->TransformPoint(iOrigin);

    ContinuousIndexType ci;
    outputImage->TransformPhysicalPointToContinuousIndex(iOrigin, ci);
    for (unsigned d = 0; d < ImageDimension; d++)
    {
      m_InputsContinuousIndices[i][d] = ci[d] + input->GetLargestPossibleRegion().GetIndex(d);
    }

    ImageIndexType ind;
    outputImage->TransformPhysicalPointToIndex(iOrigin, ind);
    RegionType reg = input->GetLargestPossibleRegion();
    for (unsigned d = 0; d < ImageDimension; d++)
    {
      reg.SetIndex(d, reg.GetIndex(d) + ind[d]);
    }
    m_InputMappings[i] = reg;
  }

  // now we split the totalRegion into pieces which have contributions
  // by the same input tiles
  m_Regions.push_back(totalRegion);
  m_RegionContributors.emplace_back(); // we start with an empty set
  for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
  {
    // first determine the region indices which the newRegion overlaps
    std::vector<size_t> roIndices;
    for (unsigned r = 0; r < m_Regions.size(); r++)
    {
      if (m_InputMappings[i].IsInside(m_Regions[r]))
      {
        m_RegionContributors[r].insert(i);
      }
      else
      {
        RegionType testR = m_InputMappings[i];
        if (testR.Crop(m_Regions[r]))
        {
          roIndices.push_back(r);
        }
      }
    }
    for (auto & roIndex : roIndices)
    {
      this->SplitRegionAndCopyContributions(m_Regions, m_RegionContributors, m_InputMappings[i], roIndex, i);
    }
  }
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::GenerateData()
{
  ImagePointer outputImage = this->GetOutput();
  RegionType   reqR = outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion(reqR);
  outputImage->Allocate(false);

  // for debugging purposes, just color the regions by their contributing tiles
  // to make sure that the regions have been generated correctly without cracks
  if (this->GetDebug())
  {
    this->UpdateProgress(0.0);
    for (unsigned i = 0; i < m_Regions.size(); i++)
    {
      PixelType val = NumericTraits<PixelType>::ZeroValue();
      PixelType val1 = NumericTraits<PixelType>::OneValue();
      unsigned  bits = sizeof(typename NumericTraits<PixelType>::ValueType) * 8;
      if (m_RegionContributors[i].empty())
      {
        val = NumericTraits<PixelType>::max();
      }
      for (auto tile : m_RegionContributors[i])
      {
        val += val1 * std::pow(2, tile % bits);
      }
      RegionType currentRegion = m_Regions[i];
      if (currentRegion.Crop(reqR)) // intersection is not empty
      {
        ImageRegionIterator<ImageType> oIt(outputImage, currentRegion);
        while (!oIt.IsAtEnd())
        {
          oIt.Set(val);
          ++oIt;
        }
      }
      this->UpdateProgress((i + 1) / float(m_Regions.size()));
    }
    return;
  }

  // now we will do resampling, one region at a time (in parallel)
  // within each of these regions the set of contributing tiles is the same
  MultiThreaderBase::Pointer                   mt = MultiThreaderBase::New();
  MultiThreaderBase::ArrayThreadingFunctorType tf = std::bind(&Self::ResampleSingleRegion, this, std::placeholders::_1);
  mt->ParallelizeArray(0, m_Regions.size(), tf, this);

  // release data from input tiles
  RegionType reg0;
  for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
  {
    if (m_Tiles[i])
    {
      m_Tiles[i]->SetBufferedRegion(reg0);
      m_Tiles[i]->Allocate(false);
    }
  }
}

template <typename TImageType, typename TPixelAccumulateType, typename TInterpolator>
void
TileMergeImageFilter<TImageType, TPixelAccumulateType, TInterpolator>::ResampleSingleRegion(SizeValueType i)
{
  ImagePointer outputImage = this->GetOutput();
  RegionType   reg0; // empty region
  RegionType   reqR = outputImage->GetRequestedRegion();
  RegionType   currentRegion = m_Regions[i];
  if (!currentRegion.Crop(reqR)) // empty intersection
  {
    return; // nothing to do
  }
  ImageRegionIteratorWithIndex<ImageType> oIt(outputImage, currentRegion);
  if (m_RegionContributors[i].empty()) // not covered by any tile
  {
    while (!oIt.IsAtEnd())
    {
      oIt.Set(m_Background);
      ++oIt;
    }
    return;
  }

  using ContinuousValueType = typename ContinuousIndexType::ValueType;
  std::vector<SizeValueType>     tileIndices(m_RegionContributors[i].begin(), m_RegionContributors[i].end());
  const unsigned                 nTiles = tileIndices.size();
  std::vector<ImageConstPointer> inputs(nTiles);
  std::vector<RegionType *>      tileRegions(nTiles);
  std::vector<RegionType>        inRegions(nTiles);
  std::vector<Vector<ContinuousValueType, ImageDimension>> continuousIndexDifferences(nTiles);
  for (unsigned t = 0; t < nTiles; t++)
  {
    TileIndexType nDIndex = this->LinearIndexTonDIndex(tileIndices[t]);
    OffsetType    tileToRegion = currentRegion.GetIndex() - m_InputMappings[tileIndices[t]].GetIndex();
    inRegions[t] = currentRegion;
    ImageConstPointer input = this->GetImage(nDIndex, reg0); // matadata (at least)
    inRegions[t].SetIndex(input->GetLargestPossibleRegion().GetIndex() + tileToRegion);
    inputs[t] = this->GetImage(nDIndex, inRegions[t]); // metadata + at least inRegions[t] of data
    tileRegions[t] = &m_InputMappings[tileIndices[t]];
    for (unsigned d = 0; d < ImageDimension; d++)
    {
      continuousIndexDifferences[t][d] =
        inputs[t]->GetLargestPossibleRegion().GetIndex(d) - m_InputsContinuousIndices[tileIndices[t]][d];
    }
  }

  ContinuousValueType             eps = 1e-4;
  typename ImageType::SpacingType spacing = outputImage->GetSpacing();
  bool                            interpolate = false;
  if (m_Montage.IsNotNull()) // we can check whether interpolation was used
  {
    const auto InterpolationNone = Superclass::PCMOptimizerType::PeakInterpolationMethodEnum::None;
    interpolate = (m_Montage->GetPeakInterpolationMethod() != InterpolationNone);
  }
  else // examine alignment of image grids of all the contributing regions
  {
    const typename ImageType::PointType oOrigin = outputImage->GetOrigin();
    for (unsigned t = 0; t < nTiles; t++)
    {
      const typename ImageType::PointType iOrigin = inputs[t]->GetOrigin();
      for (unsigned d = 0; d < ImageDimension; d++)
      {
        ContinuousValueType translation = (oOrigin[d] - iOrigin[d]) / spacing[d] + continuousIndexDifferences[t][d];
        ContinuousValueType absDiff = std::abs(translation - std::round(translation));
        if (absDiff > eps)
        {
          interpolate = true;
          break;
        }
      }
    }
  }

  if (nTiles == 1) // blending not needed
  {
    if (!interpolate)
    {
      ImageAlgorithm::Copy(inputs[0].GetPointer(), outputImage.GetPointer(), inRegions[0], currentRegion);
    }
    else
    {
      typename TInterpolator::Pointer interp = TInterpolator::New();
      interp->SetInputImage(inputs[0]);
      while (!oIt.IsAtEnd())
      {
        ContinuousIndexType continuousIndex = oIt.GetIndex();
        continuousIndex += continuousIndexDifferences[0];
        oIt.Set(interp->EvaluateAtContinuousIndex(continuousIndex));
        ++oIt;
      }
    }
  }
  else // more than one tile contributes
  {
    const TPixelAccumulateType zeroSum = NumericTraits<TPixelAccumulateType>::ZeroValue();
    if (!interpolate)
    {
      std::vector<ImageRegionConstIterator<ImageType>> iIt(nTiles);
      for (unsigned t = 0; t < nTiles; t++)
      {
        iIt[t] = ImageRegionConstIterator<ImageType>(inputs[t], inRegions[t]);
      }

      while (!oIt.IsAtEnd())
      {
        ImageIndexType       pixelIndex = oIt.GetIndex();
        SizeValueType        dist = 0;
        TPixelAccumulateType sum = zeroSum;
        for (unsigned t = 0; t < nTiles; t++)
        {
          SizeValueType dt = this->DistanceFromEdge(pixelIndex, *tileRegions[t]);
          sum += TPixelAccumulateType(iIt[t].Get()) * dt;
          dist += dt;
          ++iIt[t];
        }
        sum /= dist;
        oIt.Set(sum);
        ++oIt;
      }
    }
    else
    {
      std::vector<typename TInterpolator::Pointer> iInt(nTiles);
      for (unsigned t = 0; t < nTiles; t++)
      {
        for (unsigned d = 0; d < ImageDimension; d++)
        {
          continuousIndexDifferences[t][d] =
            inputs[t]->GetLargestPossibleRegion().GetIndex(d) - m_InputsContinuousIndices[tileIndices[t]][d];
        }
        iInt[t] = TInterpolator::New();
        iInt[t]->SetInputImage(inputs[t]);
      }

      while (!oIt.IsAtEnd())
      {
        ImageIndexType       pixelIndex = oIt.GetIndex();
        SizeValueType        dist = 0;
        TPixelAccumulateType sum = zeroSum;
        for (unsigned t = 0; t < nTiles; t++)
        {
          SizeValueType       dt = this->DistanceFromEdge(pixelIndex, *tileRegions[t]);
          ContinuousIndexType continuousIndex = pixelIndex;
          continuousIndex += continuousIndexDifferences[t];
          sum += TPixelAccumulateType(iInt[t]->EvaluateAtContinuousIndex(continuousIndex)) * dt;
          dist += dt;
        }
        sum /= dist;
        oIt.Set(sum);
        ++oIt;
      }
    }
  }
}

} // namespace itk

#endif // itkTileMergeImageFilter_hxx
