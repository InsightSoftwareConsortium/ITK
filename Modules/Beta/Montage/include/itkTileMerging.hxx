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

#ifndef itkTileMerging_hxx
#define itkTileMerging_hxx

#include "itkTileMerging.h"
#include "itkNumericTraits.h"
#include "itkMultiThreaderBase.h"
#include <algorithm>
#include <cassert>

namespace itk
{
template <typename TImageType, typename TInterpolator>
TileMerging<TImageType, TInterpolator>
::TileMerging()
{
  m_Background = PixelType();
  m_CropToFill = false;
  this->SetMontageSize(m_MontageSize); //initialize the rest of arrays

  //required for GenerateOutputInformation to be called
  this->SetNthOutput(0, this->MakeOutput(0).GetPointer());
}

template <typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "CropToFill: " << (m_CropToFill ? "Yes" : "No") << std::endl;
  os << indent << "Background: " << m_Background << std::endl;
  os << indent << "RegionsSize: " << m_Regions.size() << std::endl;

  auto nullCount = std::count(m_Transforms.begin(), m_Transforms.end(), nullptr);
  os << indent << "Transforms (filled/capcity): " << m_Transforms.size() - nullCount
      << "/" << m_Transforms.size() << std::endl;

  os << indent << "Montage: " << m_Montage.GetPointer() << std::endl;
}

template <typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::SetMontage(const Superclass* montage)
{
  if (m_Montage != montage)
    {
    m_Montage = montage;
    this->m_MontageSize = montage->m_MontageSize;
    this->m_LinearMontageSize = montage->m_LinearMontageSize;
    this->m_FinishedTiles = montage->m_FinishedTiles;
    this->m_OriginAdjustment = montage->m_OriginAdjustment;
    this->m_ForcedSpacing = montage->m_ForcedSpacing;

    this->m_Filenames = montage->m_Filenames;
    for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
      {
      this->SetNthInput(i, const_cast<DataObject *>(montage->GetInput(i)));
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

template <typename TImageType, typename TInterpolator>
TImageType *
TileMerging<TImageType, TInterpolator>
::GetOutput()
{
  return itkDynamicCastInDebugMode< ImageType * >( this->GetPrimaryOutput() );
}

template <typename TImageType, typename TInterpolator>
const TImageType *
TileMerging<TImageType, TInterpolator>
::GetOutput() const
{
  return itkDynamicCastInDebugMode< const ImageType * >( this->GetPrimaryOutput() );
}

template <typename TImageType, typename TInterpolator>
TImageType *
TileMerging<TImageType, TInterpolator>
::GetOutput(unsigned int idx)
{
  auto * out = dynamic_cast< ImageType * > ( this->ProcessObject::GetOutput(idx) );

  if ( out == nullptr && this->ProcessObject::GetOutput(idx) != nullptr )
    {
    itkWarningMacro (<< "Unable to convert output number " << idx
        << " to type " <<  typeid( ImageType ).name () );
    }
  return out;
}

template<typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::SetMontageSize(SizeType montageSize)
{
  Superclass::SetMontageSize(montageSize);
  m_Transforms.resize(this->m_LinearMontageSize);
  m_Metadata.resize(this->m_LinearMontageSize);
  this->SetNumberOfRequiredOutputs(1);
}

template<typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::SetTileTransform(TileIndexType position, TransformConstPointer transform)
{
  SizeValueType linInd = this->nDIndexToLinearIndex(position);
  if (m_Transforms[linInd].IsNull() || m_Transforms[linInd]->GetParameters() != transform->GetParameters()
      || m_Transforms[linInd]->GetFixedParameters() != transform->GetFixedParameters())
    {
    m_Transforms[linInd] = transform;
    this->Modified();
    }
}

template <typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::SplitRegionAndCopyContributions(
    std::vector<RegionType>& regions,
    std::vector<ContributingTiles>& regionContributors,
    RegionType newRegion,
    size_t ori, //oldRegionIndex
    SizeValueType tileIndex)
{
  for (int d = ImageDimension - 1; d >= 0; d--)
    {
    SizeValueType nrSize = newRegion.GetSize(d);
    IndexValueType nrInd = newRegion.GetIndex(d);
    IndexValueType endNR = nrInd + IndexValueType(nrSize);

    SizeValueType orSize = regions[ori].GetSize(d);
    IndexValueType orInd = regions[ori].GetIndex(d);
    IndexValueType endOR = orInd + IndexValueType(orSize);

    if (nrInd < endOR && orInd < nrInd)
      {
      RegionType remnant = regions[ori];
      remnant.SetSize(d, nrInd - orInd);
      regions.push_back(remnant);
      regionContributors.push_back(regionContributors[ori]);

      regions[ori].SetSize(d, orSize - (nrInd - orInd));
      regions[ori].SetIndex(d, nrInd);

      //update OR size and index
      orSize = regions[ori].GetSize(d);
      orInd = regions[ori].GetIndex(d);
      assert(endOR == orInd + IndexValueType(orSize));
      }

    if (orInd < endNR && endNR < endOR)
      {
      RegionType remnant = regions[ori];
      regions[ori].SetSize(d, endNR - orInd);

      remnant.SetSize(d, orSize - (endNR - orInd));
      remnant.SetIndex(d, endNR);
      regions.push_back(remnant);
      regionContributors.push_back(regionContributors[ori]);
      }
    }
  regionContributors[ori].insert(tileIndex);
}

template <typename TImageType, typename TInterpolator>
typename TileMerging<TImageType, TInterpolator>::RegionType
TileMerging<TImageType, TInterpolator>
::ConstructRegion(ContinuousIndexType minIndex, ContinuousIndexType maxIndex)
{
  ImageIndexType ind;
  SizeType size;
  for (unsigned d = 0; d < ImageDimension; d++)
    {
    ind[d] = std::ceil(minIndex[d]);
    size[d] = maxIndex[d] - ind[d];
    }
  RegionType region;
  region.SetIndex(ind);
  region.SetSize(size);
  return region;
}

template <typename TImageType, typename TInterpolator>
SizeValueType
TileMerging<TImageType, TInterpolator>
::DistanceFromEdge(ImageIndexType index, RegionType region)
{
  SizeValueType dist = NumericTraits<SizeValueType>::max();
  for (unsigned d = 0; d < ImageDimension; d++)
    {
    SizeValueType dimDist= std::min<IndexValueType>(index[d] - region.GetIndex(d),
        region.GetIndex(d) + region.GetSize(d) - index[d]);
    dist = std::min(dist, dimDist);
    }
  return 1 + dist;
}


template <typename TImageType, typename TInterpolator>
typename TImageType::ConstPointer
TileMerging<TImageType, TInterpolator>
::GetImage(TileIndexType nDIndex, RegionType wantedRegion)
{
  DataObjectPointerArraySizeType linearIndex = nDIndexToLinearIndex(nDIndex);
  const auto cInput = static_cast<ImageType *>(this->GetInput(linearIndex));
  ImagePointer input = const_cast<ImageType *>(cInput);
  if (input.GetPointer() == m_Dummy.GetPointer()) //filename given, we have to read
    {
    if (wantedRegion.GetNumberOfPixels()==0) //only metadata required
      {
      m_Reader->SetFileName(m_Filenames[linearIndex]);
      m_Reader->UpdateOutputInformation();
      m_Metadata[linearIndex] = m_Reader->GetOutput();
      m_Metadata[linearIndex]->DisconnectPipeline();
      input = m_Metadata[linearIndex];
      }
    else
      {
      //read the wantedRegion
      typename ReaderType::Pointer reader= ReaderType::New();
      reader->SetFileName(m_Filenames[linearIndex]);
      reader->GetOutput()->SetRequestedRegion(wantedRegion);
      reader->Update();
      input = reader->GetOutput();
      input->DisconnectPipeline();
      }
    }

  PointType origin = input->GetOrigin();
  for (unsigned d = 0; d < ImageDimension; d++)
    {
    origin[d] += m_OriginAdjustment[d] * nDIndex[d];
    }
  input->SetOrigin(origin);
  if (m_ForcedSpacing[0] != 0)
    {
    input->SetSpacing(m_ForcedSpacing);
    }

  return input;
}

template <typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  TileIndexType nDIndex0 = { 0 };
  RegionType reg0;
  ImageConstPointer input0 = this->GetImage(nDIndex0, reg0);

  if (m_Montage.IsNull())
    {
    //initialize mosaic bounds
    m_MinInner.Fill(NumericTraits<typename TInterpolator::CoordRepType>::NonpositiveMin());
    m_MinOuter.Fill(NumericTraits<typename TInterpolator::CoordRepType>::max());
    m_MaxOuter.Fill(NumericTraits<typename TInterpolator::CoordRepType>::NonpositiveMin());
    m_MaxInner.Fill(NumericTraits<typename TInterpolator::CoordRepType>::max());
    
    for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
      {
      TileIndexType nDIndex = this->LinearIndexTonDIndex(i);
      ImageConstPointer input = this->GetImage(nDIndex, reg0);
      this->UpdateMosaicBounds(nDIndex, m_Transforms[i], input, input0);
      }
    }

  //clean up internal variables
  m_InputMappings.clear();
  m_InputsContinuousIndices.clear();
  m_Regions.clear();
  m_RegionContributors.clear();

  ImagePointer outputImage = this->GetOutput();
  outputImage->CopyInformation(input0); //origin, spacing, direction

  //determine output region
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

  //determine where does each input tile map into the output image
  m_InputMappings.resize(this->m_LinearMontageSize);
  m_InputsContinuousIndices.resize(this->m_LinearMontageSize);
  for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
    {
    TransformPointer inverseT = TransformType::New();
    m_Transforms[i]->GetInverse(inverseT);
    PointType iOrigin = m_Metadata[i]->GetOrigin();
    iOrigin = inverseT->TransformPoint(iOrigin);

    ContinuousIndexType ci;
    outputImage->TransformPhysicalPointToContinuousIndex(iOrigin, ci);
    m_InputsContinuousIndices[i] = ci;

    ImageIndexType ind;
    outputImage->TransformPhysicalPointToIndex(iOrigin, ind);
    RegionType reg = m_Metadata[i]->GetLargestPossibleRegion();
    reg.SetIndex(ind);
    m_InputMappings[i] = reg;
    }

  //now we split the totalRegion into pieces which have contributions
  //by the same input tiles
  m_Regions.push_back(totalRegion);
  m_RegionContributors.push_back({}); //we start with an empty set
  for (SizeValueType i = 0; i < this->m_LinearMontageSize; i++)
    {
    //first determine the region indices which the newRegion overlaps
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
    for (unsigned r = 0; r < roIndices.size(); r++)
      {
      this->SplitRegionAndCopyContributions(m_Regions, m_RegionContributors, m_InputMappings[i], roIndices[r], i);
      }
    }
}

template <typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::GenerateData()
{
  ImagePointer outputImage = this->GetOutput();
  RegionType reqR = outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion(reqR);
  outputImage->Allocate(false);

  ////for debugging purposes, just color the regions by their contributing tiles
  ////to make sure that the regions have been generated correctly without cracks
  //for (unsigned i = 0; i < m_Regions.size(); i++)
  //  {
  //  PixelType val = 0;
  //  PixelType bits = sizeof(PixelType) * 8;
  //  if (m_RegionContributors[i].empty())
  //    {
  //    val = NumericTraits<PixelType>::max();
  //    }
  //  for (auto tile : m_RegionContributors[i])
  //    {
  //    val += std::pow(2, tile%bits);
  //    }
  //  RegionType currentRegion = m_Regions[i];
  //  if (currentRegion.Crop(reqR)) //intersection is not empty
  //    {
  //    ImageRegionIterator<ImageType> oIt(outputImage, currentRegion);
  //    while (!oIt.IsAtEnd())
  //      {
  //      oIt.Set(val);
  //      ++oIt;
  //      }
  //    }
  //  }
  //return;

  //now we will do resampling, one region at a time (in parallel)
  //within each of these regions the set of contributing tiles is the same
  using Region1D = ImageRegion<1>;
  Region1D linReg; //index is 0 by default
  linReg.SetSize(0, m_Regions.size());
  MultiThreaderBase::Pointer mt = MultiThreaderBase::New();
  mt->ParallelizeImageRegion<1>(linReg,
      [this](const Region1D& r)
      {
        //we can also get a chunk instead od just a single "pixel", so loop
        for (int i = r.GetIndex(0); i < r.GetIndex(0) + r.GetSize(0); i++)
          {
          this->ResampleSingleRegion(i);
          }
      },
      nullptr);
}

template <typename TImageType, typename TInterpolator>
void
TileMerging<TImageType, TInterpolator>
::ResampleSingleRegion(unsigned i)
{
  ImagePointer outputImage = this->GetOutput();
  RegionType reqR = outputImage->GetRequestedRegion();
  RegionType currentRegion = m_Regions[i];
  if (!currentRegion.Crop(reqR)) //empty intersection
    {
    return; //nothing to do
    }
  bool interpolate = true;
  if (this->m_PCMOptimizer->GetPeakInterpolationMethod() ==
        Superclass::PCMOptimizerType::PeakInterpolationMethod::None)
    {
    interpolate = false;
    }
  ImageRegionIteratorWithIndex<ImageType> oIt(outputImage, currentRegion);

  if (m_RegionContributors[i].empty()) //not covered by any tile
    {
    while (!oIt.IsAtEnd())
      {
      oIt.Set(m_Background);
      ++oIt;
      }
    }
  else if (m_RegionContributors[i].size() == 1) //just one tile
    {
    SizeValueType tileIndex = *m_RegionContributors[i].begin();
    TileIndexType nDIndex = this->LinearIndexTonDIndex(tileIndex);
    OffsetType tileToRegion = m_Regions[i].GetIndex() - m_InputMappings[tileIndex].GetIndex();
    RegionType iReg = currentRegion;
    iReg.SetIndex(m_Metadata[tileIndex]->GetLargestPossibleRegion().GetIndex() + tileToRegion);
    ImageConstPointer input = this->GetImage(nDIndex, iReg);

    if (!interpolate)
      {
      ImageAlgorithm::Copy(input.GetPointer(), outputImage.GetPointer(), iReg, currentRegion);
      }
    else
      {
      Vector<typename ContinuousIndexType::ValueType, ImageDimension> continuousIndexDifference;
      for (unsigned d = 0; d < ImageDimension; d++)
        {
        continuousIndexDifference[d] = input->GetLargestPossibleRegion().GetIndex(d)
            - m_InputsContinuousIndices[tileIndex][d];
        }
      typename TInterpolator::Pointer interp = TInterpolator::New();
      interp->SetInputImage(input);
      while (!oIt.IsAtEnd())
        {
        ContinuousIndexType continuousIndex = oIt.GetIndex();
        continuousIndex += continuousIndexDifference;
        oIt.Set(interp->EvaluateAtContinuousIndex(continuousIndex));
        ++oIt;
        }
      }
    }
  else //more than one tile contributes
    {
    std::vector<SizeValueType> tileIndices(m_RegionContributors[i].begin(), m_RegionContributors[i].end());
    unsigned nTiles = tileIndices.size();
    std::vector<ImageConstPointer> inputs(nTiles);
    std::vector<RegionType*> tileRegions(nTiles);
    std::vector<RegionType> inRegions(nTiles);
    for (unsigned t = 0; t < nTiles; t++)
      {
      TileIndexType nDIndex = this->LinearIndexTonDIndex(tileIndices[t]);
      OffsetType tileToRegion = m_Regions[i].GetIndex() - m_InputMappings[tileIndices[t]].GetIndex();
      inRegions[t] = currentRegion;
      inRegions[t].SetIndex(m_Metadata[tileIndices[t]]->GetLargestPossibleRegion().GetIndex() + tileToRegion);
      inputs[t] = this->GetImage(nDIndex, inRegions[t]);
      tileRegions[t] = &m_InputMappings[tileIndices[t]];
      }

    if (!interpolate)
      {
      std::vector<ImageRegionConstIterator<ImageType> > iIt(nTiles);
      for (unsigned t = 0; t < nTiles; t++)
        {
        iIt[t] = ImageRegionConstIterator<ImageType>(inputs[t], inRegions[t]);
        }

      while (!oIt.IsAtEnd())
        {
        ImageIndexType pixelIndex = oIt.GetIndex();
        SizeValueType dist = 0;
        double sum = 0.0;
        for (unsigned t = 0; t < nTiles; t++)
          {
          SizeValueType dt = this->DistanceFromEdge(pixelIndex, *tileRegions[t]);
          sum += iIt[t].Get()*dt;
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
      std::vector<Vector<typename ContinuousIndexType::ValueType, ImageDimension> > continuousIndexDifferences(nTiles);
      for (unsigned t = 0; t < nTiles; t++)
        {
        for (unsigned d = 0; d < ImageDimension; d++)
          {
          continuousIndexDifferences[t][d] = inputs[t]->GetLargestPossibleRegion().GetIndex(d)
              - m_InputsContinuousIndices[tileIndices[t]][d];
          }
        iInt[t] = TInterpolator::New();
        iInt[t]->SetInputImage(inputs[t]);
        }

      while (!oIt.IsAtEnd())
        {
        ImageIndexType pixelIndex = oIt.GetIndex();
        SizeValueType dist = 0;
        double sum = 0.0;
        for (unsigned t = 0; t < nTiles; t++)
          {
          SizeValueType dt = this->DistanceFromEdge(pixelIndex, *tileRegions[t]);
          ContinuousIndexType continuousIndex = pixelIndex;
          continuousIndex += continuousIndexDifferences[t];
          sum += iInt[t]->EvaluateAtContinuousIndex(continuousIndex)*dt;
          dist += dt;
          }
        sum /= dist;
        oIt.Set(sum);
        ++oIt;
        }
      }
    }
}

} //namespace itk

#endif //itkTileMerging_hxx
