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

#ifndef itkTileMontage_hxx
#define itkTileMontage_hxx


#include "itkMultiThreaderBase.h"
#include "itkNumericTraits.h"
#include "itkThreadPool.h"
#include "itkConfigure.h" // for ITK_USE_FFTWF and ITK_USE_FFTWD

#include "itk_eigen.h"
#include ITK_EIGEN(Sparse)

#include <algorithm>
#include <cassert>
#include <iomanip>

namespace itk
{
template <typename TImageType, typename TCoordinate>
TileMontage<TImageType, TCoordinate>::TileMontage()
{
  this->ThreaderUpdateProgressOff(); // we will update the progress from the filter
  m_OriginAdjustment.Fill(0);
  m_ForcedSpacing.Fill(0);

  // make default padding sufficient for exponential decay to zero
  m_ObligatoryPadding.Fill(0);
  SizeType pad;
  pad.Fill(8 * sizeof(typename TImageType::PixelType));
  this->SetObligatoryPadding(pad);

  SizeType initialSize;
  initialSize.Fill(1);
  initialSize[0] = 2;
  this->SetMontageSize(initialSize);

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD) // FFTW, MKL and cuFFT are already parallel
  // we want light parallelism, just to overlap IO with computation
  this->SetNumberOfWorkUnits(std::max(2u, MultiThreaderBase::GetGlobalDefaultNumberOfThreads() / 16));
#else // we are dealing with VNL's single threaded implementation
  this->SetNumberOfWorkUnits(MultiThreaderBase::GetGlobalDefaultNumberOfThreads()); // we want full parallelism
#endif

  // required for GenerateOutputInformation to be called
  this->SetNthOutput(0, this->MakeOutput(0).GetPointer());
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::PrintSelf(std::ostream & os, Indent indent) const
{
  if (this->GetDebug())
  {
    Superclass::PrintSelf(os, indent); // this can be overwhelming
  }
  os << indent << "Montage size: " << m_MontageSize << std::endl;
  os << indent << "Linear Montage size: " << m_LinearMontageSize << std::endl;
  os << indent << "Finished Pairs: " << m_FinishedPairs << std::endl;
  os << indent << "Origin Adjustment: " << m_OriginAdjustment << std::endl;
  os << indent << "Forced Spacing: " << m_ForcedSpacing << std::endl;
  os << indent << "Obligatory Padding: " << m_ObligatoryPadding << std::endl;
  os << indent << "Absolute Threshold: " << m_AbsoluteThreshold << std::endl;
  os << indent << "Relative Threshold: " << m_RelativeThreshold << std::endl;
  os << indent << "Position Tolerance: " << m_PositionTolerance << std::endl;

  auto nullCount = std::count(m_Filenames.begin(), m_Filenames.end(), std::string());
  os << indent << "Filenames (filled/capacity): " << m_Filenames.size() - nullCount << "/" << m_Filenames.size()
     << std::endl;
  nullCount = std::count(m_FFTCache.begin(), m_FFTCache.end(), nullptr);
  os << indent << "FFTCache (filled/capacity): " << m_FFTCache.size() - nullCount << "/" << m_FFTCache.size()
     << std::endl;

  os << indent << "MinInner: " << m_MinInner << std::endl;
  os << indent << "MaxInner: " << m_MaxInner << std::endl;
  os << indent << "MinOuter: " << m_MinOuter << std::endl;
  os << indent << "MaxOuter: " << m_MaxOuter << std::endl;
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::SetMontageSize(SizeType montageSize)
{
  if (m_MontageSize != montageSize)
  {
    m_LinearMontageSize = 1u;
    for (unsigned d = 0; d < ImageDimension; d++)
    {
      m_LinearMontageSize *= montageSize[d];
    }
    this->SetNumberOfRequiredInputs(m_LinearMontageSize);
    this->SetNumberOfRequiredOutputs(m_LinearMontageSize);
    m_MontageSize = montageSize;
    m_TileReadLocks.resize(m_LinearMontageSize);
    m_Filenames.resize(m_LinearMontageSize);
    m_FFTCache.resize(m_LinearMontageSize);
    m_Tiles.resize(m_LinearMontageSize);
    m_CurrentAdjustments.resize(m_LinearMontageSize);
    m_TransformCandidates.resize(ImageDimension * m_LinearMontageSize); // adjacency along each dimension
    m_CandidateConfidences.resize(ImageDimension * m_LinearMontageSize);
    this->Modified();
  }
}

template <typename TImageType, typename TCoordinate>
template <typename TImageToRead>
typename TImageToRead::Pointer
TileMontage<TImageType, TCoordinate>::GetImageHelper(TileIndexType nDIndex, bool metadataOnly, RegionType region)
{
  DataObjectPointerArraySizeType linearIndex = nDIndexToLinearIndex(nDIndex);
  const auto                     cInput = static_cast<TImageToRead *>(this->GetInput(linearIndex));
  typename TImageToRead::Pointer input = const_cast<TImageToRead *>(cInput);
  typename TImageToRead::Pointer result = nullptr;
  if (input.GetPointer() != reinterpret_cast<TImageToRead *>(this->m_Dummy.GetPointer()))
  {
    // construct new metadata so adjustments do not modify the original input
    result = TImageToRead::New();
    result->SetRegions(input->GetBufferedRegion());
    result->SetOrigin(input->GetOrigin());
    result->SetSpacing(input->GetSpacing());
    result->SetDirection(input->GetDirection());
    result->SetPixelContainer(input->GetPixelContainer());
  }
  else // examine cache and read from file if necessary
  {
    using ImageReaderType = ImageFileReader<TImageToRead>;
    typename ImageReaderType::Pointer iReader = ImageReaderType::New();
    iReader->SetFileName(this->m_Filenames[linearIndex]);
    iReader->UpdateOutputInformation();
    result = iReader->GetOutput();

    if (!metadataOnly)
    {
      RegionType regionToRead = result->GetLargestPossibleRegion();
      if (region.GetNumberOfPixels() > 0)
      {
        regionToRead.Crop(region);
        result->SetRequestedRegion(regionToRead);
      }
      iReader->Update();
    }
    result->DisconnectPipeline();
  }

  // adjust origin and spacing
  PointType origin = result->GetOrigin();
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    origin[d] += this->m_OriginAdjustment[d] * nDIndex[d];
  }
  result->SetOrigin(origin);
  if (this->m_ForcedSpacing[0] != 0)
  {
    result->SetSpacing(this->m_ForcedSpacing);
  }

  return result;
}

template <typename TImageType, typename TCoordinate>
typename TileMontage<TImageType, TCoordinate>::ImageType::Pointer
TileMontage<TImageType, TCoordinate>::GetImage(TileIndexType nDIndex, bool metadataOnly)
{
  RegionType                  reg0; // default-initialized to zeroes
  SizeValueType               linearIndex = this->nDIndexToLinearIndex(nDIndex);
  std::lock_guard<std::mutex> lockGuard(m_TileReadLocks[linearIndex]);
  // if we are not cropping to overlap, FFTCache will kick in later
  // and we don't want to double-cache the input tiles
  if (!m_CropToOverlap && m_Tiles[linearIndex].IsNotNull())
  {
    RegionType r = m_Tiles[linearIndex]->GetBufferedRegion();
    if (metadataOnly || r.GetNumberOfPixels() > 0)
    {
      return m_Tiles[linearIndex];
    }
  }

  return GetImageHelper<ImageType>(nDIndex, metadataOnly, reg0);
}

template <typename TImageType, typename TCoordinate>
DataObject::DataObjectPointerArraySizeType
TileMontage<TImageType, TCoordinate>::nDIndexToLinearIndex(TileIndexType nDIndex) const
{
  DataObjectPointerArraySizeType ind = 0;
  SizeValueType                  stride = 1u;
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    itkAssertOrThrowMacro(nDIndex[d] < m_MontageSize[d],
                          "Tile index " << nDIndex << " exceeds tile size " << m_MontageSize << " at dimension " << d);
    ind += nDIndex[d] * stride;
    stride *= m_MontageSize[d];
  }
  return ind;
}

template <typename TImageType, typename TCoordinate>
typename TileMontage<TImageType, TCoordinate>::TileIndexType
TileMontage<TImageType, TCoordinate>::LinearIndexTonDIndex(DataObject::DataObjectPointerArraySizeType linearIndex) const
{
  TileIndexType ind;
  SizeValueType stride = 1u;
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    stride *= m_MontageSize[d];
    ind[d] = linearIndex % m_MontageSize[d];
    linearIndex /= m_MontageSize[d];
  }
  itkAssertOrThrowMacro(linearIndex < stride,
                        "Linear tile index " << linearIndex << " exceeds total montage size " << stride);
  return ind;
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::RegisterPair(TileIndexType fixed, TileIndexType moving)
{
  SizeValueType lFixedInd = nDIndexToLinearIndex(fixed);
  SizeValueType lMovingInd = nDIndexToLinearIndex(moving);

  typename PCMType::Pointer          m_PCM = PCMType::New();
  typename PCMOperatorType::Pointer  m_PCMOperator = PCMOperatorType::New();
  typename PCMOptimizerType::Pointer m_PCMOptimizer = PCMOptimizerType::New();
  m_PCM->SetPaddingMethod(m_PaddingMethod);
  m_PCM->SetCropToOverlap(m_CropToOverlap);
  m_PCM->SetOperator(m_PCMOperator);
  m_PCM->SetOptimizer(m_PCMOptimizer);
  m_PCM->SetObligatoryPadding(m_ObligatoryPadding);
  m_PCM->SetReleaseDataFlag(this->GetReleaseDataFlag());
  m_PCM->SetReleaseDataBeforeUpdateFlag(this->GetReleaseDataBeforeUpdateFlag());
  m_PCMOptimizer->SetPixelDistanceTolerance(m_PositionTolerance);
  m_PCMOptimizer->SetPeakInterpolationMethod(m_PeakInterpolationMethod);

  auto mImage = this->GetImage(moving, false);
  m_PCM->SetFixedImage(this->GetImage(fixed, false));
  m_PCM->SetMovingImage(mImage);
  // scoping the lock
  {
    std::lock_guard<std::mutex> lock(m_MemberProtector);
    m_PCM->SetFixedImageFFT(m_FFTCache[lFixedInd]);   // maybe null
    m_PCM->SetMovingImageFFT(m_FFTCache[lMovingInd]); // maybe null
  }
  // m_PCM->DebugOn();
  m_PCM->Update();

  if (!m_CropToOverlap)
  {
    std::lock_guard<std::mutex> lock(m_MemberProtector);
    m_FFTCache[lFixedInd] = m_PCM->GetFixedImageFFT();   // certainly not null
    m_FFTCache[lMovingInd] = m_PCM->GetMovingImageFFT(); // certrainly not null
  }

  const typename PCMType::OffsetVector & offsets = m_PCM->GetOffsets();
  SizeValueType                          regLinearIndex = lMovingInd;
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    if (fixed[d] != moving[d]) // this is the different dimension
    {
      regLinearIndex += d * m_LinearMontageSize;
      break;
    }
  }

  m_CandidateConfidences[regLinearIndex] = m_PCM->GetConfidences();
  m_TransformCandidates[regLinearIndex].resize(offsets.size());
  PointType p0;
  p0.Fill(0.0);
  for (unsigned i = 0; i < offsets.size(); i++)
  {
    m_TransformCandidates[regLinearIndex][i] = offsets[i] - p0;
  }
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::ReleaseMemory(TileIndexType finishedTile)
{
  TileIndexType oldIndex;
  bool          releaseTile = true;
  for (unsigned dim = 0; dim < ImageDimension; dim++)
  {
    if (finishedTile[dim] > 0)
    {
      oldIndex[dim] = finishedTile[dim] - 1;
    }
    else
    {
      releaseTile = false;
    }
  }
  if (releaseTile)
  {
    SizeValueType               linearIndex = this->nDIndexToLinearIndex(oldIndex);
    std::lock_guard<std::mutex> lock(m_MemberProtector);
    m_FFTCache[linearIndex] = nullptr;
    if (!m_Filenames[linearIndex].empty()) // release the input image too
    {
      this->SetInputTile(oldIndex, m_Dummy);
    }
    if (m_Tiles[linearIndex])
    {
      RegionType reg0;
      m_Tiles[linearIndex]->SetBufferedRegion(reg0);
      m_Tiles[linearIndex]->Allocate(false);
    }
  }
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::WriteOutTransform(TileIndexType index, TranslationOffset offset)
{
  TransformPointer transform = TransformType::New();
  transform->SetOffset(offset);
  const SizeValueType linearIndex = this->nDIndexToLinearIndex(index);
  auto                dOut = this->GetOutput(linearIndex);
  const auto          cOut = static_cast<TransformOutputType *>(dOut);
  auto                decorator = const_cast<TransformOutputType *>(cOut);
  decorator->Set(transform);
  auto input0 = static_cast<const ImageType *>(this->GetInput(0));
  auto input = static_cast<const ImageType *>(this->GetInput(linearIndex));
  this->UpdateMosaicBounds(index, transform, input, input0);
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::UpdateMosaicBounds(TileIndexType         index,
                                                         TransformConstPointer transform,
                                                         const ImageType *     input,
                                                         const ImageType *     input0)
{
  PointType           p;
  ContinuousIndexType ci;
  ImageIndexType      ind = input->GetLargestPossibleRegion().GetIndex();
  input->TransformIndexToPhysicalPoint(ind, p);
  TransformPointer inverseT = TransformType::New();
  transform->GetInverse(inverseT);
  p = inverseT->TransformPoint(p);
  input0->TransformPhysicalPointToContinuousIndex(p, ci);
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    if (index[d] == 0) // this tile is on the minimum edge
    {
      m_MinInner[d] = std::max(m_MinInner[d], ci[d]);
      m_MinOuter[d] = std::min(m_MinOuter[d], ci[d]);
    }
  }
  ind += input->GetLargestPossibleRegion().GetSize();
  input->TransformIndexToPhysicalPoint(ind, p);
  p = inverseT->TransformPoint(p);
  input0->TransformPhysicalPointToContinuousIndex(p, ci);
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    if (index[d] == m_MontageSize[d] - 1) // this tile is on the maximum edge
    {
      m_MaxOuter[d] = std::max(m_MaxOuter[d], ci[d]);
      m_MaxInner[d] = std::min(m_MaxInner[d], ci[d]);
    }
  }
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();
  for (SizeValueType i = 1; i < m_LinearMontageSize; i++)
  {
    this->SetNthOutput(i, this->MakeOutput(i).GetPointer());
  }
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::OptimizeTiles()
{
  // formulate global optimization as an overdetermined linear system
  constexpr unsigned Dimension = ImageDimension;
  using SparseMatrix = Eigen::SparseMatrix<TCoordinate, Eigen::RowMajor>;
  SparseMatrix regCoef(m_NumberOfPairs + 1, m_LinearMontageSize);
  regCoef.reserve(Eigen::VectorXi::Constant(m_NumberOfPairs + 1, 2)); // 2 non-zeroes per row
  using TranslationsMatrix = Eigen::Matrix<TCoordinate, Eigen::Dynamic, Dimension>;
  TranslationsMatrix         translations(m_NumberOfPairs + 1, Dimension);
  std::vector<SizeValueType> equationToCandidate(m_NumberOfPairs);
  SizeValueType              regIndex = 0;
  double                     confidenceTotal = 0.0;
  for (SizeValueType i = 0; i < m_LinearMontageSize * ImageDimension; i++)
  {
    if (!m_TransformCandidates[i].empty())
    {
      SizeValueType linIndex = i % m_LinearMontageSize;
      TileIndexType currentIndex = this->LinearIndexTonDIndex(linIndex);
      TileIndexType referenceIndex = currentIndex;
      unsigned      dim = i / m_LinearMontageSize;
      referenceIndex[dim] = currentIndex[dim] - 1;
      SizeValueType refLinearIndex = this->nDIndexToLinearIndex(referenceIndex);

      // construct equation: -c*refLinearIndex + c*linIndex = c*candidateOffset, c=confidence
      const float & confidence = m_CandidateConfidences[i][0];
      regCoef.insert(regIndex, refLinearIndex) = -confidence;
      regCoef.insert(regIndex, linIndex) = confidence;
      const TranslationOffset & candidateOffset = m_TransformCandidates[i][0];
      for (unsigned d = 0; d < ImageDimension; d++)
      {
        translations(regIndex, d) = confidence * candidateOffset[d];
      }
      equationToCandidate[regIndex] = i;
      ++regIndex;
      assert(m_CandidateConfidences[i][0] > 0);
      confidenceTotal += m_CandidateConfidences[i][0];
    }
  }
  TCoordinate confidenceAvg = confidenceTotal / m_NumberOfPairs;
  assert(regIndex == m_NumberOfPairs);

  regCoef.insert(regIndex, 0) = confidenceAvg; // tile 0,0...0
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    translations(regIndex, d) = 0; // should have position 0,0...0
  }

  typename ImageType::SpacingType spacing = this->GetImage(this->LinearIndexTonDIndex(0), true)->GetSpacing();
  Eigen::LeastSquaresConjugateGradient<SparseMatrix> solver;
  bool                                               outlierExists = true;
  unsigned                                           iteration = 0;
  while (outlierExists)
  {
    if (this->GetDebug())
    {
      std::cout << "\n\n"; // make it easier to spot new iteration
    }
    std::cout << "\nIteration " << ++iteration << "  ";
    regCoef.makeCompressed();
    solver.compute(regCoef);
    TranslationsMatrix solutions(m_LinearMontageSize, Dimension);
    TranslationsMatrix residuals(m_NumberOfPairs + 1, Dimension);
    solutions = solver.solve(translations);
    residuals = regCoef * solutions - translations;

    if (this->GetDebug())
    {
      std::cout << "\ntranslations:\n" << translations;
      std::cout << std::endl << "current|solution:" << std::endl;
    }
    for (SizeValueType i = 0; i < m_LinearMontageSize; i++)
    {
      TranslationOffset & cOffset = m_CurrentAdjustments[i];
      for (unsigned d = 0; d < ImageDimension; d++)
      {
        if (this->GetDebug())
        {
          std::cout << std::fixed << std::setprecision(2);
          std::cout << " " << std::setw(8) << cOffset[d] << '|' << std::setw(8) << solutions(i, d);
        }

        cOffset[d] = solutions(i, d);
        solutions(i, d) /= spacing[d]; // convert solutions into pixel coordinates
      }
      if (this->GetDebug())
      {
        std::cout << std::endl;
      }
    }

    TranslationsMatrix stdDev0 =
      (translations.cwiseAbs2().colwise().sum() / m_NumberOfPairs).cwiseSqrt(); // assume zero mean
    if (this->GetDebug())
    {
      std::cout << "\nstdDev0:\n" << stdDev0;
    }

    std::vector<TCoordinate> outlierScore(m_NumberOfPairs, 0.0); // sum of squares
    for (SizeValueType i = 0; i < m_NumberOfPairs; i++)
    {
      for (unsigned d = 0; d < ImageDimension; d++)
      {
        TCoordinate trOverDev = translations(i, d) / stdDev0(d);
        outlierScore[i] += itk::Math::abs(trOverDev);
      }
      if (outlierScore[i] > m_RelativeThreshold) // more than this many standard deviations
      {
        outlierScore[i] -= m_RelativeThreshold;
      }
      else
      {
        outlierScore[i] = 0.0; // not an outlier
      }
    }

    TCoordinate   maxCost = 0;
    SizeValueType maxIndex = 0;
    if (this->GetDebug())
    {
      std::cout << "\nresiduals:\n";
    }

    for (SizeValueType i = 0; i < m_NumberOfPairs; i++)
    {
      TCoordinate residual = 0;
      if (this->GetDebug())
      {
        std::cout << 'E' << i << ':';
      }
      for (unsigned d = 0; d < ImageDimension; d++)
      {
        residuals(i, d) /= spacing[d]; // convert residuals into pixel coordinates
        if (this->GetDebug())
        {
          std::cout << ' ' << std::setw(8) << residuals(i, d);
        }
        residual += residuals(i, d) * residuals(i, d);
      }
      residual = std::sqrt(residual); // MSE -> RMSE

      // reduce residual by confidence
      SizeValueType candidateIndex = equationToCandidate[i];
      if (m_CandidateConfidences[candidateIndex].empty())
      {
        residual /= confidenceAvg;
      }
      else
      {
        residual /= m_CandidateConfidences[candidateIndex][0];
      }

      // establish cost of this equation
      TCoordinate cost = residual * (1.0 + outlierScore[i]);

      if (this->GetDebug())
      {
        std::cout << " :" << std::setw(6) << outlierScore[i];
        std::cout << " =" << std::setw(8) << cost;
        std::cout << std::endl;
      }
      if (cost > maxCost)
      {
        maxCost = cost;
        maxIndex = i;
      }
    }
    if (this->GetDebug())
    {
      std::cout << std::endl;
    }

    static float const sqrtDim = std::sqrt(ImageDimension);
    if (maxCost < m_AbsoluteThreshold * sqrtDim)
    {
      outlierExists = false;
    }
    else // eliminate the problematic equation
    {
      SizeValueType candidateIndex = equationToCandidate[maxIndex];
      std::cout << "Outlier detected. Eq. " << maxIndex << ", Reg. " << candidateIndex;

      // calculate indices of the involved tiles
      SizeValueType linIndex = candidateIndex % m_LinearMontageSize;
      TileIndexType currentIndex = this->LinearIndexTonDIndex(linIndex);
      TileIndexType referenceIndex = currentIndex;
      unsigned      dim = candidateIndex / m_LinearMontageSize;
      referenceIndex[dim] = currentIndex[dim] - 1;
      std::cout << ": " << currentIndex << "->" << referenceIndex << "  T: ";

      if (!m_TransformCandidates[candidateIndex].empty())
      {
        std::cout << m_TransformCandidates[candidateIndex][0];
        m_TransformCandidates[candidateIndex].erase(m_TransformCandidates[candidateIndex].begin());
      }
      else
      {
        std::cout << "zeroes";
      }

      if (!m_TransformCandidates[candidateIndex].empty())
      {
        // get a new equation from m_TransformCandidates
        const float &                        confidence = m_CandidateConfidences[candidateIndex][0];
        typename SparseMatrix::InnerIterator it(regCoef, maxIndex);
        regCoef.coeffRef(maxIndex, it.index()) = -confidence;
        ++it;
        regCoef.coeffRef(maxIndex, it.index()) = confidence;

        const TranslationOffset & candidateOffset = m_TransformCandidates[candidateIndex][0];
        for (unsigned d = 0; d < ImageDimension; d++)
        {
          translations(maxIndex, d) = confidence * candidateOffset[d];
        }
        std::cout << "  Replaced by T: " << candidateOffset;
      }
      else
      {
        // nudge this registration towards zero adjustment
        typename SparseMatrix::InnerIterator it(regCoef, maxIndex);
        regCoef.coeffRef(maxIndex, it.index()) = 0.01 * regCoef.coeffRef(maxIndex, it.index());
        ++it;
        regCoef.coeffRef(maxIndex, it.index()) = 0.01 * regCoef.coeffRef(maxIndex, it.index());

        for (unsigned d = 0; d < ImageDimension; d++)
        {
          translations(maxIndex, d) = 0;
        }
        std::cout << "  Replaced by zeroes.";
      }
    }
  }
}

template <typename TImageType, typename TCoordinate>
void
TileMontage<TImageType, TCoordinate>::GenerateData()
{
  // initialize mosaic bounds
  auto           input0 = static_cast<const ImageType *>(this->GetInput(0));
  ImageIndexType ind = input0->GetLargestPossibleRegion().GetIndex();
  m_MinInner = ind;
  m_MinOuter = ind;
  ind += input0->GetLargestPossibleRegion().GetSize();
  m_MaxOuter = ind;
  m_MaxInner.Fill(NumericTraits<TCoordinate>::max());

  m_NumberOfPairs = 0; // number of equations = number of registration pairs
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    m_NumberOfPairs += (m_LinearMontageSize / m_MontageSize[d]) * (m_MontageSize[d] - 1);
  }

  TileIndexType ind0;
  ind0.Fill(0);
  m_FinishedPairs = 0;
  m_CurrentAdjustments[0].Fill(0.0); // 0 translation by default

  typename ThreadPool::Pointer pool = ThreadPool::GetInstance();
  ThreadIdType                 tpThreads = pool->GetMaximumNumberOfThreads();
  ThreadIdType                 workUnits = this->GetNumberOfWorkUnits();
  if (tpThreads <= workUnits)
  {
    pool->AddThreads(workUnits - tpThreads + 1);
  }

  std::vector<std::future<void>> futures(m_LinearMontageSize);
  SizeValueType                  waited = 0;
  for (SizeValueType i = 0; i < m_LinearMontageSize; i++)
  {
    // filling ThreadPool's queue with more top-level jobs
    // than there are threads causes dead-lock, so let's be conservative
    if (i - waited >= workUnits)
    {
      TileIndexType currentIndex = this->LinearIndexTonDIndex(waited);
      futures[waited].get(); // waits for the computation to finish
      this->ReleaseMemory(currentIndex);
      // all registrations finished = 95% of total progress
      this->UpdateProgress(m_FinishedPairs * 0.95 / m_NumberOfPairs);
      ++waited;
    }

    TileIndexType currentIndex = this->LinearIndexTonDIndex(i);
    futures[i] = pool->AddWork([this, currentIndex]() {
      // register i-th tile to adjacent tiles along all dimensions (lower index only)
      TileIndexType currentTile = currentIndex; // we cannot modify currentIndex
      for (unsigned regDim = 0; regDim < ImageDimension; regDim++)
      {
        if (currentTile[regDim] > 0) // we are not at the edge along this dimension
        {
          TileIndexType referenceIndex = currentTile;
          referenceIndex[regDim] = currentTile[regDim] - 1;
          this->RegisterPair(referenceIndex, currentTile);
          ++m_FinishedPairs;
        }
      }
    });
    // optimize positions later, now just set the expected position (no translation)
    m_CurrentAdjustments[waited].Fill(0.0);
  }

  for (SizeValueType i = waited; i < m_LinearMontageSize; i++)
  {
    TileIndexType currentIndex = this->LinearIndexTonDIndex(i);
    futures[i].get(); // waits for the computation to finish
    this->ReleaseMemory(currentIndex);
    // all registrations finished = 95% of total progress
    this->UpdateProgress(m_FinishedPairs * 0.95 / m_NumberOfPairs);
  }

  this->OptimizeTiles();

  // clear rest of the cache after montaging is finished
  RegionType reg0;
  for (SizeValueType i = 0; i < m_LinearMontageSize; i++)
  {
    TileIndexType tileIndex = this->LinearIndexTonDIndex(i);
    WriteOutTransform(tileIndex, m_CurrentAdjustments[i]);
    m_FFTCache[i] = nullptr;
    if (!m_Filenames[i].empty()) // release the input image too
    {
      this->SetInputTile(tileIndex, m_Dummy);
    }
    if (m_Tiles[i])
    {
      m_Tiles[i]->SetBufferedRegion(reg0);
      m_Tiles[i]->Allocate(false);
    }
  }
  this->UpdateProgress(1.0f);
}

} // namespace itk

#endif // itkTileMontage_hxx
