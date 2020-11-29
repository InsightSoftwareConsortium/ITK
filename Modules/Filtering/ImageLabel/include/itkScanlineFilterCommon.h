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
#ifndef itkScanlineFilterCommon_h
#define itkScanlineFilterCommon_h

#include "itkImageToImageFilter.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include <atomic>
#include <deque>
#include <functional>
#include <mutex>
#include <vector>

namespace itk
{
/**
 * \class ScanlineFilterCommon
 * \brief Helper class for a group of filters which operate on scan-lines.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ITKImageLabel
 */
template <typename TInputImage, typename TOutputImage>
class ScanlineFilterCommon
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScanlineFilterCommon);

  using Self = ScanlineFilterCommon;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  void
  Register() const
  {
    auto * obj = static_cast<Object *>(m_EnclosingFilter.GetPointer());
    obj->Register();
  }
  void
  UnRegister() const noexcept
  {
    auto * obj = static_cast<Object *>(m_EnclosingFilter.GetPointer());
    obj->UnRegister();
  }
  static Pointer
  New()
  {
    Pointer smartPtr = ObjectFactory<Self>::Create();
    if (smartPtr == nullptr)
    {
      smartPtr = new Self(nullptr);
    }
    smartPtr->UnRegister();
    return smartPtr;
  }

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;
  using OffsetType = typename TInputImage::OffsetType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputRegionType = typename TOutputImage::RegionType;
  using RegionType = OutputRegionType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using OutputSizeType = typename TOutputImage::SizeType;
  using OutputOffsetType = typename TOutputImage::OffsetType;
  using OutputImagePixelType = typename TOutputImage::PixelType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Concept checking -- input and output dimensions must be the same
  itkConceptMacro(SameDimension, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
#endif

  using EnclosingFilter = ImageToImageFilter<TInputImage, TOutputImage>;

  ScanlineFilterCommon(EnclosingFilter * enclosingFilter)
    : m_EnclosingFilter(enclosingFilter)
    , m_FullyConnected(false)
  {}
  ~ScanlineFilterCommon() = default;

protected:
  using InternalLabelType = SizeValueType;
  using OutSizeType = typename TOutputImage::RegionType::SizeType;

  struct RunLength
  {
    SizeValueType                      length;
    typename InputImageType::IndexType where;
    InternalLabelType                  label;

    RunLength(SizeValueType iLength, const IndexType & iWhere, InternalLabelType iLabel = 0)
      : length(iLength)
      , where(iWhere)
      , label(iLabel)
    {}
  };

  using LineEncodingType = std::vector<RunLength>;
  using LineEncodingIterator = typename LineEncodingType::iterator;
  using LineEncodingConstIterator = typename LineEncodingType::const_iterator;

  using OffsetVectorType = std::vector<OffsetValueType>;
  using OffsetVectorConstIterator = typename OffsetVectorType::const_iterator;

  using LineMapType = std::vector<LineEncodingType>;

  using UnionFindType = std::vector<InternalLabelType>;
  using ConsecutiveVectorType = std::vector<OutputPixelType>;

  SizeValueType
  IndexToLinearIndex(const IndexType & index) const
  {
    SizeValueType linearIndex = 0;
    SizeValueType stride = 1;
    RegionType    requestedRegion = m_EnclosingFilter->GetOutput()->GetRequestedRegion();
    // ignore x axis, which is always full size
    for (unsigned dim = 1; dim < ImageDimension; dim++)
    {
      itkAssertOrThrowMacro(requestedRegion.GetIndex(dim) <= index[dim], "Index must be within the requested region!");
      linearIndex += (index[dim] - requestedRegion.GetIndex(dim)) * stride;
      stride *= requestedRegion.GetSize(dim);
    }
    return linearIndex;
  }

  void
  InitUnion(InternalLabelType numberOfLabels)
  {
    m_UnionFind = UnionFindType(numberOfLabels + 1);

    typename LineMapType::iterator MapBegin, MapEnd, LineIt;
    MapBegin = m_LineMap.begin();
    MapEnd = m_LineMap.end();
    LineIt = MapBegin;
    InternalLabelType label = 1;
    for (LineIt = MapBegin; LineIt != MapEnd; ++LineIt)
    {
      LineEncodingIterator cIt;
      for (cIt = LineIt->begin(); cIt != LineIt->end(); ++cIt)
      {
        cIt->label = label;
        m_UnionFind[label] = label;
        label++;
      }
    }
  }

  InternalLabelType
  LookupSet(const InternalLabelType label)
  {
    InternalLabelType l = label;
    while (l != m_UnionFind[l])
    {
      l = m_UnionFind[l]; // transitively sets equivalence
    }
    return l;
  }

  void
  LinkLabels(const InternalLabelType label1, const InternalLabelType label2)
  {
    std::lock_guard<std::mutex> mutexHolder(m_Mutex);
    InternalLabelType           E1 = this->LookupSet(label1);
    InternalLabelType           E2 = this->LookupSet(label2);

    if (E1 < E2)
    {
      m_UnionFind[E2] = E1;
    }
    else
    {
      m_UnionFind[E1] = E2;
    }
  }

  SizeValueType
  CreateConsecutive(OutputPixelType backgroundValue)
  {
    const size_t N = m_UnionFind.size();

    m_Consecutive = ConsecutiveVectorType(N);
    m_Consecutive[0] = backgroundValue;

    OutputPixelType consecutiveLabel = 0;
    SizeValueType   count = 0;

    for (size_t i = 1; i < N; i++)
    {
      const auto label = static_cast<size_t>(m_UnionFind[i]);
      if (label == i)
      {
        if (consecutiveLabel == backgroundValue)
        {
          ++consecutiveLabel;
        }
        m_Consecutive[label] = consecutiveLabel;
        ++consecutiveLabel;
        ++count;
      }
    }
    return count;
  }

  bool
  CheckNeighbors(const OutputIndexType & A, const OutputIndexType & B) const
  {
    // This checks whether the line encodings are really neighbors. The first
    // dimension gets ignored because the encodings are along that axis.
    SizeValueType diffSum = 0;
    for (unsigned i = 1; i < OutputImageDimension; i++)
    {
      SizeValueType diff = Math::abs(A[i] - B[i]);
      if (diff > 1)
      {
        return false;
      }
      diffSum += diff;
    }

    if (!this->m_FullyConnected)
    {
      return (diffSum <= 1); // indices can differ only along one dimension
    }
    return true;
  }

  using CompareLinesCallback = std::function<void(const LineEncodingConstIterator & currentRun,
                                                  const LineEncodingConstIterator & neighborRun,
                                                  OffsetValueType                   oStart,
                                                  OffsetValueType                   oLast)>;

  void
  CompareLines(const LineEncodingType & current,
               const LineEncodingType & Neighbour,
               bool                     sameLineOffset,
               bool                     labelCompare,
               OutputPixelType          background,
               CompareLinesCallback     callback)
  {
    bool sameLine = sameLineOffset;
    if (sameLineOffset)
    {
      OutputOffsetType Off = current[0].where - Neighbour[0].where;

      for (unsigned int i = 1; i < ImageDimension; i++)
      {
        if (Off[i] != 0)
        {
          sameLine = false;
          break;
        }
      }
    }

    OffsetValueType offset = 0;
    if (m_FullyConnected || sameLine)
    {
      offset = 1;
    }

    LineEncodingConstIterator nIt, mIt, cIt;

    mIt = Neighbour.begin(); // out marker iterator

    for (cIt = current.begin(); cIt != current.end(); ++cIt)
    {
      if (!labelCompare || cIt->label != InternalLabelType(background))
      {
        OffsetValueType cStart = cIt->where[0]; // the start x position
        OffsetValueType cLast = cStart + cIt->length - 1;

        if (labelCompare)
        {
          mIt = Neighbour.begin();
        }

        for (nIt = mIt; nIt != Neighbour.end(); ++nIt)
        {
          if (!labelCompare || cIt->label != nIt->label)
          {
            OffsetValueType nStart = nIt->where[0];
            OffsetValueType nLast = nStart + nIt->length - 1;

            // there are a few ways that neighbouring lines might overlap
            //   neighbor      S------------------E
            //   current    S------------------------E
            //-------------
            //   neighbor      S------------------E
            //   current    S----------------E
            //-------------
            //   neighbor      S------------------E
            //   current             S------------------E
            //-------------
            //   neighbor      S------------------E
            //   current             S-------E
            //-------------
            OffsetValueType ss1 = nStart - offset;
            // OffsetValueType ss2 = nStart + offset;
            OffsetValueType ee1 = nLast - offset;
            OffsetValueType ee2 = nLast + offset;

            bool            eq = false;
            OffsetValueType oStart = 0;
            OffsetValueType oLast = 0;

            // the logic here can probably be improved a lot
            if ((ss1 >= cStart) && (ee2 <= cLast))
            {
              // case 1
              eq = true;
              oStart = ss1;
              oLast = ee2;
            }
            else if ((ss1 <= cStart) && (ee2 >= cLast))
            {
              // case 4
              eq = true;
              oStart = cStart;
              oLast = cLast;
            }
            else if ((ss1 <= cLast) && (ee2 >= cLast))
            {
              // case 2
              eq = true;
              oStart = ss1;
              oLast = cLast;
            }
            else if ((ss1 <= cStart) && (ee2 >= cStart))
            {
              // case 3
              eq = true;
              oStart = cStart;
              oLast = ee2;
            }

            if (eq)
            {
              callback(cIt, nIt, oStart, oLast);
              if (sameLineOffset && oStart == cStart && oLast == cLast)
              {
                mIt = nIt;
                break;
              }
            }

            if (!sameLineOffset && ee1 >= cLast)
            {
              // No point looking for more overlaps with the current run
              // because the neighbor run is either case 2 or 4
              mIt = nIt;
              break;
            }
          }
        }
      }
    }
  }

  void
  SetupLineOffsets(bool wholeNeighborhood)
  {
    // Create a neighborhood so that we can generate a table of offsets
    // to "previous" line indexes
    // We are going to mis-use the neighborhood iterators to compute the
    // offset for us. All this messing around produces an array of
    // offsets that will be used to index the map
    typename TOutputImage::Pointer output = m_EnclosingFilter->GetOutput();
    using PretendImageType = Image<OffsetValueType, TOutputImage::ImageDimension - 1>;
    using PretendSizeType = typename PretendImageType::RegionType::SizeType;
    using PretendIndexType = typename PretendImageType::RegionType::IndexType;
    using LineNeighborhoodType = ConstShapedNeighborhoodIterator<PretendImageType>;

    typename PretendImageType::Pointer fakeImage;
    fakeImage = PretendImageType::New();

    typename PretendImageType::RegionType LineRegion;

    OutSizeType OutSize = output->GetRequestedRegion().GetSize();

    PretendSizeType PretendSize;
    // The first dimension has been collapsed
    for (SizeValueType i = 0; i < PretendSize.GetSizeDimension(); i++)
    {
      PretendSize[i] = OutSize[i + 1];
    }

    LineRegion.SetSize(PretendSize);
    fakeImage->SetRegions(LineRegion);
    PretendSizeType kernelRadius;
    kernelRadius.Fill(1);
    LineNeighborhoodType lnit(kernelRadius, fakeImage, LineRegion);

    if (wholeNeighborhood)
    {
      setConnectivity(&lnit, m_FullyConnected);
    }
    else
    {
      setConnectivityPrevious(&lnit, m_FullyConnected);
    }

    typename LineNeighborhoodType::IndexListType ActiveIndexes;
    ActiveIndexes = lnit.GetActiveIndexList();

    typename LineNeighborhoodType::IndexListType::const_iterator LI;

    PretendIndexType idx = LineRegion.GetIndex();
    OffsetValueType  offset = fakeImage->ComputeOffset(idx);

    for (LI = ActiveIndexes.begin(); LI != ActiveIndexes.end(); ++LI)
    {
      m_LineOffsets.push_back(fakeImage->ComputeOffset(idx + lnit.GetOffset(*LI)) - offset);
    }

    if (wholeNeighborhood)
    {
      m_LineOffsets.push_back(0); // center pixel
    }
  }

  WeakPointer<EnclosingFilter> m_EnclosingFilter;

  struct WorkUnitData
  {
    SizeValueType firstLine;
    SizeValueType lastLine;
  };

  WorkUnitData
  CreateWorkUnitData(const RegionType & outputRegionForThread)
  {
    const SizeValueType xsizeForThread = outputRegionForThread.GetSize()[0];
    const SizeValueType numberOfLines = outputRegionForThread.GetNumberOfPixels() / xsizeForThread;

    const SizeValueType firstLine = this->IndexToLinearIndex(outputRegionForThread.GetIndex());
    const SizeValueType lastLine = firstLine + numberOfLines - 1;

    return WorkUnitData{ firstLine, lastLine };
  }

  /* Process the map and make appropriate entries in an equivalence table */
  void
  ComputeEquivalence(const SizeValueType workUnitResultsIndex, bool strictlyLess)
  {
    const OffsetValueType linecount = m_LineMap.size();
    WorkUnitData          wud = m_WorkUnitResults[workUnitResultsIndex];
    SizeValueType         lastLine = wud.lastLine;
    if (!strictlyLess)
    {
      lastLine++;
      // make sure we are not wrapping around
      itkAssertInDebugAndIgnoreInReleaseMacro(lastLine >= wud.lastLine);
    }
    for (SizeValueType thisIdx = wud.firstLine; thisIdx < lastLine; ++thisIdx)
    {
      if (!m_LineMap[thisIdx].empty())
      {
        auto it = this->m_LineOffsets.begin();
        while (it != this->m_LineOffsets.end())
        {
          OffsetValueType neighIdx = thisIdx + (*it);
          // check if the neighbor is in the map
          if (neighIdx >= 0 && neighIdx < linecount && !m_LineMap[neighIdx].empty())
          {
            // Now check whether they are really neighbors
            bool areNeighbors = this->CheckNeighbors(m_LineMap[thisIdx][0].where, m_LineMap[neighIdx][0].where);
            if (areNeighbors)
            {
              this->CompareLines(m_LineMap[thisIdx],
                                 m_LineMap[neighIdx],
                                 false,
                                 false,
                                 0,
                                 [this](const LineEncodingConstIterator & currentRun,
                                        const LineEncodingConstIterator & neighborRun,
                                        OffsetValueType,
                                        OffsetValueType) { this->LinkLabels(neighborRun->label, currentRun->label); });
            }
          }
          ++it;
        }
      }
    }
  }

protected:
  bool                  m_FullyConnected;
  OffsetVectorType      m_LineOffsets;
  UnionFindType         m_UnionFind;
  ConsecutiveVectorType m_Consecutive;
  std::mutex            m_Mutex;

  std::atomic<SizeValueType> m_NumberOfLabels;
  std::deque<WorkUnitData>   m_WorkUnitResults;
  LineMapType                m_LineMap;
};
} // end namespace itk

#endif
