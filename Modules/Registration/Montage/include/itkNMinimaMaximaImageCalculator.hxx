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
#ifndef itkNMinimaMaximaImageCalculator_hxx
#define itkNMinimaMaximaImageCalculator_hxx


#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkMultiThreaderBase.h"
#include "itkNumericTraits.h"
#include <algorithm>

namespace itk
{

template <typename TInputImage>
NMinimaMaximaImageCalculator<TInputImage>::NMinimaMaximaImageCalculator()
{
  m_Image = TInputImage::New();
}

template <typename TInputImage>
template <typename TComparator>
inline void
NMinimaMaximaImageCalculator<TInputImage>::SortedInsert(ValueVector &     vals,
                                                        IndexVector &     indices,
                                                        const PixelType & val,
                                                        const IndexType & ind,
                                                        TComparator       comp)
{
  auto ub = std::upper_bound(vals.begin(), vals.end(), val, comp);
  if (ub != vals.end())
  {
    unsigned i = ub - vals.begin();
    vals.insert(ub, val);
    vals.pop_back();
    indices.insert(indices.begin() + i, ind);
    indices.pop_back();
  }
}

template <typename TInputImage>
void
NMinimaMaximaImageCalculator<TInputImage>::InternalCompute()
{
  if (!m_RegionSetByUser)
  {
    m_Region = m_Image->GetRequestedRegion();
  }

  m_Minima.clear();
  m_Minima.reserve(m_N + 1); // for overflow during insertion
  m_Minima.resize(m_N, NumericTraits<PixelType>::max());
  m_Maxima.clear();
  m_Maxima.reserve(m_N + 1); // for overflow during insertion
  m_Maxima.resize(m_N, NumericTraits<PixelType>::NonpositiveMin());

  m_IndicesOfMinima.reserve(m_N + 1); // for overflow during insertion
  m_IndicesOfMinima.resize(m_N);
  m_IndicesOfMaxima.reserve(m_N + 1); // for overflow during insertion
  m_IndicesOfMaxima.resize(m_N);

  typename MultiThreaderBase::Pointer mt = MultiThreaderBase::New();
  mt->template ParallelizeImageRegion<ImageDimension>(
    m_Region,
    [this](const RegionType & region) {
      thread_local ValueVector mins;
      thread_local ValueVector maxs;
      thread_local IndexVector minInd;
      thread_local IndexVector maxInd;
      if (m_ComputeMinima)
      {
        mins.clear();
        mins.reserve(m_N + 1); // for overflow during insertion
        mins.resize(m_N, NumericTraits<PixelType>::max());
        minInd.reserve(m_N + 1); // for overflow during insertion
        minInd.resize(m_N);
      }
      if (m_ComputeMaxima)
      {
        maxs.clear();
        maxs.reserve(m_N + 1); // for overflow during insertion
        maxs.resize(m_N, NumericTraits<PixelType>::NonpositiveMin());
        maxInd.reserve(m_N + 1); // for overflow during insertion
        maxInd.resize(m_N);
      }

      std::greater<PixelType>                      compGreater;
      ImageRegionConstIteratorWithIndex<ImageType> iIt(this->m_Image, region);
      for (; !iIt.IsAtEnd(); ++iIt)
      {
        const IndexType & ind = iIt.GetIndex();
        const PixelType & val = iIt.Get();
        if (m_ComputeMinima)
        {
          SortedInsert(mins, minInd, val, ind);
        }
        if (m_ComputeMaxima)
        {
          SortedInsert(maxs, maxInd, val, ind, compGreater);
        }
      }

      // merge
      std::lock_guard<std::mutex> mutexHolder(m_Mutex);
      if (m_ComputeMinima)
      {
        for (unsigned i = 0; i < m_N; i++)
        {
          SortedInsert(m_Minima, m_IndicesOfMinima, mins[i], minInd[i]);
        }
      }
      if (m_ComputeMaxima)
      {
        for (unsigned i = 0; i < m_N; i++)
        {
          SortedInsert(m_Maxima, m_IndicesOfMaxima, maxs[i], maxInd[i], compGreater);
        }
      }
    },
    nullptr);
}

template <typename TInputImage>
void
NMinimaMaximaImageCalculator<TInputImage>::Compute()
{
  m_ComputeMinima = true;
  m_ComputeMaxima = true;
  this->InternalCompute();
}

template <typename TInputImage>
void
NMinimaMaximaImageCalculator<TInputImage>::ComputeMinima()
{
  m_ComputeMinima = true;
  m_ComputeMaxima = false;
  this->InternalCompute();
}

template <typename TInputImage>
void
NMinimaMaximaImageCalculator<TInputImage>::ComputeMaxima()
{
  m_ComputeMinima = false;
  m_ComputeMaxima = true;
  this->InternalCompute();
}

template <typename TInputImage>
void
NMinimaMaximaImageCalculator<TInputImage>::SetRegion(const RegionType & region)
{
  m_Region = region;
  m_RegionSetByUser = true;
}

template <typename TInputImage>
void
NMinimaMaximaImageCalculator<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Minima:";
  for (unsigned i = 0; i < m_Minima.size(); i++)
  {
    os << " " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Minima[i]);
  }
  os << std::endl;

  os << indent << "Indices of Minima:";
  for (unsigned i = 0; i < m_IndicesOfMinima.size(); i++)
  {
    os << " " << m_IndicesOfMinima[i];
  }
  os << std::endl;

  os << indent << "Maxima:";
  for (unsigned i = 0; i < m_Maxima.size(); i++)
  {
    os << " " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Maxima[i]);
  }
  os << std::endl;

  os << indent << "Indices of Maxima:";
  for (unsigned i = 0; i < m_IndicesOfMaxima.size(); i++)
  {
    os << " " << m_IndicesOfMaxima[i];
  }
  os << std::endl;

  itkPrintSelfObjectMacro(Image);
  os << indent << "Region: " << std::endl;
  m_Region.Print(os, indent.GetNextIndent());
  os << indent << "Region set by User: " << m_RegionSetByUser << std::endl;
}

} // end namespace itk

#endif
