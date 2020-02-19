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
#ifndef itkRLEImage_hxx
#define itkRLEImage_hxx

#include "itkImageRegionConstIterator.h" // for underlying buffer
#include "itkRLEImage.h"

// include all specializations of iterators and filters
// so only #include <itkRLEImage.h> is needed in user code
#include "itkRLEImageRegionIterator.h"
#include "itkRLEImageScanlineIterator.h"
#include "itkRLERegionOfInterestImageFilter.h"

namespace itk
{
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
inline typename RLEImage<TPixel, VImageDimension, CounterType>::BufferType::IndexType
RLEImage<TPixel, VImageDimension, CounterType>::truncateIndex(const IndexType & index)
{
  typename BufferType::IndexType result;
  for (IndexValueType i = 0; i < VImageDimension - 1; i++)
  {
    result[i] = index[i + 1];
  }
  return result;
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::Allocate(bool itkNotUsed(initialize))
{
  itkAssertOrThrowMacro(this->GetBufferedRegion().GetSize(0) == this->GetLargestPossibleRegion().GetSize(0),
                        "BufferedRegion must contain complete run-length lines!");
  itkAssertOrThrowMacro(this->GetLargestPossibleRegion().GetSize(0) <=
                          itk::SizeValueType(std::numeric_limits<CounterType>::max()),
                        "CounterType is not large enough to support image's X dimension!");
  this->ComputeOffsetTable();
  // SizeValueType num = static_cast<SizeValueType>(this->GetOffsetTable()[VImageDimension]);
  m_Buffer->Allocate(false);
  // if (initialize) //there is assumption that the image is fully formed after a call to allocate
  {
    RLSegment segment(CounterType(this->GetBufferedRegion().GetSize(0)), TPixel());
    RLLine    line(1);
    line[0] = segment;
    m_Buffer->FillBuffer(line);
  }
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::FillBuffer(const TPixel & value)
{
  RLSegment segment(CounterType(this->GetBufferedRegion().GetSize(0)), value);
  RLLine    line(1);

  line[0] = segment;
  m_Buffer->FillBuffer(line);
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::CleanUpLine(RLLine & line) const
{
  CounterType x = 0;
  RLLine      out;

  out.reserve(this->GetLargestPossibleRegion().GetSize(0));

  do
  {
    out.push_back(line[x]);
    while (++x < line.size() && line[x].second == line[x - 1].second)
    {
      out.back().first += line[x].first;
    }
  } while (x < line.size());

  out.swap(line);
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::CleanUp() const
{
  assert(!m_Buffer.empty());
  if (this->GetLargestPossibleRegion().GetSize(0) == 0)
  {
    return;
  }
#ifndef __GNUC__
#  pragma omp parallel for
#endif
  for (CounterType z = 0; z < m_Buffer.size(); z++)
  {
    for (CounterType y = 0; y < m_Buffer[0].size(); y++)
    {
      CleanUpLine(m_Buffer[z][y]);
    }
  }
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
int
RLEImage<TPixel, VImageDimension, CounterType>::SetPixel(RLLine &         line,
                                                         IndexValueType & segmentRemainder,
                                                         SizeValueType &  m_RealIndex,
                                                         const TPixel &   value)
{
  // complete Run-Length Lines have to be buffered
  itkAssertOrThrowMacro(this->GetBufferedRegion().GetSize(0) == this->GetLargestPossibleRegion().GetSize(0),
                        "BufferedRegion must contain complete run-length lines!");
  if (line[m_RealIndex].second == value) // already correct value
  {
    return 0;
  }
  else if (line[m_RealIndex].first == 1) // single pixel segment
  {
    line[m_RealIndex].second = value;
    if (m_OnTheFlyCleanup) // now see if we can merge it into adjacent segments
    {
      if (m_RealIndex > 0 && m_RealIndex < line.size() - 1 && line[m_RealIndex + 1].second == value &&
          line[m_RealIndex - 1].second == value)
      {
        // merge these 3 segments
        line[m_RealIndex - 1].first += 1 + line[m_RealIndex + 1].first;
        segmentRemainder += line[m_RealIndex + 1].first;
        line.erase(line.begin() + m_RealIndex, line.begin() + m_RealIndex + 2);
        m_RealIndex--;
        return -2;
      }
      if (m_RealIndex > 0 && line[m_RealIndex - 1].second == value)
      {
        // merge into previous
        line[m_RealIndex - 1].first++;
        line.erase(line.begin() + m_RealIndex);
        m_RealIndex--;
        assert(segmentRemainder == 1);
        return -1;
      }
      else if (m_RealIndex < line.size() - 1 && line[m_RealIndex + 1].second == value)
      {
        // merge into next
        segmentRemainder = ++(line[m_RealIndex + 1].first);
        line.erase(line.begin() + m_RealIndex);
        return -1;
      }
    }
    return 0;
  }
  else if (segmentRemainder == 1 && m_RealIndex < line.size() - 1 && line[m_RealIndex + 1].second == value)
  {
    // shift this pixel to next segment
    line[m_RealIndex].first--;
    segmentRemainder = ++(line[m_RealIndex + 1].first);
    m_RealIndex++;
    return 0;
  }
  else if (m_RealIndex > 0 && segmentRemainder == line[m_RealIndex].first && line[m_RealIndex - 1].second == value)
  {
    // shift this pixel to previous segment
    line[m_RealIndex].first--;
    line[m_RealIndex - 1].first++;
    m_RealIndex--;
    segmentRemainder = 1;
    return 0;
  }
  else if (segmentRemainder == 1) // insert after
  {
    line[m_RealIndex].first--;
    line.insert(line.begin() + m_RealIndex + 1, RLSegment(1, value));
    m_RealIndex++;
    return +1;
  }
  else if (segmentRemainder == line[m_RealIndex].first) // insert before
  {
    line[m_RealIndex].first--;
    line.insert(line.begin() + m_RealIndex, RLSegment(1, value));
    segmentRemainder = 1;
    return +1;
  }
  else // general case: split a segment into 3 segments
  {
    // first take care of values
    line.insert(line.begin() + m_RealIndex + 1, 2, RLSegment(1, value));
    line[m_RealIndex + 2].second = line[m_RealIndex].second;

    // now take care of counts
    line[m_RealIndex].first -= segmentRemainder;
    line[m_RealIndex + 2].first = segmentRemainder - 1;
    m_RealIndex++;
    segmentRemainder = 1;
    return +2;
  }
} // >::SetPixel

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::SetPixel(const IndexType & index, const TPixel & value)
{
  // complete Run-Length Lines have to be buffered
  itkAssertOrThrowMacro(this->GetBufferedRegion().GetSize(0) == this->GetLargestPossibleRegion().GetSize(0),
                        "BufferedRegion must contain complete run-length lines!");
  IndexValueType                 bri0 = this->GetBufferedRegion().GetIndex(0);
  typename BufferType::IndexType bi = truncateIndex(index);
  RLLine &                       line = m_Buffer->GetPixel(bi);
  IndexValueType                 t = 0;
  for (SizeValueType x = 0; x < line.size(); x++)
  {
    t += line[x].first;
    if (t > index[0] - bri0)
    {
      t -= index[0] - bri0; // we need to supply a reference
      SetPixel(line, t, x, value);
      return;
    }
  }
  throw itk::ExceptionObject(__FILE__, __LINE__, "Reached past the end of Run-Length line!", __FUNCTION__);
} // >::SetPixel

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
const TPixel &
RLEImage<TPixel, VImageDimension, CounterType>::GetPixel(const IndexType & index) const
{
  // complete Run-Length Lines have to be buffered
  itkAssertOrThrowMacro(this->GetBufferedRegion().GetSize(0) == this->GetLargestPossibleRegion().GetSize(0),
                        "BufferedRegion must contain complete run-length lines!");
  IndexValueType                 bri0 = this->GetBufferedRegion().GetIndex(0);
  typename BufferType::IndexType bi = truncateIndex(index);
  RLLine &                       line = m_Buffer->GetPixel(bi);
  IndexValueType                 t = 0;
  for (SizeValueType x = 0; x < line.size(); x++)
  {
    t += line[x].first;
    if (t > index[0] - bri0)
    {
      return line[x].second;
    }
  }
  throw itk::ExceptionObject(__FILE__, __LINE__, "Reached past the end of Run-Length line!", __FUNCTION__);
} // >::GetPixel

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::PrintSelf(std::ostream & os, itk::Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Internal image (for storage of RLLine-s): " << std::endl;
  m_Buffer->Print(os, indent.GetNextIndent());

  itk::SizeValueType c = 0;
  itk::SizeValueType pixelCount = this->GetOffsetTable()[VImageDimension];

  itk::ImageRegionConstIterator<BufferType> it(m_Buffer, m_Buffer->GetBufferedRegion());
  while (!it.IsAtEnd())
  {
    c += it.Get().capacity();
    ++it;
  }

  itk::SizeValueType memUsed =
    c * sizeof(RLSegment) + sizeof(std::vector<RLLine>) * (pixelCount / this->GetOffsetTable()[1]);
  double cr = double(memUsed) / (pixelCount * sizeof(PixelType));

  os << indent << "OnTheFlyCleanup: " << (m_OnTheFlyCleanup ? "On" : "Off") << std::endl;
  os << indent << "RLSegment count: " << c << std::endl;
  int prec = os.precision(3);
  os << indent << "Compressed size in relation to original size: " << cr * 100 << "%" << std::endl;
  os.precision(prec);
} // >::PrintSelf
} // end namespace itk
#endif // itkRLEImage_hxx
