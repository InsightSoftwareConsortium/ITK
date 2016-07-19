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
#ifndef itkRLEImage_hxx
#define itkRLEImage_hxx

#include "itkRLEImage.h"
#include "itkImageRegionConstIterator.h"


namespace itk
{
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
inline typename RLEImage<TPixel, VImageDimension, CounterType>::BufferType::IndexType
RLEImage<TPixel, VImageDimension, CounterType>::truncateIndex(const IndexType & index)
{
  typename BufferType::IndexType result;
  for (IndexValueType i = 0; i < VImageDimension - 1; i++)
    result[i] = index[i + 1];
  return result;
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
inline typename RLEImage<TPixel, VImageDimension, CounterType>::BufferType::SizeType
RLEImage<TPixel, VImageDimension, CounterType>::truncateSize(const SizeType & size)
{
  typename BufferType::SizeType result;
  for (IndexValueType i = 0; i < VImageDimension - 1; i++)
    result[i] = size[i + 1];
  return result;
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
typename RLEImage<TPixel, VImageDimension, CounterType>::BufferType::RegionType
RLEImage<TPixel, VImageDimension, CounterType>::truncateRegion(const RegionType & region)
{
  typename BufferType::RegionType result;
  result.SetIndex(truncateIndex(region.GetIndex()));
  result.SetSize(truncateSize(region.GetSize()));
  return result;
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::Allocate(bool initialize)
{
  itkAssertOrThrowMacro(this->GetBufferedRegion().GetSize(0) == this->GetLargestPossibleRegion().GetSize(0),
                        "BufferedRegion must contain complete run-length lines!");
  itkAssertOrThrowMacro(this->GetLargestPossibleRegion().GetSize(0) <= std::numeric_limits<CounterType>::max(),
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
      out.back().first += line[x].first;
  } while (x < line.size());
  out.swap(line);
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::CleanUp() const
{
  assert(!m_Buffer.empty());
  if (this->GetLargestPossibleRegion().GetSize(0) == 0)
    return;
#pragma omp parallel for
  for (CounterType z = 0; z < m_Buffer.size(); z++)
    for (CounterType y = 0; y < m_Buffer[0].size(); y++)
      CleanUpLine(m_Buffer[z][y]);
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
int
RLEImage<TPixel, VImageDimension, CounterType>::SetPixel(RLLine &         line,
                                                         IndexValueType & segmentRemainder,
                                                         IndexValueType & realIndex,
                                                         const TPixel &   value)
{
  // complete Run-Length Lines have to be buffered
  itkAssertOrThrowMacro(this->GetBufferedRegion().GetSize(0) == this->GetLargestPossibleRegion().GetSize(0),
                        "BufferedRegion must contain complete run-length lines!");
  if (line[realIndex].second == value) // already correct value
    return 0;
  else if (line[realIndex].first == 1) // single pixel segment
  {
    line[realIndex].second = value;
    if (m_OnTheFlyCleanup) // now see if we can merge it into adjacent segments
    {
      if (realIndex > 0 && realIndex < line.size() - 1 && line[realIndex + 1].second == value &&
          line[realIndex - 1].second == value)
      {
        // merge these 3 segments
        line[realIndex - 1].first += 1 + line[realIndex + 1].first;
        segmentRemainder += line[realIndex + 1].first;
        line.erase(line.begin() + realIndex, line.begin() + realIndex + 2);
        realIndex--;
        return -2;
      }
      if (realIndex > 0 && line[realIndex - 1].second == value)
      {
        // merge into previous
        line[realIndex - 1].first++;
        line.erase(line.begin() + realIndex);
        realIndex--;
        assert(segmentRemainder == 1);
        return -1;
      }
      else if (realIndex < line.size() - 1 && line[realIndex + 1].second == value)
      {
        // merge into next
        segmentRemainder = ++(line[realIndex + 1].first);
        line.erase(line.begin() + realIndex);
        return -1;
      }
    }
    return 0;
  }
  else if (segmentRemainder == 1 && realIndex < line.size() - 1 && line[realIndex + 1].second == value)
  {
    // shift this pixel to next segment
    line[realIndex].first--;
    segmentRemainder = ++(line[realIndex + 1].first);
    realIndex++;
    return 0;
  }
  else if (realIndex > 0 && segmentRemainder == line[realIndex].first && line[realIndex - 1].second == value)
  {
    // shift this pixel to previous segment
    line[realIndex].first--;
    line[realIndex - 1].first++;
    realIndex--;
    segmentRemainder = 1;
    return 0;
  }
  else if (segmentRemainder == 1) // insert after
  {
    line[realIndex].first--;
    line.insert(line.begin() + realIndex + 1, RLSegment(1, value));
    realIndex++;
    return +1;
  }
  else if (segmentRemainder == line[realIndex].first) // insert before
  {
    line[realIndex].first--;
    line.insert(line.begin() + realIndex, RLSegment(1, value));
    segmentRemainder = 1;
    return +1;
  }
  else // general case: split a segment into 3 segments
  {
    // first take care of values
    line.insert(line.begin() + realIndex + 1, 2, RLSegment(1, value));
    line[realIndex + 2].second = line[realIndex].second;

    // now take care of counts
    line[realIndex].first -= segmentRemainder;
    line[realIndex + 2].first = segmentRemainder - 1;
    realIndex++;
    segmentRemainder = 1;
    return +2;
  }
}

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
  for (IndexValueType x = 0; x < line.size(); x++)
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
}

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
  for (IndexValueType x = 0; x < line.size(); x++)
  {
    t += line[x].first;
    if (t > index[0] - bri0)
      return line[x].second;
  }
  throw itk::ExceptionObject(__FILE__, __LINE__, "Reached past the end of Run-Length line!", __FUNCTION__);
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RLEImage<TPixel, VImageDimension, CounterType>::PrintSelf(std::ostream & os, itk::Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Internal image (for storage of RLLine-s): " << std::endl;
  m_Buffer->Print(os, indent.GetNextIndent());

  itk::SizeValueType                        c = 0;
  itk::ImageRegionConstIterator<BufferType> it(m_Buffer, m_Buffer->GetBufferedRegion());
  while (!it.IsAtEnd())
  {
    c += it.Get().capacity();
    ++it;
  }

  double cr =
    double(c * (sizeof(PixelType) + sizeof(CounterType)) +
           sizeof(std::vector<RLLine>) * this->GetOffsetTable()[VImageDimension] / this->GetOffsetTable()[1]) /
    (this->GetOffsetTable()[VImageDimension] * sizeof(PixelType));

  os << indent << "OnTheFlyCleanup: " << (m_OnTheFlyCleanup ? "On" : "Off") << std::endl;
  os << indent << "RLEImage compressed pixel count: " << c << std::endl;
  int prec = os.precision(3);
  os << indent << "Compressed size in relation to original size: " << cr * 100 << "%" << std::endl;
  os.precision(prec);
}

} // end namespace itk
#endif // itkRLEImage_hxx
