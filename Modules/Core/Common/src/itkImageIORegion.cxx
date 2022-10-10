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

#include "itkImageIORegion.h"
#include <algorithm> // For copy_n.

namespace itk
{

ImageIORegion::~ImageIORegion() = default;

ImageIORegion::ImageIORegion(unsigned int dimension)
  : m_ImageDimension{ dimension }
  , m_Index(dimension)
  , m_Size(dimension)
{}


ImageIORegion &
ImageIORegion::operator=(const Self & region)
{
  if ((region.m_Index.size() == m_Index.size()) && (region.m_Size.size() == m_Size.size()))
  {
    // Copy the values from 'region', but do not change the size of m_Index and m_Size.
    std::copy_n(region.m_Index.cbegin(), m_Index.size(), m_Index.begin());
    std::copy_n(region.m_Size.cbegin(), m_Size.size(), m_Size.begin());
    m_ImageDimension = region.m_ImageDimension;
  }
  else
  {
    // Copy the region to 'temp' and then do a non-throwing move.
    Self temp(region);
    static_assert(noexcept(*this = std::move(temp)),
                  "Move-assignment should be noexcept, to provide the strong exception guarantee for copy-assignment.");
    *this = std::move(temp);
  }
  return *this;
}


std::ostream &
operator<<(std::ostream & os, const ImageIORegion & region)
{
  region.Print(os);
  return os;
}

/** Set the index defining the corner of the region. */
void
ImageIORegion::SetIndex(const IndexType & index)
{
  m_Index = index;
}

/** Get index defining the corner of the region. */
const ImageIORegion::IndexType &
ImageIORegion::GetIndex() const
{
  return m_Index;
}

ImageIORegion::IndexType &
ImageIORegion::GetModifiableIndex()
{
  return m_Index;
}


/** Set the size of the region. This plus the index determines the
 * rectangular shape, or extent, of the region. */
void
ImageIORegion::SetSize(const SizeType & size)
{
  m_Size = size;
}

/** Get the size of the region. */
const ImageIORegion::SizeType &
ImageIORegion::GetSize() const
{
  return m_Size;
}

ImageIORegion::SizeType &
ImageIORegion::GetModifiableSize()
{
  return m_Size;
}

unsigned int
ImageIORegion::GetImageDimension() const
{
  return m_ImageDimension;
}

ImageIORegion::RegionType
ImageIORegion::GetRegionType() const
{
  return Superclass::RegionEnum::ITK_STRUCTURED_REGION;
}

unsigned int
ImageIORegion::GetRegionDimension() const
{
  unsigned int dim = 0;

  for (unsigned int i = 0; i < m_ImageDimension; ++i)
  {
    if (m_Size[i] > 1)
    {
      ++dim;
    }
  }
  return dim;
}

ImageIORegion::SizeValueType
ImageIORegion::GetSize(unsigned long i) const
{
  if (i >= m_Size.size())
  {
    itkExceptionMacro("Invalid index in GetSize()");
  }
  return m_Size[i];
}

ImageIORegion::IndexValueType
ImageIORegion::GetIndex(unsigned long i) const
{
  if (i >= m_Index.size())
  {
    itkExceptionMacro("Invalid index in GetIndex()");
  }
  return m_Index[i];
}

void
ImageIORegion::SetSize(const unsigned long i, SizeValueType size)
{
  if (i >= m_Size.size())
  {
    itkExceptionMacro("Invalid index in SetSize()");
  }
  m_Size[i] = size;
}

void
ImageIORegion::SetIndex(const unsigned long i, IndexValueType idx)
{
  if (i >= m_Index.size())
  {
    itkExceptionMacro("Invalid index in SetIndex()");
  }
  m_Index[i] = idx;
}

bool
ImageIORegion::IsInside(const IndexType & index) const
{
  if (m_ImageDimension != index.size())
  {
    return false;
  }
  for (unsigned int i = 0; i < m_ImageDimension; ++i)
  {
    if (index[i] < m_Index[i])
    {
      return false;
    }
    if (static_cast<SizeValueType>(index[i] - m_Index[i]) >= m_Size[i])
    {
      return false;
    }
  }
  return true;
}


/** Test if a region (the argument) is completely inside of this region. If
 * the region that is passed as argument has a size of value zero, or if the
 * dimensionality is zero, then it will not be considered to be inside of the
 * current region, even its starting index is inside. */
bool
ImageIORegion::IsInside(const Self & otherRegion) const
{
  if (m_ImageDimension == 0 || otherRegion.m_ImageDimension != m_ImageDimension)
  {
    return false;
  }
  const auto & otherIndex = otherRegion.m_Index;
  const auto & otherSize = otherRegion.m_Size;

  for (unsigned int i = 0; i < m_ImageDimension; ++i)
  {
    if (otherIndex[i] < m_Index[i] || otherSize[i] == 0 ||
        otherIndex[i] + static_cast<IndexValueType>(otherSize[i]) > m_Index[i] + static_cast<IndexValueType>(m_Size[i]))
    {
      return false;
    }
  }
  return true;
}

/** Get the number of pixels contained in this region. This just
 * multiplies the size components. */
ImageIORegion::SizeValueType
ImageIORegion::GetNumberOfPixels() const
{
  size_t numPixels = 1;

  for (unsigned int d = 0; d < this->GetImageDimension(); ++d)
  {
    numPixels *= m_Size[d];
  }

  return Math::CastWithRangeCheck<ImageIORegion::SizeValueType>(numPixels);
}

bool
ImageIORegion::operator==(const Self & region) const
{
  return (m_Index == region.m_Index) && (m_Size == region.m_Size) && (m_ImageDimension == region.m_ImageDimension);
}

void
ImageIORegion::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Dimension: " << this->GetImageDimension() << std::endl;
  os << indent << "Index: ";
  for (const auto i : this->GetIndex())
  {
    os << i << " ";
  }
  os << std::endl;
  os << indent << "Size: ";
  for (const auto k : this->GetSize())
  {
    os << k << " ";
  }
  os << std::endl;
}
} // namespace itk
