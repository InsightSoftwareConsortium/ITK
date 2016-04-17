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

#include "itkImageIORegion.h"

namespace itk
{
ImageIORegion
::ImageIORegion()
{
  m_ImageDimension = 2;
  m_Index.resize(2);
  m_Size.resize(2);
  std::fill(m_Index.begin(), m_Index.end(), 0);
  std::fill(m_Size.begin(), m_Size.end(), 0);
}

ImageIORegion
::~ImageIORegion()
{}

ImageIORegion
::ImageIORegion(unsigned int dimension)
{
  m_ImageDimension = dimension;
  m_Index.resize(m_ImageDimension);
  m_Size.resize(m_ImageDimension);
  std::fill(m_Index.begin(), m_Index.end(), 0);
  std::fill(m_Size.begin(), m_Size.end(), 0);
}

ImageIORegion
::ImageIORegion(const Self & region):Region()
{
  m_Index = region.m_Index;
  m_Size = region.m_Size;
  m_ImageDimension = region.m_ImageDimension;
}

void
ImageIORegion
::operator=(const Self & region)
{
  m_Index = region.m_Index;
  m_Size = region.m_Size;
  m_ImageDimension = region.m_ImageDimension;
}

std::ostream & operator<<(std::ostream & os, const ImageIORegion & region)
{
  region.Print(os);
  return os;
}

/** Set the index defining the corner of the region. */
void
ImageIORegion
::SetIndex(const IndexType & index)
{
  m_Index = index;
}

/** Get index defining the corner of the region. */
const ImageIORegion::IndexType &
ImageIORegion
::GetIndex() const
{
  return m_Index;
}

ImageIORegion::IndexType &
ImageIORegion
::GetModifiableIndex()
{
  return m_Index;
}


/** Set the size of the region. This plus the index determines the
 * rectangular shape, or extent, of the region. */
void
ImageIORegion
::SetSize(const SizeType & size)
{
  m_Size = size;
}

/** Get the size of the region. */
const ImageIORegion::SizeType &
ImageIORegion
::GetSize() const
{
  return m_Size;
}

ImageIORegion::SizeType &
ImageIORegion
::GetModifiableSize()
{
  return m_Size;
}

unsigned int
ImageIORegion
::GetImageDimension() const
{
  return m_ImageDimension;
}

ImageIORegion::RegionType
ImageIORegion
::GetRegionType() const
{
  return Superclass::ITK_STRUCTURED_REGION;
}

unsigned int
ImageIORegion
::GetRegionDimension() const
{
  unsigned int dim = 0;

  for ( unsigned int i = 0; i < m_ImageDimension; i++ )
    {
    if ( m_Size[i] > 1 ) { dim++; }
    }
  return dim;
}

ImageIORegion::SizeValueType
ImageIORegion
::GetSize(unsigned long i) const
{
  if ( i >= m_Size.size() )
    {
    itkExceptionMacro("Invalid index in GetSize()");
    }
  return m_Size[i];
}

ImageIORegion::IndexValueType
ImageIORegion
::GetIndex(unsigned long i) const
{
  if ( i >= m_Index.size() )
    {
    itkExceptionMacro("Invalid index in GetIndex()");
    }
  return m_Index[i];
}

void
ImageIORegion
::SetSize(const unsigned long i, SizeValueType size)
{
  if ( i >= m_Size.size() )
    {
    itkExceptionMacro("Invalid index in SetSize()");
    }
  m_Size[i] = size;
}

void
ImageIORegion
::SetIndex(const unsigned long i, IndexValueType idx)
{
  if ( i >= m_Index.size() )
    {
    itkExceptionMacro("Invalid index in SetIndex()");
    }
  m_Index[i] = idx;
}

bool
ImageIORegion
::IsInside(const IndexType & index) const
{
  if ( m_ImageDimension != index.size() )
    {
    return false;
    }
  for ( unsigned int i = 0; i < m_ImageDimension; i++ )
    {
    if ( index[i] < m_Index[i] )
      {
      return false;
      }
    if ( static_cast< SizeValueType >( index[i] - m_Index[i] ) >= m_Size[i] )
      {
      return false;
      }
    }
  return true;
}

/** Test if a region (the argument) is completly inside of this region */
bool
ImageIORegion
::IsInside(const Self & region) const
{
  IndexType beginCorner = region.GetIndex();

  if ( !this->IsInside(beginCorner) )
    {
    return false;
    }
  IndexType endCorner(region.m_ImageDimension);
  SizeType  size = region.GetSize();
  for ( unsigned int i = 0; i < m_ImageDimension; i++ )
    {
    endCorner[i] = beginCorner[i] + size[i] - 1;
    }
  if ( !this->IsInside(endCorner) )
    {
    return false;
    }
  return true;
}

/** Get the number of pixels contained in this region. This just
   * multiplies the size components. */
ImageIORegion::SizeValueType
ImageIORegion
::GetNumberOfPixels(void) const
{
  size_t numPixels = 1;

  for ( unsigned int d = 0; d < this->GetImageDimension(); ++d )
    {
    numPixels *= m_Size[d];
    }

  return Math::CastWithRangeCheck<ImageIORegion::SizeValueType>(numPixels);
}

bool
ImageIORegion
::operator==(const Self & region) const
{
  bool same;

  same = ( m_Index == region.m_Index );
  same = same && ( m_Size == region.m_Size );
  same = same && ( m_ImageDimension == region.m_ImageDimension );
  return same;
}

/** Compare two regions. */
bool
ImageIORegion
::operator!=(const Self & region) const
{
  bool same;

  same = ( m_Index == region.m_Index );
  same = same && ( m_Size == region.m_Size );
  same = same && ( m_ImageDimension == region.m_ImageDimension );
  return !same;
}

void
ImageIORegion
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Dimension: " << this->GetImageDimension() << std::endl;
  os << indent << "Index: ";
  for ( ImageIORegion::IndexType::const_iterator i = this->GetIndex().begin();
        i != this->GetIndex().end(); ++i )
    {
    os << *i << " ";
    }
  os << std::endl;
  os << indent << "Size: ";
  for ( ImageIORegion::SizeType::const_iterator k = this->GetSize().begin();
        k != this->GetSize().end(); ++k )
    {
    os << *k << " ";
    }
  os << std::endl;
}
} //namespace itk
