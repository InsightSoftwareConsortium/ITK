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
#ifndef itkLabelObjectLine_hxx
#define itkLabelObjectLine_hxx

#include "itkLabelObjectLine.h"

namespace itk
{
template< unsigned int VImageDimension >
LabelObjectLine< VImageDimension >::LabelObjectLine() :
  m_Length( NumericTraits< SizeValueType >::ZeroValue() )
{
  m_Index.Fill( NumericTraits< IndexValueType >::ZeroValue() );
}

template< unsigned int VImageDimension >
LabelObjectLine< VImageDimension >::LabelObjectLine(const IndexType & idx, const LengthType & length) :
  m_Index( idx ),
  m_Length( length )
{}

template< unsigned int VImageDimension >
void LabelObjectLine< VImageDimension >::SetIndex(const IndexType & idx)
{
  m_Index = idx;
}

template< unsigned int VImageDimension >
const typename LabelObjectLine< VImageDimension >::IndexType &
LabelObjectLine< VImageDimension >::GetIndex() const
{
  return m_Index;
}

template< unsigned int VImageDimension >
void LabelObjectLine< VImageDimension >::SetLength(const LengthType length)
{
  m_Length = length;
}

template< unsigned int VImageDimension >
const typename LabelObjectLine< VImageDimension >::LengthType &
LabelObjectLine< VImageDimension >::GetLength() const
{
  return m_Length;
}

template< unsigned int VImageDimension >
bool LabelObjectLine< VImageDimension >::HasIndex(const IndexType idx) const
{
  // are we talking about the right line ?
  for ( unsigned int i = 1; i < ImageDimension; i++ )
    {
    if ( m_Index[i] != idx[i] )
      {
      return false;
      }
    }
  return ( idx[0] >= m_Index[0] && idx[0] < m_Index[0] + (OffsetValueType)m_Length );
}

template< unsigned int VImageDimension >
bool LabelObjectLine< VImageDimension >::IsNextIndex(const IndexType & idx) const
{
  // are we talking about the right line ?
  for ( unsigned int i = 1; i < ImageDimension; i++ )
    {
    if ( m_Index[i] != idx[i] )
      {
      return false;
      }
    }
  return idx[0] == m_Index[0] + (OffsetValueType)m_Length;
}

/**
 * This function just calls the
 * header/self/trailer virtual print methods, which can be overriden by
 * subclasses.
 */
template< unsigned int VImageDimension >
void
LabelObjectLine< VImageDimension >
::Print(std::ostream & os, Indent indent) const
{
  this->PrintHeader(os, indent);
  this->PrintSelf( os, indent.GetNextIndent() );
  this->PrintTrailer(os, indent);
}

/**
 * Define a default print header for all objects.
 */
template< unsigned int VImageDimension >
void
LabelObjectLine< VImageDimension >
::PrintHeader(std::ostream & os, Indent indent) const
{
  os << indent << " (" << this << ")\n";
}

/**
 * Define a default print body for all objects.
 */
template< unsigned int VImageDimension >
void
LabelObjectLine< VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Index: " << this->m_Index << std::endl;
  os << indent << "Length: " << this->m_Length << std::endl;
}

/**
 * Define a default print trailer for all objects.
 */
template< unsigned int VImageDimension >
void
LabelObjectLine< VImageDimension >
::PrintTrailer( std::ostream & itkNotUsed(os), Indent itkNotUsed(indent) ) const
{}
}  // namespace itk

#endif
