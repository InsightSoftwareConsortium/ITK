/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelObjectLine.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelObjectLine_txx
#define __itkLabelObjectLine_txx

#include "itkLabelObjectLine.h"

namespace itk
{
template< unsigned int VImageDimension >
LabelObjectLine< VImageDimension >::LabelObjectLine(const IndexType & idx, const LengthType & length)
{
  this->SetIndex(idx);
  this->SetLength(length);
}

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
  return ( idx[0] >= m_Index[0] && idx[0] < m_Index[0] + (long)m_Length );
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
  return idx[0] == m_Index[0] + (long)m_Length;
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
