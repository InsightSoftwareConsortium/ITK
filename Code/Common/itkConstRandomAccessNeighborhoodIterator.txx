/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstRandomAccessNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConstRandomAccessNeighborhoodIterator_txx
#define _itkConstRandomAccessNeighborhoodIterator_txx
#include "itkConstRandomAccessNeighborhoodIterator.h"
namespace itk {

template<class TImage>
void
ConstRandomAccessNeighborhoodIterator<TImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  os << indent;
  os << "ConstRandomAccessNeighborhoodIterator {this= " << this << "}"
     << std::endl;

  Superclass::PrintSelf(os, indent.GetNextIndent());
}
  /*
template<class TImage>
ConstRandomAccessNeighborhoodIterator<TImage> &
ConstRandomAccessNeighborhoodIterator<TImage>
::operator+=(const OffsetType & idx)
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->End();
  OffsetValueType accumulator = 0;
  const OffsetValueType* stride = this->GetImagePointer()->GetOffsetTable();

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];
  
  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets. 
  for (i = 1; i< Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for (it = this->Begin(); it < _end; ++it)
    {
      (*it) += accumulator;
    }

  // Update loop counter values
  m_Loop += idx;

  return *this;
}

template<class TImage>
ConstRandomAccessNeighborhoodIterator<TImage> &
ConstRandomAccessNeighborhoodIterator<TImage> 
::operator-=(const OffsetType & idx)
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->End();
  OffsetValueType accumulator = 0;
  const OffsetValueType* stride = this->GetImagePointer()->GetOffsetTable();

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];
  
  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets. 
  for (i = 1; i< Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for (it = this->Begin(); it < _end; ++it)
    {
      (*it) -= accumulator;
    }

  // Update loop counter values
  m_Loop -= idx;

  return *this;
}
*/  
} // namespace itk

#endif
