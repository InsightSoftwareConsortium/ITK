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

} // namespace itk

#endif
