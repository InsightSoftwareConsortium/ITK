/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionReverseConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegionReverseConstIterator_txx
#define __itkImageRegionReverseConstIterator_txx

#include "itkImageRegionReverseConstIterator.h"

namespace itk
{
template< typename TImage >
ImageRegionReverseConstIterator< TImage >
ImageRegionReverseConstIterator< TImage >
::Begin() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the iterator to the beginning of the region
  it.GoToBegin();

  return it;
}

template< typename TImage >
ImageRegionReverseConstIterator< TImage >
ImageRegionReverseConstIterator< TImage >
::End() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the iterator to the end of the region
  it.GoToEnd();

  return it;
}
} // end namespace itk

#endif
