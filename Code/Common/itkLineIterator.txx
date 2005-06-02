/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLineIterator_txx
#define _itkLineIterator_txx

#include "itkLineIterator.h"

namespace itk
{

template<class TImage>
LineIterator<TImage>
::LineIterator(ImageType *imagePtr, const IndexType &startIndex, const IndexType &endIndex):
  LineConstIterator<TImage>(imagePtr, startIndex, endIndex)
{
}


template<class TImage>
LineIterator<TImage> &
LineIterator<TImage>
::operator=(const Self & it)
{
  this->LineConstIterator<TImage>::operator=( it );
  return *this;
}

} // end namespace itk

#endif
