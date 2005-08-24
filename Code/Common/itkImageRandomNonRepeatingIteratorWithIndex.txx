/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRandomNonRepeatingIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRandomNonRepeatingIteratorWithIndex_txx
#define _itkImageRandomNonRepeatingIteratorWithIndex_txx

#include "itkImageRandomNonRepeatingIteratorWithIndex.h"

namespace itk
{

template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex<TImage>
::ImageRandomNonRepeatingIteratorWithIndex()
  : ImageRandomNonRepeatingConstIteratorWithIndex<TImage>() 
{
}


template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex<TImage>
::ImageRandomNonRepeatingIteratorWithIndex(ImageType *ptr, const RegionType& region) :
  ImageRandomNonRepeatingConstIteratorWithIndex<TImage>(   ptr, region ) 
{
}

template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex<TImage>
::ImageRandomNonRepeatingIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it):
  ImageRandomNonRepeatingConstIteratorWithIndex<TImage>(it)
{ 
}
 
template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex<TImage>
::ImageRandomNonRepeatingIteratorWithIndex( const ImageRandomNonRepeatingConstIteratorWithIndex<TImage> &it):
  ImageRandomNonRepeatingConstIteratorWithIndex<TImage>(it)
{ 
}

 
template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex<TImage> &
ImageRandomNonRepeatingIteratorWithIndex<TImage>
::operator=( const ImageRandomNonRepeatingConstIteratorWithIndex<TImage> &it)
{ 
  this->ImageRandomNonRepeatingConstIteratorWithIndex<TImage>::operator=(it);
  return *this;
}

} // end namespace itk

#endif
