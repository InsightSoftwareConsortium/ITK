/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageReverseIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageReverseIterator_txx
#define _itkImageReverseIterator_txx

#include "itkImageReverseIterator.h"

namespace itk
{



template< typename TImage >
ImageReverseIterator<TImage>
::ImageReverseIterator()
  : ImageRegionReverseConstIterator<TImage>() 
{


}



template< typename TImage >
ImageReverseIterator<TImage>
::ImageReverseIterator(ImageType *ptr, const RegionType& region) :
  ImageRegionReverseConstIterator<TImage>(   ptr, region ) 
{


}


 
template< typename TImage >
ImageReverseIterator<TImage>
::ImageReverseIterator( const ImageIteratorWithIndex<TImage> &it):
  ImageRegionReverseConstIterator<TImage>(it)
{ 
}

 
template< typename TImage >
ImageReverseIterator<TImage>
::ImageReverseIterator( const ImageRegionReverseConstIterator<TImage> &it):
  ImageRegionReverseConstIterator<TImage>(it)
{ 
}

 
template< typename TImage >
ImageReverseIterator<TImage> &
ImageReverseIterator<TImage>
::operator=( const ImageRegionReverseConstIterator<TImage> &it)
{ 
  this->ImageRegionReverseConstIterator<TImage>::operator=(it);
  return *this;
}



} // end namespace itk

#endif
