/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageSliceIteratorWithIndex_txx
#define _itkImageSliceIteratorWithIndex_txx

#include "itkImageSliceIteratorWithIndex.h"

namespace itk
{



template< typename TImage >
ImageSliceIteratorWithIndex<TImage>
::ImageSliceIteratorWithIndex()
    : ImageSliceConstIteratorWithIndex<TImage>() 
{


}



template< typename TImage >
ImageSliceIteratorWithIndex<TImage>
::ImageSliceIteratorWithIndex(ImageType *ptr, const RegionType& region) :
    ImageSliceConstIteratorWithIndex<TImage>(   ptr, region ) 
{


}


 
template< typename TImage >
ImageSliceIteratorWithIndex<TImage>
::ImageSliceIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it):
                                        ImageSliceConstIteratorWithIndex<TImage>(it)
{ 
}

 
template< typename TImage >
ImageSliceIteratorWithIndex<TImage>
::ImageSliceIteratorWithIndex( const ImageSliceConstIteratorWithIndex<TImage> &it):
                                        ImageSliceConstIteratorWithIndex<TImage>(it)
{ 
}

 
template< typename TImage >
ImageSliceIteratorWithIndex<TImage> &
ImageSliceIteratorWithIndex<TImage>
::operator=( const ImageSliceConstIteratorWithIndex<TImage> &it)
{ 
  this->ImageSliceConstIteratorWithIndex<TImage>::operator=(it);
  return *this;
}



} // end namespace itk

#endif
