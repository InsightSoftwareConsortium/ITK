/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionIteratorWithIndex_txx
#define _itkImageRegionIteratorWithIndex_txx

#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{


template< typename TImage >
ImageRegionIteratorWithIndex<TImage>
::ImageRegionIteratorWithIndex()
  : ImageRegionConstIteratorWithIndex<TImage>() 
{


}



template< typename TImage >
ImageRegionIteratorWithIndex<TImage>
::ImageRegionIteratorWithIndex(TImage *ptr, const RegionType& region)
  : ImageRegionConstIteratorWithIndex<TImage>(ptr, region) 
{


}


 
template< typename TImage >
ImageRegionIteratorWithIndex<TImage>
::ImageRegionIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it):
  ImageRegionConstIteratorWithIndex<TImage>(it)
{ 
}

 
template< typename TImage >
ImageRegionIteratorWithIndex<TImage>
::ImageRegionIteratorWithIndex( const ImageRegionConstIteratorWithIndex<TImage> &it):
  ImageRegionConstIteratorWithIndex<TImage>(it)
{ 
}

 
template< typename TImage >
ImageRegionIteratorWithIndex<TImage> &
ImageRegionIteratorWithIndex<TImage>
::operator=( const ImageRegionConstIteratorWithIndex<TImage> &it)
{ 
  this->ImageRegionConstIteratorWithIndex<TImage>::operator=(it);
  return *this;
}



} // end namespace itk

#endif
