/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionExclusionIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionExclusionIteratorWithIndex_txx
#define _itkImageRegionExclusionIteratorWithIndex_txx

#include "itkImageRegionExclusionIteratorWithIndex.h"

namespace itk
{



template< typename TImage >
ImageRegionExclusionIteratorWithIndex<TImage>
::ImageRegionExclusionIteratorWithIndex()
    : ImageRegionExclusionConstIteratorWithIndex<TImage>() 
{


}



template< typename TImage >
ImageRegionExclusionIteratorWithIndex<TImage>
::ImageRegionExclusionIteratorWithIndex(ImageType *ptr, const RegionType& region) :
    ImageRegionExclusionConstIteratorWithIndex<TImage>(   ptr, region ) 
{


}


 
template< typename TImage >
ImageRegionExclusionIteratorWithIndex<TImage>
::ImageRegionExclusionIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it):
                                        ImageRegionExclusionConstIteratorWithIndex<TImage>(it)
{ 
}

 
template< typename TImage >
ImageRegionExclusionIteratorWithIndex<TImage>
::ImageRegionExclusionIteratorWithIndex( const ImageRegionExclusionConstIteratorWithIndex<TImage> &it):
                                        ImageRegionExclusionConstIteratorWithIndex<TImage>(it)
{ 
}

 
template< typename TImage >
ImageRegionExclusionIteratorWithIndex<TImage> &
ImageRegionExclusionIteratorWithIndex<TImage>
::operator=( const ImageRegionExclusionConstIteratorWithIndex<TImage> &it)
{ 
  this->ImageRegionExclusionConstIteratorWithIndex<TImage>::operator=(it);
  return *this;
}



} // end namespace itk

#endif
