/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionIterator_txx
#define _itkImageRegionIterator_txx

#include "itkImageRegionIterator.h"

namespace itk
{



template< typename TImage >
ImageRegionIterator<TImage>
::ImageRegionIterator()
  : ImageRegionConstIterator<TImage>() 
{


}



template< typename TImage >
ImageRegionIterator<TImage>
::ImageRegionIterator(ImageType *ptr, const RegionType& region) :
  ImageRegionConstIterator<TImage>(   ptr, region ) 
{


}


 
template< typename TImage >
ImageRegionIterator<TImage>
::ImageRegionIterator( const ImageIteratorWithIndex<TImage> &it):
  ImageRegionConstIterator<TImage>(it)
{ 
}

 
template< typename TImage >
ImageRegionIterator<TImage>
::ImageRegionIterator( const ImageRegionConstIterator<TImage> &it):
  ImageRegionConstIterator<TImage>(it)
{ 
}

 
template< typename TImage >
ImageRegionIterator<TImage> &
ImageRegionIterator<TImage>
::operator=( const ImageRegionConstIterator<TImage> &it)
{ 
  this->ImageRegionConstIterator<TImage>::operator=(it);
  return *this;
}


 
template< typename TImage >
ImageRegionIterator<TImage> 
ImageRegionIterator<TImage>
::Begin() const
{ 
  return this->Superclass::Begin();
}



 
template< typename TImage >
ImageRegionIterator<TImage> 
ImageRegionIterator<TImage>
::End() const
{ 
  return this->Superclass::End();
}




} // end namespace itk

#endif
