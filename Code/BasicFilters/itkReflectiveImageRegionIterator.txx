/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkReflectiveImageRegionIterator_txx
#define _itkReflectiveImageRegionIterator_txx

#include "itkReflectiveImageRegionIterator.h"

namespace itk
{



template< typename TImage >
ReflectiveImageRegionIterator<TImage>
::ReflectiveImageRegionIterator()
    : ReflectiveImageRegionConstIterator<TImage>() 
{


}



template< typename TImage >
ReflectiveImageRegionIterator<TImage>
::ReflectiveImageRegionIterator(ImageType *ptr, const RegionType& region) :
    ReflectiveImageRegionConstIterator<TImage>(   ptr, region ) 
{


}


 
template< typename TImage >
ReflectiveImageRegionIterator<TImage>
::ReflectiveImageRegionIterator( const ImageIteratorWithIndex<TImage> &it):
                                        ReflectiveImageRegionConstIterator<TImage>(it)
{ 
}

 
template< typename TImage >
ReflectiveImageRegionIterator<TImage>
::ReflectiveImageRegionIterator( const ReflectiveImageRegionConstIterator<TImage> &it):
                                        ReflectiveImageRegionConstIterator<TImage>(it)
{ 
}

 
template< typename TImage >
ReflectiveImageRegionIterator<TImage> &
ReflectiveImageRegionIterator<TImage>
::operator=( const ReflectiveImageRegionConstIterator<TImage> &it)
{ 
  this->ReflectiveImageRegionConstIterator<TImage>::operator=(it);
  return *this;
}



} // end namespace itk

#endif
