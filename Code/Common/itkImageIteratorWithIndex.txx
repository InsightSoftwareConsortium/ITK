/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageIteratorWithIndex_txx
#define _itkImageIteratorWithIndex_txx

#include "itkImageIteratorWithIndex.h"

namespace itk
{



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage>
::ImageIteratorWithIndex()
{
}



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage>
::ImageIteratorWithIndex(const Self& it):
        ImageConstIteratorWithIndex<TImage>(it)
{
}



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage>
::ImageIteratorWithIndex(TImage *ptr, const RegionType & region ):
                                  ImageConstIteratorWithIndex<TImage>( ptr, region )
{
}
 

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage>
::ImageIteratorWithIndex(const ImageConstIteratorWithIndex<TImage> & it):
                                  ImageConstIteratorWithIndex<TImage>( it )
{
}


   
//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage> &
ImageIteratorWithIndex<TImage>
::operator=(const ImageConstIteratorWithIndex<TImage> & it)
{
  this->ImageConstIteratorWithIndex<TImage>::operator=( it );
  return *this;
} 
  

 
//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage> &
ImageIteratorWithIndex<TImage>
::operator=(const Self& it)
{
  this->ImageConstIteratorWithIndex<TImage>::operator=( it );
  return *this;
} 
  




} // end namespace itk




#endif
