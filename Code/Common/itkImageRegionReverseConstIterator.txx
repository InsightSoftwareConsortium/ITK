/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionReverseConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionReverseConstIterator_txx
#define _itkImageRegionReverseConstIterator_txx

#include "itkImageRegionReverseConstIterator.h"

namespace itk
{

template< typename TImage >
ImageRegionReverseConstIterator<TImage> 
ImageRegionReverseConstIterator<TImage>
::Begin() const
{ 
  return this->Superclass::Begin();
}



template< typename TImage >
ImageRegionReverseConstIterator<TImage> 
ImageRegionReverseConstIterator<TImage>
::End() const
{ 
  return this->Superclass::End();
}


} // end namespace itk

#endif
