/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxImage_txx
#define __itkBloxImage_txx

#include "itkBloxPixel.h"
#include "itkBloxImage.h"

namespace itk
{

template<typename TBloxPixelType, unsigned int VImageDimension>
BloxImage<TBloxPixelType, VImageDimension>
::BloxImage()
{

}

template<typename TBloxPixelType, unsigned int VImageDimension>
BloxImage<TBloxPixelType, VImageDimension>
::~BloxImage()
{

}

template<typename TBloxPixelType, unsigned int VImageDimension>
void
BloxImage<TBloxPixelType, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

template<typename TBloxPixelType, unsigned int VImageDimension>
void
BloxImage<TBloxPixelType, VImageDimension>
::EmptyImage()
{
  printf("In BloxImage::EmptyImage()\n");
  // Create an iterator to walk this image
  typedef ImageRegionIterator<Self> TImageIterator;

  TImageIterator imageIt ( this,
                           this->GetLargestPossibleRegion() );

  // Iterate through the entire image and delete the contents of each pixel
  for ( imageIt.GoToBegin(); !imageIt.IsAtEnd(); ++imageIt)
    {
    ( imageIt.Get() ).DeleteListEntries();
    }
}

} // end namespace itk

#endif
