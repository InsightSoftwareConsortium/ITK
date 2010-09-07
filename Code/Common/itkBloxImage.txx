/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
template< typename TBloxPixelType, unsigned int VImageDimension >
BloxImage< TBloxPixelType, VImageDimension >
::BloxImage()
{}

template< typename TBloxPixelType, unsigned int VImageDimension >
BloxImage< TBloxPixelType, VImageDimension >
::~BloxImage()
{}

template< typename TBloxPixelType, unsigned int VImageDimension >
void
BloxImage< TBloxPixelType, VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TBloxPixelType, unsigned int VImageDimension >
void
BloxImage< TBloxPixelType, VImageDimension >
::EmptyImage()
{
  const unsigned long numberOfPixels =
    this->GetBufferedRegion().GetNumberOfPixels();

  for ( unsigned int i = 0; i < numberOfPixels; i++ )
    {
    this->GetBufferPointer()[i].DeleteListEntries();
    }
}
} // end namespace itk

#endif
