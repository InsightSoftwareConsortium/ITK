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

template<typename TBloxPixelType, unsigned int VImageDimension, class TImageTraits>
BloxImage<TBloxPixelType, VImageDimension, TImageTraits>
::BloxImage()
{

}

template<typename TBloxPixelType, unsigned int VImageDimension, class TImageTraits>
BloxImage<TBloxPixelType, VImageDimension, TImageTraits>
::~BloxImage()
{

}

template<typename TBloxPixelType, unsigned int VImageDimension, class TImageTraits>
void
BloxImage<TBloxPixelType, VImageDimension, TImageTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
