/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImage);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageBase, 2, itkImageBase2);
    ITK_WRAP_OBJECT1(ImageBase, 3, itkImageBase3);
    ITK_WRAP_OBJECT2(Image, float, 2, itkImageF2);
    ITK_WRAP_OBJECT2(Image, float, 3, itkImageF3);
    ITK_WRAP_OBJECT2(Image, unsigned short, 2, itkImageUS2);
    ITK_WRAP_OBJECT2(Image, unsigned short, 3, itkImageUS3);
    
    typedef image::F2::PixelContainer::Self itkImageF_PixelContainer;
    typedef image::US2::PixelContainer::Self itkImageUS_PixelContainer;
    typedef itkImageF_PixelContainer::Pointer::SmartPointer 
    itkImageF_PixelContainer_Pointer;
    typedef itkImageUS_PixelContainer::Pointer::SmartPointer 
    itkImageUS_PixelContainer_Pointer;
  }
}

#endif
