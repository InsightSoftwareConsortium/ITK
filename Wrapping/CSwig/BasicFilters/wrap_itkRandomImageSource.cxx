/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkRandomImageSource.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRandomImageSource.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkRandomImageSource);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(RandomImageSource, image::F2, itkRandomImageSourceF2);
    ITK_WRAP_OBJECT1(RandomImageSource, image::F3, itkRandomImageSourceF3);
    ITK_WRAP_OBJECT1(RandomImageSource, image::US2, itkRandomImageSourceUS2);
    ITK_WRAP_OBJECT1(RandomImageSource, image::US3, itkRandomImageSourceUS3);
  }
}

#endif
