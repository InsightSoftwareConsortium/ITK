/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkFlipImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkFlipImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFlipImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(FlipImageFilter, image::F2, itkFlipImageFilterF2);
    ITK_WRAP_OBJECT1(FlipImageFilter, image::F3, itkFlipImageFilterF3);
    ITK_WRAP_OBJECT1(FlipImageFilter, image::US2, itkFlipImageFilterUS2);
    ITK_WRAP_OBJECT1(FlipImageFilter, image::US3, itkFlipImageFilterUS3);
  }
}

#endif
