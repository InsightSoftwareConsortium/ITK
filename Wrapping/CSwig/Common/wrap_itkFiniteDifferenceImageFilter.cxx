/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkFiniteDifferenceImageFilter.cxx
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
#include "itkFiniteDifferenceImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F2, image::F2, 
                       itkFiniteDifferenceImageFilterF2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US2, image::F2, 
                       itkFiniteDifferenceImageFilterUS2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F3, image::F3, 
                       itkFiniteDifferenceImageFilterF3F3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US3, image::F3, 
                       itkFiniteDifferenceImageFilterUS3F3);
  }
}
#endif
