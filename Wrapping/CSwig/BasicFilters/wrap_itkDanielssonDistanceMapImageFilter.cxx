/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkDanielssonDistanceMapImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkDanielssonDistanceMapImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::F2, image::F2,
                     DanielssonDistanceMapImageFilterF2F2);
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::F2, image::US2,
                     DanielssonDistanceMapImageFilterF2US2);
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::US2, image::F2,
                     DanielssonDistanceMapImageFilterUS2F2);
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::F3, image::F3,
                     DanielssonDistanceMapImageFilterF3F3);
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::F3, image::US3,
                     DanielssonDistanceMapImageFilterF3US3);
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::US2, image::US2,
                     DanielssonDistanceMapImageFilterUS2US2);
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::US3, image::US3,
                     DanielssonDistanceMapImageFilterUS3US3);
    ITK_WRAP_OBJECT2(DanielssonDistanceMapImageFilter, image::US3, image::F3,
                     DanielssonDistanceMapImageFilterUS3F3);
  }
}
#endif
