/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkNormalizeImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNormalizeImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkNormalizeImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::F2, image::US2, 
                                     itkNormalizeImageFilterF2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::US2, image::F2, 
                                     itkNormalizeImageFilterUS2F2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::F2, image::F2, 
                                     itkNormalizeImageFilterF2F2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::F2, image::US2, 
                                     itkNormalizeImageFilterF2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::US2, image::US2, 
                                     itkNormalizeImageFilterUS2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::F3, image::US3, 
                                     itkNormalizeImageFilterF3US3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::F3, image::F3, 
                                     itkNormalizeImageFilterF3F3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::US3, image::US3, 
                                     itkNormalizeImageFilterUS3US3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(NormalizeImageFilter, image::US3, image::F3, 
                                     itkNormalizeImageFilterUS3F3);
  }
}


#endif
