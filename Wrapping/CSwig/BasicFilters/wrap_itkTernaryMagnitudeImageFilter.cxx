/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkTernaryMagnitudeImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTernaryMagnitudeImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkTernaryMagnitudeImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT4_WITH_SUPERCLASS(TernaryMagnitudeImageFilter,
                                     image::F2, 
                                     image::F2, 
                                     image::F2, 
                                     image::F2, 
                                     TernaryMagnitudeImageFilterF2F2);
    ITK_WRAP_OBJECT4_WITH_SUPERCLASS(TernaryMagnitudeImageFilter,
                                     image::F3,
                                     image::F3,
                                     image::F3,
                                     image::F3, 
                                     TernaryMagnitudeImageFilterF3F3);
    ITK_WRAP_OBJECT4_WITH_SUPERCLASS(TernaryMagnitudeImageFilter,
                                     image::US2, 
                                     image::US2, 
                                     image::US2, 
                                     image::US2, 
                                     TernaryMagnitudeImageFilterUS2US2);
    ITK_WRAP_OBJECT4_WITH_SUPERCLASS(TernaryMagnitudeImageFilter,
                                     image::US3,
                                     image::US3,
                                     image::US3,
                                     image::US3, 
                                     TernaryMagnitudeImageFilterUS3US3);
    
  }
}
#endif
