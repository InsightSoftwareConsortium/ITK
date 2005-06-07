/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkSubtractImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSubtractImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkSubtractImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(SubtractImageFilter, image::F2,
                                     image::F2, image::F2,
                                     itkSubtractImageFilterF2F2F2);
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(SubtractImageFilter, image::US2,
                                     image::US2, image::US2,
                                     itkSubtractImageFilterUS2US2US2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(SubtractImageFilter, image::F3,
                                     image::F3, image::F3,
                                     itkSubtractImageFilterF3F3F3);
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(SubtractImageFilter, image::US3,
                                     image::US3, image::US3,
                                     itkSubtractImageFilterUS3US3US3);
  }
}
#endif
