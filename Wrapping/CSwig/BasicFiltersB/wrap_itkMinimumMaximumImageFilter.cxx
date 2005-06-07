/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkMinimumMaximumImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMinimumMaximumImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkMinimumMaximumImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::F2,
                     itkMinimumMaximumImageFilterF2);
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::US2,
                     itkMinimumMaximumImageFilterUS2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::F3,
                     itkMinimumMaximumImageFilterF3);
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::US3,
                     itkMinimumMaximumImageFilterUS3);
  }
}
#endif
