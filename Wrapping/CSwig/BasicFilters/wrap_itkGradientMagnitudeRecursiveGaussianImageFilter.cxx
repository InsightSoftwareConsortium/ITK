/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkGradientMagnitudeRecursiveGaussianImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkGradientMagnitudeRecursiveGaussianImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(GradientMagnitudeRecursiveGaussianImageFilter, image::F2, image::F2,
                     GradientMagnitudeRecursiveGaussianImageFilterF2F2);
    ITK_WRAP_OBJECT2(GradientMagnitudeRecursiveGaussianImageFilter, image::F3, image::F3,
                     GradientMagnitudeRecursiveGaussianImageFilterF3F3);
    ITK_WRAP_OBJECT2(GradientMagnitudeRecursiveGaussianImageFilter, image::US2, image::US2,
                     GradientMagnitudeRecursiveGaussianImageFilterUS2US2);
    ITK_WRAP_OBJECT2(GradientMagnitudeRecursiveGaussianImageFilter, image::US3, image::US3,
                     GradientMagnitudeRecursiveGaussianImageFilterUS3US3);
  }
}

#endif
