/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkGradientRecursiveGaussianImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkGradientRecursiveGaussianImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(GradientRecursiveGaussianImageFilter, image::F2,
                     itkGradientRecursiveGaussianImageFilterF2);
    ITK_WRAP_OBJECT1(GradientRecursiveGaussianImageFilter, image::F3,
                     itkGradientRecursiveGaussianImageFilterF3);
    ITK_WRAP_OBJECT1(GradientRecursiveGaussianImageFilter, image::US2,
                     itkGradientRecursiveGaussianImageFilterUS2);
    ITK_WRAP_OBJECT1(GradientRecursiveGaussianImageFilter, image::US3,
                     itkGradientRecursiveGaussianImageFilterUS3);
  }
}

#endif
