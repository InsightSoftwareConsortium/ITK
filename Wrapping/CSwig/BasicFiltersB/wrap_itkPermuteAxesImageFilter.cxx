/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkPermuteAxesImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPermuteAxesImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkPermuteAxesImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::F2, itkPermuteAxesImageFilterF2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::F3, itkPermuteAxesImageFilterF3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::D2, itkPermuteAxesImageFilterD2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::D3, itkPermuteAxesImageFilterD3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UC2, itkPermuteAxesImageFilterUC2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UC3, itkPermuteAxesImageFilterUC3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::US2, itkPermuteAxesImageFilterUS2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::US3, itkPermuteAxesImageFilterUS3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UI2, itkPermuteAxesImageFilterUI2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UI3, itkPermuteAxesImageFilterUI3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SC2, itkPermuteAxesImageFilterSC2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SC3, itkPermuteAxesImageFilterSC3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SS2, itkPermuteAxesImageFilterSS2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SS3, itkPermuteAxesImageFilterSS3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SI2, itkPermuteAxesImageFilterSI2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SI3, itkPermuteAxesImageFilterSI3);
  }
}

#endif
