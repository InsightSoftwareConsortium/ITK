/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkThresholdImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkThresholdImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkThresholdImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ThresholdImageFilter, image::F2,
                                     itkThresholdImageFilterF2);
    ITK_WRAP_OBJECT1(ThresholdImageFilter, image::US2,
                                     itkThresholdImageFilterUS2);
    ITK_WRAP_OBJECT1(ThresholdImageFilter, image::F3, 
                                     itkThresholdImageFilterF3);
    ITK_WRAP_OBJECT1(ThresholdImageFilter, image::US3, 
                                     itkThresholdImageFilterUS3);
  }
}

#endif
