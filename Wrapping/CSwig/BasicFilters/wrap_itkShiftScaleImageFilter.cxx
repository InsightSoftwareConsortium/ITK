/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkShiftScaleImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkShiftScaleImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkShiftScaleImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::F2, image::US2, 
                     ShiftScaleImageFilterF2US2);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::US2, image::F2, 
                     ShiftScaleImageFilterUS2F2);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::F2, image::F2, 
                     ShiftScaleImageFilterF2);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::F2, image::US2, 
                     ShiftScaleImageFilterF2US2);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::US2, image::US2, 
                     ShiftScaleImageFilterUS2US2);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::F3, image::US3, 
                     ShiftScaleImageFilterF3US3);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::F3, image::F3, 
                     ShiftScaleImageFilterF3F3);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::US3, image::US3, 
                     ShiftScaleImageFilterUS3US3);
    ITK_WRAP_OBJECT2(ShiftScaleImageFilter, image::US3, image::F3, 
                     ShiftScaleImageFilterUS3F3);
  }
}


#endif
