/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkChangeInformationImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkChangeInformationImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkChangeInformationImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ChangeInformationImageFilter, image::F2, itkChangeInformationImageFilterF2);
    ITK_WRAP_OBJECT1(ChangeInformationImageFilter, image::F3, itkChangeInformationImageFilterF3);
    ITK_WRAP_OBJECT1(ChangeInformationImageFilter, image::US2, itkChangeInformationImageFilterUS2);
    ITK_WRAP_OBJECT1(ChangeInformationImageFilter, image::US3, itkChangeInformationImageFilterUS3);
  }
}

#endif
