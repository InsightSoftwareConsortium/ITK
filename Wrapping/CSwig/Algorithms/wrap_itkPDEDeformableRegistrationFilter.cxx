/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkPDEDeformableRegistrationFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkPDEDeformableRegistrationFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkPDEDeformableRegistrationFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT3(PDEDeformableRegistrationFilter, image::F2, image::F2, image::VF2,
                     itkPDEDeformableRegistrationFilterF2F2);
    ITK_WRAP_OBJECT3(PDEDeformableRegistrationFilter, image::F3, image::F3, image::VF3,
                     itkPDEDeformableRegistrationFilterF3F3);
    ITK_WRAP_OBJECT3(PDEDeformableRegistrationFilter, image::US2, image::US2, image::VF2,
                     itkPDEDeformableRegistrationFilterUS2US2);
    ITK_WRAP_OBJECT3(PDEDeformableRegistrationFilter, image::US3, image::US3, image::VF3,
                     itkPDEDeformableRegistrationFilterUS3US3);
  }
}

#endif
