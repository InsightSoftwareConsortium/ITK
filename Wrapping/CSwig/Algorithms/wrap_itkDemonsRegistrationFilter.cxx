/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkDemonsRegistrationFilter.cxx
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
#include "itkDemonsRegistrationFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkDemonsRegistrationFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT3(DemonsRegistrationFilter, image::F2, image::F2, image::VF2,
                     itkDemonsRegistrationFilterF2F2);
    ITK_WRAP_OBJECT3(DemonsRegistrationFilter, image::F3, image::F3, image::VF3,
                     itkDemonsRegistrationFilterF3F3);
    ITK_WRAP_OBJECT3(DemonsRegistrationFilter, image::US2, image::US2, image::VF2,
                     itkDemonsRegistrationFilterUS2US2);
    ITK_WRAP_OBJECT3(DemonsRegistrationFilter, image::US3, image::US3, image::VF3,
                     itkDemonsRegistrationFilterUS3US3);
  }
}

#endif
