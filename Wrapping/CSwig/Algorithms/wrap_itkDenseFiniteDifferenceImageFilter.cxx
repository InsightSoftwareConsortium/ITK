/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkDenseFiniteDifferenceImageFilter.cxx
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
  const char* const group = ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter,
                     image::VF2, image::VF2, 
                     itkDenseFiniteDifferenceImageFilterVF2VF2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter,
                     image::VF3, image::VF3, 
                     itkDenseFiniteDifferenceImageFilterVF3VF3);
  }
}

#endif
