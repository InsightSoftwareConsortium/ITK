/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkNormalizeImageFilter.cxx
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
#include "itkNormalizeImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkNormalizeImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(NormalizeImageFilter,  image::F2,  image::F2,   itkNormalizeImageFilterF2F2);
    ITK_WRAP_OBJECT2(NormalizeImageFilter,  image::D2,  image::D2,   itkNormalizeImageFilterD2D2);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::UC2, image::UC2, itkNormalizeImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::US2, image::US2, itkNormalizeImageFilterUS2US2);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::UI2, image::UI2, itkNormalizeImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::SC2, image::SC2, itkNormalizeImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::SS2, image::SS2, itkNormalizeImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::SI2, image::SI2, itkNormalizeImageFilterSI2SI2);

    ITK_WRAP_OBJECT2(NormalizeImageFilter,  image::F3,  image::F3,   itkNormalizeImageFilterF3F3);
    ITK_WRAP_OBJECT2(NormalizeImageFilter,  image::D3,  image::D3,   itkNormalizeImageFilterD3D3);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::UC3, image::UC3, itkNormalizeImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::US3, image::US3, itkNormalizeImageFilterUS3US3);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::UI3, image::UI3, itkNormalizeImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::SC3, image::SC3, itkNormalizeImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::SS3, image::SS3, itkNormalizeImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(NormalizeImageFilter, image::SI3, image::SI3, itkNormalizeImageFilterSI3SI3);

  }
}


#endif
