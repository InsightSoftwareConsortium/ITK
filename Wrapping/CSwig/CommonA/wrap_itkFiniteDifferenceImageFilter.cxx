/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkFiniteDifferenceImageFilter.cxx
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
#include "itkFiniteDifferenceImageFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F2 , image::F2 , itkFiniteDifferenceImageFilterF2F2  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::D2 , image::D2 , itkFiniteDifferenceImageFilterD2D2  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UC2, image::UC2, itkFiniteDifferenceImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US2, image::US2, itkFiniteDifferenceImageFilterUS2US2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UI2, image::UI2, itkFiniteDifferenceImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SC2, image::SC2, itkFiniteDifferenceImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SS2, image::SS2, itkFiniteDifferenceImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SI2, image::SI2, itkFiniteDifferenceImageFilterSI2SI2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F2 , image::VF2 ,itkFiniteDifferenceImageFilterF2VF2 );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::VF2 , image::VF2 ,itkFiniteDifferenceImageFilterVF2VF2 );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US2, image::VF2, itkFiniteDifferenceImageFilterUS2VF2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F3 , image::F3 , itkFiniteDifferenceImageFilterF3F3  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::D3 , image::D3 , itkFiniteDifferenceImageFilterD3D3  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UC3, image::UC3, itkFiniteDifferenceImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US3, image::US3, itkFiniteDifferenceImageFilterUS3US3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UI3, image::UI3, itkFiniteDifferenceImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SC3, image::SC3, itkFiniteDifferenceImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SS3, image::SS3, itkFiniteDifferenceImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SI3, image::SI3, itkFiniteDifferenceImageFilterSI3SI3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F3 , image::VF3 ,itkFiniteDifferenceImageFilterF3VF3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::VF3 , image::VF3 ,itkFiniteDifferenceImageFilterVF3VF3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US3, image::VF3, itkFiniteDifferenceImageFilterUS3VF3);
  }
}
#endif
