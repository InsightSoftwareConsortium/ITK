/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkInPlaceImageFilter_B.cxx
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
#include "itkInPlaceImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkInPlaceImageFilter_B);
  namespace wrappers
  {
    //===== Mixed types InPlaceImageFilters 2D ImageFilters
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2,  image::UC2, itkInPlaceImageFilterF2UC2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2,  image::US2, itkInPlaceImageFilterF2US2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2,  image::UI2, itkInPlaceImageFilterF2UI2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2,  image::SC2, itkInPlaceImageFilterF2SC2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2,  image::SI2, itkInPlaceImageFilterF2SI2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2,  image::SS2, itkInPlaceImageFilterF2SS2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2 , image::D2 , itkInPlaceImageFilterF2D2   );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::D2 , image::F2 , itkInPlaceImageFilterD2F2   );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US2, image::F2,  itkInPlaceImageFilterUS2F2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US2, image::UC2,  itkInPlaceImageFilterUS2UC2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC2, image::F2,  itkInPlaceImageFilterUC2F2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC2, image::US2, itkInPlaceImageFilterUC2US2 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SC2, image::F2,  itkInPlaceImageFilterSC2F2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SS2, image::F2,  itkInPlaceImageFilterSS2F2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SI2, image::F2,  itkInPlaceImageFilterSI2F2  );
    //===== Mixed types InPlaceImageFilters 3D ImageFilters
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3,  image::UC3, itkInPlaceImageFilterF3UC3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3,  image::US3, itkInPlaceImageFilterF3US3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3,  image::UI3, itkInPlaceImageFilterF3UI3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3,  image::SC3, itkInPlaceImageFilterF3SC3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3,  image::SI3, itkInPlaceImageFilterF3SI3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3,  image::SS3, itkInPlaceImageFilterF3SS3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3 , image::D3 , itkInPlaceImageFilterF3D3   );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::D3 , image::F3 , itkInPlaceImageFilterD3F3   );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US3, image::F3,  itkInPlaceImageFilterUS3F3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US3, image::UC3,  itkInPlaceImageFilterUS3UC3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC3, image::F3,  itkInPlaceImageFilterUC3F3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC3, image::US3, itkInPlaceImageFilterUC3US3 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SC3, image::F3,  itkInPlaceImageFilterSC3F3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SS3, image::F3,  itkInPlaceImageFilterSS3F3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SI3, image::F3,  itkInPlaceImageFilterSI3F3  );
  }
}
#endif
