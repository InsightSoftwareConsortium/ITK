/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkInPlaceImageFilter_A.cxx
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
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkInPlaceImageFilter_A);
  namespace wrappers
  {
    //===========Same type 2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2 , image::F2 , itkInPlaceImageFilterF2F2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::D2 , image::D2 , itkInPlaceImageFilterD2D2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC2, image::UC2, itkInPlaceImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US2, image::US2, itkInPlaceImageFilterUS2US2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UI2, image::UI2, itkInPlaceImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UI2, image::F2,  itkInPlaceImageFilterUI2F2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SC2, image::SC2, itkInPlaceImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SS2, image::SS2, itkInPlaceImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SI2, image::SI2, itkInPlaceImageFilterSI2SI2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2 , image::VF2, itkInPlaceImageFilterF2VF2 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::VF2 , image::VF2, itkInPlaceImageFilterVF2VF2 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US2, image::VF2, itInPlaceeImageFilterUS2VF2);

    //===========Same type 3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3 , image::F3 , itkInPlaceImageFilterF3F3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::D3 , image::D3 , itkInPlaceImageFilterD3D3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC3, image::UC3, itkInPlaceImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US3, image::US3, itkInPlaceImageFilterUS3US3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UI3, image::UI3, itkInPlaceImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UI3, image::F3,  itkInPlaceImageFilterUI3F3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SC3, image::SC3, itkInPlaceImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SS3, image::SS3, itkInPlaceImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SI3, image::SI3, itkInPlaceImageFilterSI3SI3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3 , image::VF3, itkInPlaceImageFilterF3VF3 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::VF3 , image::VF3, itkInPlaceImageFilterVF3VF3 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US3, image::VF3, itInPlaceeImageFilterUS3VF3);

    //===========Same type 2D->3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2 , image::F3 , itkInPlaceImageFilterF2F3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::D2 , image::D3 , itkInPlaceImageFilterD2D3  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC2, image::UC3, itkInPlaceImageFilterUC2UC3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US2, image::US3, itkInPlaceImageFilterUS2US3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UI2, image::UI3, itkInPlaceImageFilterUI2UI3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SC2, image::SC3, itkInPlaceImageFilterSC2SC3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SS2, image::SS3, itkInPlaceImageFilterSS2SS3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SI2, image::SI3, itkInPlaceImageFilterSI2SI3);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F2 , image::VF3, itkInPlaceImageFilterF2VF3 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US2, image::VF3, itInPlaceeImageFilterUS2VF3);

    //===========Same type 3D->2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3 , image::F2 , itkInPlaceImageFilterF3F2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::D3 , image::D2 , itkInPlaceImageFilterD3D2  );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UC3, image::UC2, itkInPlaceImageFilterUC3UC2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US3, image::US2, itkInPlaceImageFilterUS3US2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::UI3, image::UI2, itkInPlaceImageFilterUI3UI2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SC3, image::SC2, itkInPlaceImageFilterSC3SC2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SS3, image::SS2, itkInPlaceImageFilterSS3SS2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::SI3, image::SI2, itkInPlaceImageFilterSI3SI2);
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::F3 , image::VF2, itkInPlaceImageFilterF3VF2 );
    ITK_WRAP_OBJECT2(InPlaceImageFilter, image::US3, image::VF2, itInPlaceeImageFilterUS3VF2);
  }
}
#endif
