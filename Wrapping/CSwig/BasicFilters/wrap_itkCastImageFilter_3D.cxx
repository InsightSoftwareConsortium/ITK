/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkCastImageFilter_3D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCastImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkCastImageFilter_3D);
  namespace wrappers
  {
    //Cast to self
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::F3,
                                     itkCastImageFilterF3F3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::D3, image::D3,
                                     itkCastImageFilterD3D3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC3, image::UC3,
                                     itkCastImageFilterUC3UC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US3, image::US3,
                                     itkCastImageFilterUS3US3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UI3, image::UI3,
                                     itkCastImageFilterUI3UI3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SC3, image::SC3,
                                     itkCastImageFilterSC3SC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SS3, image::SS3,
                                     itkCastImageFilterSS3SS3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SI3, image::SI3,
                                     itkCastImageFilterSI3SI3);

    //Double to/from float 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::D3,
                                     itkCastImageFilterF3D3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::D3, image::F3,
                                     itkCastImageFilterD3F3);

    //Unsigned char to/from float 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::UC3,
                                     itkCastImageFilterF3UC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC3, image::F3,
                                     itkCastImageFilterUC3F3);
    //Unsigned short to/from float 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::US3,
                                     itkCastImageFilterF3US3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US3, image::F3,
                                     itkCastImageFilterUS3F3);
    //Unsigned int to/from float 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::UI3,
                                     itkCastImageFilterF3UI3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UI3, image::F3,
                                     itkCastImageFilterUI3F3);

    //Signed char to/from float 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::SC3,
                                     itkCastImageFilterF3SC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SC3, image::F3,
                                     itkCastImageFilterSC3F3);
    //Signed short to/from float 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::SS3,
                                     itkCastImageFilterF3SS3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SS3, image::F3,
                                     itkCastImageFilterSS3F3);
    //Signed int to/from float 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F3, image::SI3,
                                     itkCastImageFilterF3SI3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SI3, image::F3,
                                     itkCastImageFilterSI3F3);
    //Unsigned char to/from unsigned short 3D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US3, image::UC3,
                                     itkCastImageFilterUS3UC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC3, image::US3,
                                     itkCastImageFilterUC3US3);
  }
}
#endif
