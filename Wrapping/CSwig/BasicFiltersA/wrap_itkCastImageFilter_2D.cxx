/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkCastImageFilter_2D.cxx
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
  const char* const group = ITK_WRAP_GROUP(itkCastImageFilter_2D);
  namespace wrappers
  {
    //Cast to self
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::F2,
                                     itkCastImageFilterF2F2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::D2, image::D2,
                                     itkCastImageFilterD2D2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC2, image::UC2,
                                     itkCastImageFilterUC2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US2, image::US2,
                                     itkCastImageFilterUS2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UI2, image::UI2,
                                     itkCastImageFilterUI2UI2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SC2, image::SC2,
                                     itkCastImageFilterSC2SC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SS2, image::SS2,
                                     itkCastImageFilterSS2SS2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SI2, image::SI2,
                                     itkCastImageFilterSI2SI2);

    //Double to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::D2,
                                     itkCastImageFilterF2D2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::D2, image::F2,
                                     itkCastImageFilterD2F2);

    //Unsigned char to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::UC2,
                                     itkCastImageFilterF2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC2, image::F2,
                                     itkCastImageFilterUC2F2);
    //Unsigned short to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::US2,
                                     itkCastImageFilterF2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US2, image::F2,
                                     itkCastImageFilterUS2F2);
    //Unsigned int to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::UI2,
                                     itkCastImageFilterF2UI2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UI2, image::F2,
                                     itkCastImageFilterUI2F2);

    //Signed char to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::SC2,
                                     itkCastImageFilterF2SC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SC2, image::F2,
                                     itkCastImageFilterSC2F2);
    //Signed short to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::SS2,
                                     itkCastImageFilterF2SS2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SS2, image::F2,
                                     itkCastImageFilterSS2F2);
    //Signed int to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::SI2,
                                     itkCastImageFilterF2SI2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SI2, image::F2,
                                     itkCastImageFilterSI2F2);
    //Unsigned char to/from unsigned short 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US2, image::UC2,
                                     itkCastImageFilterUS2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC2, image::US2,
                                     itkCastImageFilterUC2US2);
 
  }
}
#endif
