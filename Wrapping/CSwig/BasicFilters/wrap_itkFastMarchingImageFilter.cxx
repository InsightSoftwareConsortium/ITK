/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkFastMarchingImageFilter.cxx
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
#include "itkFastMarchingImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH MakeConsistentWrappedClasses.sh
//=================================
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFastMarchingImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::F2 , image::F2 , itkFastMarchingImageFilterF2F2  );
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::D2 , image::D2 , itkFastMarchingImageFilterD2D2  );
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::UC2, image::UC2, itkFastMarchingImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::US2, image::US2, itkFastMarchingImageFilterUS2US2);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::UI2, image::UI2, itkFastMarchingImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::SC2, image::SC2, itkFastMarchingImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::SS2, image::SS2, itkFastMarchingImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::SI2, image::SI2, itkFastMarchingImageFilterSI2SI2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::F3 , image::F3 , itkFastMarchingImageFilterF3F3  );
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::D3 , image::D3 , itkFastMarchingImageFilterD3D3  );
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::UC3, image::UC3, itkFastMarchingImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::US3, image::US3, itkFastMarchingImageFilterUS3US3);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::UI3, image::UI3, itkFastMarchingImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::SC3, image::SC3, itkFastMarchingImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::SS3, image::SS3, itkFastMarchingImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(FastMarchingImageFilter, image::SI3, image::SI3, itkFastMarchingImageFilterSI3SI3);
  }
}
#endif
