/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkGradientMagnitudeImageFilter.cxx
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
#include "itkGradientMagnitudeImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH MakeConsistentWrappedClasses.sh
//=================================
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkGradientMagnitudeImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::F2 , image::F2 , itkGradientMagnitudeImageFilterF2F2  );
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::D2 , image::D2 , itkGradientMagnitudeImageFilterD2D2  );
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::UC2, image::UC2, itkGradientMagnitudeImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::US2, image::US2, itkGradientMagnitudeImageFilterUS2US2);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::UI2, image::UI2, itkGradientMagnitudeImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::SC2, image::SC2, itkGradientMagnitudeImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::SS2, image::SS2, itkGradientMagnitudeImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::SI2, image::SI2, itkGradientMagnitudeImageFilterSI2SI2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::F3 , image::F3 , itkGradientMagnitudeImageFilterF3F3  );
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::D3 , image::D3 , itkGradientMagnitudeImageFilterD3D3  );
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::UC3, image::UC3, itkGradientMagnitudeImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::US3, image::US3, itkGradientMagnitudeImageFilterUS3US3);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::UI3, image::UI3, itkGradientMagnitudeImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::SC3, image::SC3, itkGradientMagnitudeImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::SS3, image::SS3, itkGradientMagnitudeImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(GradientMagnitudeImageFilter, image::SI3, image::SI3, itkGradientMagnitudeImageFilterSI3SI3);
  }
}
#endif
