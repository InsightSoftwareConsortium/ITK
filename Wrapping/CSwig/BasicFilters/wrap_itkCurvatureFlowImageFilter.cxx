/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkCurvatureFlowImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCurvatureFlowImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH MakeConsistentWrappedClasses.sh
//=================================
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkCurvatureFlowImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::F2 , image::F2 , itkCurvatureFlowImageFilterF2F2  );
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::D2 , image::D2 , itkCurvatureFlowImageFilterD2D2  );
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::UC2, image::UC2, itkCurvatureFlowImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::US2, image::US2, itkCurvatureFlowImageFilterUS2US2);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::UI2, image::UI2, itkCurvatureFlowImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::SC2, image::SC2, itkCurvatureFlowImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::SS2, image::SS2, itkCurvatureFlowImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::SI2, image::SI2, itkCurvatureFlowImageFilterSI2SI2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::F3 , image::F3 , itkCurvatureFlowImageFilterF3F3  );
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::D3 , image::D3 , itkCurvatureFlowImageFilterD3D3  );
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::UC3, image::UC3, itkCurvatureFlowImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::US3, image::US3, itkCurvatureFlowImageFilterUS3US3);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::UI3, image::UI3, itkCurvatureFlowImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::SC3, image::SC3, itkCurvatureFlowImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::SS3, image::SS3, itkCurvatureFlowImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(CurvatureFlowImageFilter, image::SI3, image::SI3, itkCurvatureFlowImageFilterSI3SI3);
  }
}
#endif
