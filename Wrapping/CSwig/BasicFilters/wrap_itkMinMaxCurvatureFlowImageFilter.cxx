/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkMinMaxCurvatureFlowImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH MakeConsistentWrappedClasses.sh
//=================================

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkMinMaxCurvatureFlowImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::F2 , image::F2 , itkMinMaxCurvatureFlowImageFilterF2F2  );
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::D2 , image::D2 , itkMinMaxCurvatureFlowImageFilterD2D2  );

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::F3 , image::F3 , itkMinMaxCurvatureFlowImageFilterF3F3  );
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::D3 , image::D3 , itkMinMaxCurvatureFlowImageFilterD3D3  );
  }
}
#endif
