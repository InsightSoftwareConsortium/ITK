/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkAnisotropicDiffusionImageFilter_2D.cxx
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
#include "itkAnisotropicDiffusionImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkAnisotropicDiffusionImageFilter_2D);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::F2 , image::F2 , itkAnisotropicDiffusionImageFilterF2F2  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::D2 , image::D2 , itkAnisotropicDiffusionImageFilterD2D2  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UC2, image::UC2, itkAnisotropicDiffusionImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::US2, image::US2, itkAnisotropicDiffusionImageFilterUS2US2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UI2, image::UI2, itkAnisotropicDiffusionImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SC2, image::SC2, itkAnisotropicDiffusionImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SS2, image::SS2, itkAnisotropicDiffusionImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SI2, image::SI2, itkAnisotropicDiffusionImageFilterSI2SI2);
  }
}
#endif
