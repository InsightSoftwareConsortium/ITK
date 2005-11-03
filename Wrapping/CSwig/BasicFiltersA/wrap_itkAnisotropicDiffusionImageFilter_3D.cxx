/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkAnisotropicDiffusionImageFilter_3D.cxx
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
  const char* const group = ITK_WRAP_GROUP(itkAnisotropicDiffusionImageFilter_3D);
  namespace wrappers
  {
    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::F3 , image::F3 , itkAnisotropicDiffusionImageFilterF3F3  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::D3 , image::D3 , itkAnisotropicDiffusionImageFilterD3D3  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UC3, image::UC3, itkAnisotropicDiffusionImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::US3, image::US3, itkAnisotropicDiffusionImageFilterUS3US3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UI3, image::UI3, itkAnisotropicDiffusionImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SC3, image::SC3, itkAnisotropicDiffusionImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SS3, image::SS3, itkAnisotropicDiffusionImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SI3, image::SI3, itkAnisotropicDiffusionImageFilterSI3SI3);
  }
}
#endif
