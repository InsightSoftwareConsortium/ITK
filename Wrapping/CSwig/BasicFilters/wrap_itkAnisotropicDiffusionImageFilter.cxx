/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkAnisotropicDiffusionImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  const char* const group = ITK_WRAP_GROUP(itkAnisotropicDiffusionImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(AnisotropicDiffusionImageFilter, image::F2, image::F2,
                                     AnisotropicDiffusionImageFilterF2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(AnisotropicDiffusionImageFilter, image::F3, image::F3,
                                     AnisotropicDiffusionImageFilterF3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(AnisotropicDiffusionImageFilter, image::US2, image::US2,
                                     AnisotropicDiffusionImageFilterUS2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(AnisotropicDiffusionImageFilter, image::US3, image::US3,
                                     AnisotropicDiffusionImageFilterUS3);
    
  }
}


#endif
