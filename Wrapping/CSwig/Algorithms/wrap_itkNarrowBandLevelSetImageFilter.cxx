/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkNarrowBandLevelSetImageFilter.cxx
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
#include "itkNarrowBandLevelSetImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkNarrowBandLevelSetImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(NarrowBandImageFilterBase, image::F2, image::F2,
                     itkNarrowBandImageFilterBaseF2F2);
    ITK_WRAP_OBJECT2(NarrowBandImageFilterBase, image::F3, image::F3,
                     itkNarrowBandImageFilterBaseF3F3);
    ITK_WRAP_OBJECT2(NarrowBandLevelSetImageFilter, image::F2, image::F2,
                     itkNarrowBandLevelSetImageFilterF2F2);
    ITK_WRAP_OBJECT2(NarrowBandLevelSetImageFilter, image::F3, image::F3,
                     itkNarrowBandLevelSetImageFilterF3F3);
  }
}

#endif
