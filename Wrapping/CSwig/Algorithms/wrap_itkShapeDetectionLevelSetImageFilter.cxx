/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkShapeDetectionLevelSetImageFilter.cxx
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
#include "itkShapeDetectionLevelSetImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkShapeDetectionLevelSetImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(ShapeDetectionLevelSetImageFilter, image::F2, image::F2,
                     itkShapeDetectionLevelSetImageFilterF2F2);
    ITK_WRAP_OBJECT2(ShapeDetectionLevelSetImageFilter, image::F3, image::F3,
                     itkShapeDetectionLevelSetImageFilterF3F3);
  }
}

#endif
