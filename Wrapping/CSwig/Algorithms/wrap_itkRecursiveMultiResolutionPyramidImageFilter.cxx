/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkRecursiveMultiResolutionPyramidImageFilter.cxx
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
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkRecursiveMultiResolutionPyramidImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RecursiveMultiResolutionPyramidImageFilter, image::F2, image::F2,
                     itkRecursiveMultiResolutionPyramidImageFilterF2F2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RecursiveMultiResolutionPyramidImageFilter, image::F3, image::F3,
                     itkRecursiveMultiResolutionPyramidImageFilterF3F3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RecursiveMultiResolutionPyramidImageFilter, image::US2, image::US2,
                     itkRecursiveMultiResolutionPyramidImageFilterUS2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RecursiveMultiResolutionPyramidImageFilter, image::US3, image::US3,
                     itkRecursiveMultiResolutionPyramidImageFilterUS3US3);
  }
}

#endif
