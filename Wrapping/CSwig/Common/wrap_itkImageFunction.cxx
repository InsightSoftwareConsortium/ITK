/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFunction.cxx
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
#include "itkImageFunction.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFunction);
  namespace wrappers
  {
    // wrap ImageFunction 
    ITK_WRAP_OBJECT2(ImageFunction, image::F2, double, itkImageFunctionF2D);
    ITK_WRAP_OBJECT2(ImageFunction, image::F3, double, itkImageFunctionF3D);
    ITK_WRAP_OBJECT2(ImageFunction, image::US2, double, itkImageFunctionUS2D);
    ITK_WRAP_OBJECT2(ImageFunction, image::US3, double, itkImageFunctionUS3D);
  }
}

#endif
