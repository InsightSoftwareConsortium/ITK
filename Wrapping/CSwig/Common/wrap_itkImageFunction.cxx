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
    ITK_WRAP_OBJECT3(ImageFunction, image::F2, double, float, itkImageFunctionF2DF);
    ITK_WRAP_OBJECT3(ImageFunction, image::F3, double, float, itkImageFunctionF3DF);
    ITK_WRAP_OBJECT3(ImageFunction, image::US2, double, float, itkImageFunctionUS2DF);
    ITK_WRAP_OBJECT3(ImageFunction, image::US3, double, float, itkImageFunctionUS3DF);

    ITK_WRAP_OBJECT3(ImageFunction, image::F2, double, double, itkImageFunctionF2DD);
    ITK_WRAP_OBJECT3(ImageFunction, image::F3, double, double, itkImageFunctionF3DD);
    ITK_WRAP_OBJECT3(ImageFunction, image::US2, double, double, itkImageFunctionUS2DD);
    ITK_WRAP_OBJECT3(ImageFunction, image::US3, double, double, itkImageFunctionUS3DD);
  }
}

#endif
