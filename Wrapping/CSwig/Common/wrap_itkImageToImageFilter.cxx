/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageToImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageToImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageToImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::F2, itkImageToImageFilterF2F2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::UC2, itkImageToImageFilterF2UC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::US2, itkImageToImageFilterF2US2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::F2, itkImageToImageFilterUS2F2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::UC2, itkImageToImageFilterUS2UC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::US2, itkImageToImageFilterUS2US2);

    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::F3, itkImageToImageFilterF3F3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::UC3, itkImageToImageFilterF3UC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::US3, itkImageToImageFilterF3US3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::F3, itkImageToImageFilterUS3F3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::UC3, itkImageToImageFilterUS3UC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::US3, itkImageToImageFilterUS3US3);

    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::UC3, itkImageToImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::F3, itkImageToImageFilterUC3F3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::SS3, itkImageToImageFilterUC3SS3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::US3, itkImageToImageFilterUC3US3);
  }
}
#endif
