/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKFilterBase.cxx
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
#include "itkImageToImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKFilterBase);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageSource, image::F2 , itkImageSourceF2 );
    ITK_WRAP_OBJECT1(ImageSource, image::D2 , itkImageSourceD2 );
    ITK_WRAP_OBJECT1(ImageSource, image::UC2, itkImageSourceUC2);
    ITK_WRAP_OBJECT1(ImageSource, image::US2, itkImageSourceUS2);
    ITK_WRAP_OBJECT1(ImageSource, image::UI2, itkImageSourceUI2);
    ITK_WRAP_OBJECT1(ImageSource, image::SC2, itkImageSourceSC2);
    ITK_WRAP_OBJECT1(ImageSource, image::SS2, itkImageSourceSS2);
    ITK_WRAP_OBJECT1(ImageSource, image::SI2, itkImageSourceSI2);

    ITK_WRAP_OBJECT1(ImageSource, image::F3 , itkImageSourceF3 );
    ITK_WRAP_OBJECT1(ImageSource, image::D3 , itkImageSourceD3 );
    ITK_WRAP_OBJECT1(ImageSource, image::UC3, itkImageSourceUC3);
    ITK_WRAP_OBJECT1(ImageSource, image::US3, itkImageSourceUS3);
    ITK_WRAP_OBJECT1(ImageSource, image::UI3, itkImageSourceUI3);
    ITK_WRAP_OBJECT1(ImageSource, image::SC3, itkImageSourceSC3);
    ITK_WRAP_OBJECT1(ImageSource, image::SS3, itkImageSourceSS3);
    ITK_WRAP_OBJECT1(ImageSource, image::SI3, itkImageSourceSI3);

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
  }
}
#endif
