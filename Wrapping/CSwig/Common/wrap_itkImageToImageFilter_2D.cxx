/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageToImageFilter_2D.cxx
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
  const char* const group = ITK_WRAP_GROUP(itkImageToImageFilter_2D);
  namespace wrappers
  {
    // to self
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::F2, itkImageToImageFilterF2F2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D2, image::D2, itkImageToImageFilterD2D2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC2, image::UC2, itkImageToImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::US2, itkImageToImageFilterUS2US2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UI2, image::UI2, itkImageToImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SC2, image::SC2, itkImageToImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SS2, image::SS2, itkImageToImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SI2, image::SI2, itkImageToImageFilterSI2SI2);

    // 3D --> 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::F2, itkImageToImageFilterF3F2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D3, image::D2, itkImageToImageFilterD3D2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::UC2, itkImageToImageFilterUC3UC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::US2, itkImageToImageFilterUS3US2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UI3, image::UI2, itkImageToImageFilterUI3UI2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SC3, image::SC2, itkImageToImageFilterSC3SC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SS3, image::SS2, itkImageToImageFilterSS3SS2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SI3, image::SI2, itkImageToImageFilterSI3SI2);

    //Double to/from float 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::D2, itkImageToImageFilterF2D2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D2, image::F2, itkImageToImageFilterD2F2);

    //Unsigned char to/from float 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::UC2, itkImageToImageFilterF2UC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC2, image::F2, itkImageToImageFilterUC2F2);
    //Unsigned short to/from float 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::US2, itkImageToImageFilterF2US2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::F2, itkImageToImageFilterUS2F2);
    //Unsigned int to/from float 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::UI2, itkImageToImageFilterF2UI2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UI2, image::F2, itkImageToImageFilterUI2F2);

    //Signed char to/from float 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::SC2, itkImageToImageFilterF2SC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SC2, image::F2, itkImageToImageFilterSC2F2);
    //Signed short to/from float 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::SS2, itkImageToImageFilterF2SS2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SS2, image::F2, itkImageToImageFilterSS2F2);
    //Signed int to/from float 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::SI2, itkImageToImageFilterF2SI2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SI2, image::F2, itkImageToImageFilterSI2F2);
    //Unsigned char to/from unsigned short 2D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::UC2, itkImageToImageFilterUS2UC2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC2, image::US2, itkImageToImageFilterUC2US2);

    //Unsigned short to CovariantVector
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::CVD2, itkImageToImageFilterUS2CVD2);

    // float to unsigned long
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::UL2, itkImageToImageFilterF2UL2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D2, image::UL2, itkImageToImageFilterD2UL2);
    

    // Image to Image of vectors
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2 , image::VF2 ,itkImageToImageFilterF2VF2 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::VF2, itkImageToImageFilterUS2VF2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2 , image::CVF2 ,itkImageToImageFilterF2CVF2 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2 , image::CVD2 ,itkImageToImageFilterF2CVD2 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::CVF2, itkImageToImageFilterUS2CVF2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::V2F2, image::V2F2, itkImageToImageFilterV2F2V2F2);

    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2 , image::VF3 ,itkImageToImageFilterF2VF3 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::VF3, itkImageToImageFilterUS2VF3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2 , image::CVF3 ,itkImageToImageFilterF2CVF3 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::CVF3, itkImageToImageFilterUS2CVF3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::V2F2, image::V2F3, itkImageToImageFilterV2F2V2F3);
  }
}
#endif
