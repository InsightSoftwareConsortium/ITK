/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageToImageFilter_3D.cxx
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
#include "itkVector.h"
#include "itkCovariantVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageToImageFilter_3D);
  namespace wrappers
  {
    // to self
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::F3, itkImageToImageFilterF3F3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D3, image::D3, itkImageToImageFilterD3D3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::UC3, itkImageToImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::US3, itkImageToImageFilterUS3US3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UI3, image::UI3, itkImageToImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SC3, image::SC3, itkImageToImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SS3, image::SS3, itkImageToImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SI3, image::SI3, itkImageToImageFilterSI3SI3);

    // 2D-->3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F2, image::F3, itkImageToImageFilterF2F3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D2, image::D3, itkImageToImageFilterD2D3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC2, image::UC3, itkImageToImageFilterUC2UC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US2, image::US3, itkImageToImageFilterUS2US3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UI2, image::UI3, itkImageToImageFilterUI2UI3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SC2, image::SC3, itkImageToImageFilterSC2SC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SS2, image::SS3, itkImageToImageFilterSS2SS3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SI2, image::SI3, itkImageToImageFilterSI2SI3);

    //Double to/from float 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::D3, itkImageToImageFilterF3D3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D3, image::F3, itkImageToImageFilterD3F3);

    //Unsigned char to/from float 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::UC3, itkImageToImageFilterF3UC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::F3, itkImageToImageFilterUC3F3);
    //Unsigned short to/from float 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::US3, itkImageToImageFilterF3US3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::F3, itkImageToImageFilterUS3F3);
    //Unsigned int to/from float 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::UI3, itkImageToImageFilterF3UI3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UI3, image::F3, itkImageToImageFilterUI3F3);

    //Signed char to/from float 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::SC3, itkImageToImageFilterF3SC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SC3, image::F3, itkImageToImageFilterSC3F3);
    //Signed short to/from float 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::SS3, itkImageToImageFilterF3SS3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SS3, image::F3, itkImageToImageFilterSS3F3);
    //Signed int to/from float 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::SI3, itkImageToImageFilterF3SI3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::SI3, image::F3, itkImageToImageFilterSI3F3);
    //Unsigned char to/from unsigned short 3D
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::UC3, itkImageToImageFilterUS3UC3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::UC3, image::US3, itkImageToImageFilterUC3US3);

    // float to unsigned long
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3, image::UL3, itkImageToImageFilterF3UL3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::D3, image::UL3, itkImageToImageFilterD3UL3);

    //Unsigned short to CovariantVector
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::CVD3, itkImageToImageFilterUS3CVD3);

    // Image to Image of vectors
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3 , image::VF3 ,itkImageToImageFilterF3VF3 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::VF3 , image::VF3 ,itkImageToImageFilterVF3VF3 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::VF3, itkImageToImageFilterUS3VF3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3 , image::CVF3 ,itkImageToImageFilterF3CVF3 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::CVF3, itkImageToImageFilterUS3CVF3);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::V2F3, image::V2F3, itkImageToImageFilterV2F3V2F3);

    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3 , image::VF2 ,itkImageToImageFilterF3VF2 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::VF2, itkImageToImageFilterUS3VF2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3 , image::CVF2 ,itkImageToImageFilterF3CVF2 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::US3, image::CVF2, itkImageToImageFilterUS3CVF2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::V2F3, image::V2F2, itkImageToImageFilterV2F3V2F2);
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3 , image::CVF3 ,itkImageToImageFilterF3CVF3 );
    ITK_WRAP_OBJECT2(ImageToImageFilter, image::F3 , image::CVD3 ,itkImageToImageFilterF3CVD3 );
  }
}
#endif
