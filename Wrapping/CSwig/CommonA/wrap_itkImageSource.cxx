/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageSource.cxx
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
#include "itkVector.h"
#include "itkCovariantVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageSource);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageSource, image::F2 , itkImageSourceF2 );
    ITK_WRAP_OBJECT1(ImageSource, image::D2 , itkImageSourceD2 );
    ITK_WRAP_OBJECT1(ImageSource, image::UC2, itkImageSourceUC2);
    ITK_WRAP_OBJECT1(ImageSource, image::US2, itkImageSourceUS2);
    ITK_WRAP_OBJECT1(ImageSource, image::UI2, itkImageSourceUI2);
    ITK_WRAP_OBJECT1(ImageSource, image::UL2, itkImageSourceUL2);
    ITK_WRAP_OBJECT1(ImageSource, image::SC2, itkImageSourceSC2);
    ITK_WRAP_OBJECT1(ImageSource, image::SS2, itkImageSourceSS2);
    ITK_WRAP_OBJECT1(ImageSource, image::SI2, itkImageSourceSI2);
    ITK_WRAP_OBJECT1(ImageSource, image::VF2 , itkImageSourceVF2 );
    ITK_WRAP_OBJECT1(ImageSource, image::CVF2 , itkImageSourceCVF2 );
    ITK_WRAP_OBJECT1(ImageSource, image::CVD2 , itkImageSourceCVD2 );

    ITK_WRAP_OBJECT1(ImageSource, image::F3 , itkImageSourceF3 );
    ITK_WRAP_OBJECT1(ImageSource, image::D3 , itkImageSourceD3 );
    ITK_WRAP_OBJECT1(ImageSource, image::UC3, itkImageSourceUC3);
    ITK_WRAP_OBJECT1(ImageSource, image::US3, itkImageSourceUS3);
    ITK_WRAP_OBJECT1(ImageSource, image::UI3, itkImageSourceUI3);
    ITK_WRAP_OBJECT1(ImageSource, image::UL3, itkImageSourceUL3);
    ITK_WRAP_OBJECT1(ImageSource, image::SC3, itkImageSourceSC3);
    ITK_WRAP_OBJECT1(ImageSource, image::SS3, itkImageSourceSS3);
    ITK_WRAP_OBJECT1(ImageSource, image::SI3, itkImageSourceSI3);
    ITK_WRAP_OBJECT1(ImageSource, image::VF3 , itkImageSourceVF3 );
    ITK_WRAP_OBJECT1(ImageSource, image::CVF3 , itkImageSourceCVF3 );
    ITK_WRAP_OBJECT1(ImageSource, image::CVD3 , itkImageSourceCVD3 );
    ITK_WRAP_OBJECT1(ImageSource, image::V2F3 , itkImageSourceV2F3 );
  }
}
#endif
