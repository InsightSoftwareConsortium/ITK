/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKFilterBase.cxx
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
#include "itkImageToImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

#define ITK_WRAP_IMAGE_SOURCE_TYPEDEF(x) \
  typedef ::itk::ImageSource< Image##x > ImageSource##x; \
  typedef ImageSource##x::Pointer ImageSource##x##_Pointer

#define ITK_WRAP_IMAGE_SOURCE_SIZEOF(x) \
  sizeof(ImageSource##x); \
  sizeof(ImageSource##x##_Pointer)

#define ITK_WRAP_I2I_FILTER_TYPEDEF(x, y) \
  typedef ::itk::ImageToImageFilter< Image##x , Image##y > \
          ImageToImageFilter##x##y; \
  typedef ImageToImageFilter##x##y::Pointer ImageToImageFilter##x##y##_Pointer

#define ITK_WRAP_I2I_FILTER_SIZEOF(x, y) \
  sizeof(ImageToImageFilter##x##y); \
  sizeof(ImageToImageFilter##x##y##_Pointer)

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKFilterBase);
  typedef ::itk::Image<float, 2> ImageF2;
  typedef ::itk::Image<float, 3> ImageF3;
  typedef ::itk::Image<unsigned short, 2> ImageUS2;
  typedef ::itk::Image<unsigned short, 3> ImageUS3;
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_IMAGE_SOURCE_TYPEDEF(F2);
      ITK_WRAP_IMAGE_SOURCE_TYPEDEF(F3);
      ITK_WRAP_IMAGE_SOURCE_TYPEDEF(US2);
      ITK_WRAP_IMAGE_SOURCE_TYPEDEF(US3);
      ITK_WRAP_I2I_FILTER_TYPEDEF(F2, F2);
      ITK_WRAP_I2I_FILTER_TYPEDEF(F2, US2);
      ITK_WRAP_I2I_FILTER_TYPEDEF(US2, F2);
      ITK_WRAP_I2I_FILTER_TYPEDEF(US2, US2);
      ITK_WRAP_I2I_FILTER_TYPEDEF(F3, F3);
      ITK_WRAP_I2I_FILTER_TYPEDEF(F3, US3);
      ITK_WRAP_I2I_FILTER_TYPEDEF(US3, F3);
      ITK_WRAP_I2I_FILTER_TYPEDEF(US3, US3);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_IMAGE_SOURCE_SIZEOF(F2);
  ITK_WRAP_IMAGE_SOURCE_SIZEOF(F3);
  ITK_WRAP_IMAGE_SOURCE_SIZEOF(US2);
  ITK_WRAP_IMAGE_SOURCE_SIZEOF(US3);
  ITK_WRAP_I2I_FILTER_SIZEOF(F2, F2);
  ITK_WRAP_I2I_FILTER_SIZEOF(F2, US2);
  ITK_WRAP_I2I_FILTER_SIZEOF(US2, F2);
  ITK_WRAP_I2I_FILTER_SIZEOF(US2, US2);
  ITK_WRAP_I2I_FILTER_SIZEOF(F3, F3);
  ITK_WRAP_I2I_FILTER_SIZEOF(F3, US3);
  ITK_WRAP_I2I_FILTER_SIZEOF(US3, F3);
  ITK_WRAP_I2I_FILTER_SIZEOF(US3, US3);
}

#endif
