/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImage.cxx
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

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImage);
  namespace wrappers
  {
    namespace itk
    {
      typedef ::itk::ImageBase<2> ImageBase2;
      typedef ::itk::ImageBase<3> ImageBase3;
      typedef ::itk::Image<float, 2> ImageF2;
      typedef ::itk::Image<float, 3> ImageF3;
      typedef ::itk::Image<unsigned short, 2> ImageUS2;
      typedef ::itk::Image<unsigned short, 3> ImageUS3;
      typedef ImageF2::Pointer ImageF2_Pointer;
      typedef ImageF3::Pointer ImageF3_Pointer;
      typedef ImageUS2::Pointer ImageUS2_Pointer;
      typedef ImageUS3::Pointer ImageUS3_Pointer;
      typedef ImageF2::PixelContainer ImageF_PixelContainer;
      typedef ImageUS2::PixelContainer ImageUS_PixelContainer;
      typedef ImageF_PixelContainer::Pointer ImageF_PixelContainer_Pointer;
      typedef ImageUS_PixelContainer::Pointer ImageUS_PixelContainer_Pointer;
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  sizeof(ImageBase2);
  sizeof(ImageBase3);
  sizeof(ImageF2);
  sizeof(ImageF3);
  sizeof(ImageUS2);
  sizeof(ImageUS3);
  sizeof(ImageF2_Pointer);
  sizeof(ImageF3_Pointer);
  sizeof(ImageUS2_Pointer);
  sizeof(ImageUS3_Pointer);
  sizeof(ImageF_PixelContainer);
  sizeof(ImageUS_PixelContainer);
  sizeof(ImageF_PixelContainer_Pointer);
  sizeof(ImageUS_PixelContainer_Pointer);
}

#endif
