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

ITK_WRAP_CONFIG_GROUP(itkImage);
ITK_WRAP_OBJECT_TEMPLATE_1(ImageBase2, ImageBase<2>);
ITK_WRAP_OBJECT_TEMPLATE_1(ImageBase3, ImageBase<3>);
ITK_WRAP_OBJECT_TEMPLATE_2(ImageF2, Image<float, 2>);
ITK_WRAP_OBJECT_TEMPLATE_2(ImageF3, Image<float, 3>);
ITK_WRAP_OBJECT_TEMPLATE_2(ImageUS2, Image<unsigned short, 2>);
ITK_WRAP_OBJECT_TEMPLATE_2(ImageUS3, Image<unsigned short, 3>);

namespace _cable_
{
  namespace wrappers
  {
    namespace itk
    {
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
  sizeof(ImageF_PixelContainer);
  sizeof(ImageUS_PixelContainer);
  sizeof(ImageF_PixelContainer_Pointer);
  sizeof(ImageUS_PixelContainer_Pointer);
}

#endif
