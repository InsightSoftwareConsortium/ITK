/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImage_2D.cxx
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
#include "itkVector.h"
#include "itkCovariantVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImage_2D);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageBase, 2, itkImageBase2);
    ITK_WRAP_OBJECT2(Image, float, 2, itkImageF2);
    ITK_WRAP_OBJECT2(Image, double, 2, itkImageD2);
    ITK_WRAP_OBJECT2(Image, unsigned char, 2, itkImageUC2);
    ITK_WRAP_OBJECT2(Image, unsigned short, 2, itkImageUS2);
    ITK_WRAP_OBJECT2(Image, unsigned int, 2, itkImageUI2);
    ITK_WRAP_OBJECT2(Image, unsigned long, 2, itkImageUL2);
    ITK_WRAP_OBJECT2(Image, signed char, 2, itkImageSC2);
    ITK_WRAP_OBJECT2(Image, signed short, 2, itkImageSS2);
    ITK_WRAP_OBJECT2(Image, signed int, 2, itkImageSI2);

    ITK_WRAP_OBJECT2(Image, itkvector::F2, 2, itkImageVF2);
    ITK_WRAP_OBJECT2(Image, itkvector::D2, 2, itkImageVD2);
    ITK_WRAP_OBJECT2(Image, covariantvector::F2, 2, itkImageCVF2);
    ITK_WRAP_OBJECT2(Image, covariantvector::D2, 2, itkImageCVD2);


//    typedef image::F2::PixelContainer::Self itkImageF_PixelContainer;
//    typedef image::D2::PixelContainer::Self itkImageD_PixelContainer;
//    typedef image::UC2::PixelContainer::Self itkImageUC_PixelContainer;
//    typedef image::US2::PixelContainer::Self itkImageUS_PixelContainer;
//    typedef image::UI2::PixelContainer::Self itkImageUI_PixelContainer;
//    typedef image::SC2::PixelContainer::Self itkImageSC_PixelContainer;
//    typedef image::SS2::PixelContainer::Self itkImageSS_PixelContainer;
//    typedef image::SI2::PixelContainer::Self itkImageSI_PixelContainer;

//    typedef itkImageF_PixelContainer::Pointer::SmartPointer itkImageF_PixelContainer_Pointer;
//    typedef itkImageD_PixelContainer::Pointer::SmartPointer itkImageD_PixelContainer_Pointer;
//    typedef itkImageUC_PixelContainer::Pointer::SmartPointer itkImageUC_PixelContainer_Pointer;
//    typedef itkImageUS_PixelContainer::Pointer::SmartPointer itkImageUS_PixelContainer_Pointer;
//    typedef itkImageUI_PixelContainer::Pointer::SmartPointer itkImageUI_PixelContainer_Pointer;
//    typedef itkImageSC_PixelContainer::Pointer::SmartPointer itkImageSC_PixelContainer_Pointer;
//    typedef itkImageSS_PixelContainer::Pointer::SmartPointer itkImageSS_PixelContainer_Pointer;
//    typedef itkImageSI_PixelContainer::Pointer::SmartPointer itkImageSI_PixelContainer_Pointer;
  }
}

#endif
