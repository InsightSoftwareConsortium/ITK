/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkGrayscaleDilateImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGrayscaleDilateImageFilter.h"
#include "itkGrayscaleBallStructuringElement.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkGrayscaleDilateImageFilter);
  namespace wrappers
  {
    namespace structuringElement 
    {
      typedef itk::GrayscaleBallStructuringElement<float, 2 >::Self             F2;
      typedef itk::GrayscaleBallStructuringElement<float, 3 >::Self             F3;
      typedef itk::GrayscaleBallStructuringElement<unsigned char, 2 >::Self     UC2;
      typedef itk::GrayscaleBallStructuringElement<unsigned char, 3 >::Self     UC3;
      typedef itk::GrayscaleBallStructuringElement<unsigned short, 2 >::Self    US2;
      typedef itk::GrayscaleBallStructuringElement<unsigned short, 3 >::Self    US3;
    }

    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(GrayscaleDilateImageFilter, image::F2 , image::F2 , structuringElement::F2,   itkGrayscaleDilateImageFilterF2F2  );
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(GrayscaleDilateImageFilter, image::UC2, image::UC2, structuringElement::UC2,  itkGrayscaleDilateImageFilterUC2UC2);
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(GrayscaleDilateImageFilter, image::US2, image::US2, structuringElement::US2,  itkGrayscaleDilateImageFilterUS2US2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(GrayscaleDilateImageFilter, image::F3 , image::F3 , structuringElement::F3,   itkGrayscaleDilateImageFilterF3F3  );
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(GrayscaleDilateImageFilter, image::UC3, image::UC3, structuringElement::UC3,  itkGrayscaleDilateImageFilterUC3UC3);
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(GrayscaleDilateImageFilter, image::US3, image::US3, structuringElement::US3,  itkGrayscaleDilateImageFilterUS3US3);
  }
}


#endif
