/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkBinaryErodeImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"
#include "itkCSwigBinaryBallStructuringElement.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkBinaryErodeImageFilter);
  namespace wrappers
  {
    // NOTE: since both the BinaryDilateImageFilter and BinaryErodeImageFilter derive from the same superclass, only one of
    //       them should do the wrapping WITH_SUPERCLASS.
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(BinaryErodeImageFilter, image::F2 , image::F2 , structuringElement::F2,   itkBinaryErodeImageFilterF2F2  );
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(BinaryErodeImageFilter, image::UC2, image::UC2, structuringElement::UC2,  itkBinaryErodeImageFilterUC2UC2);
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(BinaryErodeImageFilter, image::US2, image::US2, structuringElement::US2,  itkBinaryErodeImageFilterUS2US2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(BinaryErodeImageFilter, image::F3 , image::F3 , structuringElement::F3,   itkBinaryErodeImageFilterF3F3  );
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(BinaryErodeImageFilter, image::UC3, image::UC3, structuringElement::UC3,  itkBinaryErodeImageFilterUC3UC3);
    ITK_WRAP_OBJECT3_WITH_SUPERCLASS(BinaryErodeImageFilter, image::US3, image::US3, structuringElement::US3,  itkBinaryErodeImageFilterUS3US3);
  }
}


#endif
