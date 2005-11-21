/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkMorphologyImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMorphologyImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"
#include "itkCSwigBinaryBallStructuringElement.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkMorphologyImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT3(MorphologyImageFilter, image::F2 , image::F2 , structuringElement::F2,   itkMorphologyImageFilterF2F2  );
    ITK_WRAP_OBJECT3(MorphologyImageFilter, image::UC2, image::UC2, structuringElement::UC2,  itkMorphologyImageFilterUC2UC2);
    ITK_WRAP_OBJECT3(MorphologyImageFilter, image::US2, image::US2, structuringElement::US2,  itkMorphologyImageFilterUS2US2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT3(MorphologyImageFilter, image::F3 , image::F3 , structuringElement::F3,   itkMorphologyImageFilterF3F3  );
    ITK_WRAP_OBJECT3(MorphologyImageFilter, image::UC3, image::UC3, structuringElement::UC3,  itkMorphologyImageFilterUC3UC3);
    ITK_WRAP_OBJECT3(MorphologyImageFilter, image::US3, image::US3, structuringElement::US3,  itkMorphologyImageFilterUS3US3);
  }
}

#endif

