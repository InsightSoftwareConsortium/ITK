/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkFiniteDifferenceImageFilter_2D.cxx
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
#include "itkFiniteDifferenceImageFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter_2D);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F2 , image::F2 , itkFiniteDifferenceImageFilterF2F2  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::D2 , image::D2 , itkFiniteDifferenceImageFilterD2D2  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UC2, image::F2, itkFiniteDifferenceImageFilterUC2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US2, image::F2, itkFiniteDifferenceImageFilterUS2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UI2, image::F2, itkFiniteDifferenceImageFilterUI2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SC2, image::F2, itkFiniteDifferenceImageFilterSC2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SS2, image::F2, itkFiniteDifferenceImageFilterSS2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SI2, image::F2, itkFiniteDifferenceImageFilterSI2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F2 , image::VF2 ,itkFiniteDifferenceImageFilterF2VF2 );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::VF2 , image::VF2 ,itkFiniteDifferenceImageFilterVF2VF2 );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US2, image::VF2, itkFiniteDifferenceImageFilterUS2VF2);
  }
}
#endif
