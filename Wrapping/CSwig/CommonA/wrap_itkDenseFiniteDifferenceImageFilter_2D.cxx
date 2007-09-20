/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkDenseFiniteDifferenceImageFilter_2D.cxx
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
#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = 
  ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter_2D);
  namespace wrappers
  {
    // vector image wrapped Filters 
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter,
                     image::VF2, image::VF2, 
                     itkDenseFiniteDifferenceImageFilterVF2VF2);

    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F2 , image::F2 , itkDenseFiniteDifferenceImageFilterF2F2  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::D2 , image::D2 , itkDenseFiniteDifferenceImageFilterD2D2  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UC2, image::F2, itkDenseFiniteDifferenceImageFilterUC2F2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US2, image::F2, itkDenseFiniteDifferenceImageFilterUS2F2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UI2, image::F2, itkDenseFiniteDifferenceImageFilterUI2F2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SC2, image::F2, itkDenseFiniteDifferenceImageFilterSC2F2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SS2, image::F2, itkDenseFiniteDifferenceImageFilterSS2F2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SI2, image::F2, itkDenseFiniteDifferenceImageFilterSI2F2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F2 , image::VF2 ,itkDenseFiniteDifferenceImageFilterF2VF2  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US2, image::VF2, itkDenseFiniteDifferenceImageFilterUS2VF2);
  }
}

#endif
