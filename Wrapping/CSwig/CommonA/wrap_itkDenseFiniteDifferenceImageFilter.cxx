/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkDenseFiniteDifferenceImageFilter.cxx
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
  ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter);
  namespace wrappers
  {
    // vector image wrapped Filters 
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter,
                     image::VF2, image::VF2, 
                     itkDenseFiniteDifferenceImageFilterVF2VF2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter,
                     image::VF3, image::VF3, 
                     itkDenseFiniteDifferenceImageFilterVF3VF3);
    
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F2 , image::F2 , itkDenseFiniteDifferenceImageFilterF2F2  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::D2 , image::D2 , itkDenseFiniteDifferenceImageFilterD2D2  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UC2, image::UC2, itkDenseFiniteDifferenceImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US2, image::US2, itkDenseFiniteDifferenceImageFilterUS2US2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UI2, image::UI2, itkDenseFiniteDifferenceImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SC2, image::SC2, itkDenseFiniteDifferenceImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SS2, image::SS2, itkDenseFiniteDifferenceImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SI2, image::SI2, itkDenseFiniteDifferenceImageFilterSI2SI2);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F2 , image::VF2 ,itkDenseFiniteDifferenceImageFilterF2VF2  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US2, image::VF2, itkDenseFiniteDifferenceImageFilterUS2VF2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F3 , image::F3 , itkDenseFiniteDifferenceImageFilterF3F3  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::D3 , image::D3 , itkDenseFiniteDifferenceImageFilterD3D3  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UC3, image::UC3, itkDenseFiniteDifferenceImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US3, image::US3, itkDenseFiniteDifferenceImageFilterUS3US3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UI3, image::UI3, itkDenseFiniteDifferenceImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SC3, image::SC3, itkDenseFiniteDifferenceImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SS3, image::SS3, itkDenseFiniteDifferenceImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SI3, image::SI3, itkDenseFiniteDifferenceImageFilterSI3SI3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F3 , image::VF3 ,itkDenseFiniteDifferenceImageFilterF3VF3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US3, image::VF3, itkDenseFiniteDifferenceImageFilterUS3VF3);
  }
}

#endif
