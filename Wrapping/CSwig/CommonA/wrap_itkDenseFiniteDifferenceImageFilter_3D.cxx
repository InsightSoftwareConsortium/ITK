/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkDenseFiniteDifferenceImageFilter_3D.cxx
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
  ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter_3D);
  namespace wrappers
  {
    // vector image wrapped Filters 
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter,
                     image::VF3, image::VF3, 
                     itkDenseFiniteDifferenceImageFilterVF3VF3);

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
