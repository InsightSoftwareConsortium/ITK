/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkIsolatedConnectedImageFilter.cxx
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
#include "itkIsolatedConnectedImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH MakeConsistentWrappedClasses.sh
//=================================
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkIsolatedConnectedImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::F2 , image::F2 , itkIsolatedConnectedImageFilterF2F2  );
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::D2 , image::D2 , itkIsolatedConnectedImageFilterD2D2  );
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::UC2, image::UC2, itkIsolatedConnectedImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::US2, image::US2, itkIsolatedConnectedImageFilterUS2US2);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::UI2, image::UI2, itkIsolatedConnectedImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::SC2, image::SC2, itkIsolatedConnectedImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::SS2, image::SS2, itkIsolatedConnectedImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::SI2, image::SI2, itkIsolatedConnectedImageFilterSI2SI2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::F3 , image::F3 , itkIsolatedConnectedImageFilterF3F3  );
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::D3 , image::D3 , itkIsolatedConnectedImageFilterD3D3  );
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::UC3, image::UC3, itkIsolatedConnectedImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::US3, image::US3, itkIsolatedConnectedImageFilterUS3US3);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::UI3, image::UI3, itkIsolatedConnectedImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::SC3, image::SC3, itkIsolatedConnectedImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::SS3, image::SS3, itkIsolatedConnectedImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(IsolatedConnectedImageFilter, image::SI3, image::SI3, itkIsolatedConnectedImageFilterSI3SI3);
  }
}
#endif
