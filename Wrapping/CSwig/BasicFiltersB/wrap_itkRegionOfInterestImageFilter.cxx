/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkRegionOfInterestImageFilter.cxx
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
#include "itkRegionOfInterestImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH MakeConsistentWrappedClasses.sh
//=================================
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkRegionOfInterestImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::F2 , image::F2 , itkRegionOfInterestImageFilterF2F2  );
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::D2 , image::D2 , itkRegionOfInterestImageFilterD2D2  );
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::UC2, image::UC2, itkRegionOfInterestImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::US2, image::US2, itkRegionOfInterestImageFilterUS2US2);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::UI2, image::UI2, itkRegionOfInterestImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::SC2, image::SC2, itkRegionOfInterestImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::SS2, image::SS2, itkRegionOfInterestImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::SI2, image::SI2, itkRegionOfInterestImageFilterSI2SI2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::F3 , image::F3 , itkRegionOfInterestImageFilterF3F3  );
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::D3 , image::D3 , itkRegionOfInterestImageFilterD3D3  );
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::UC3, image::UC3, itkRegionOfInterestImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::US3, image::US3, itkRegionOfInterestImageFilterUS3US3);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::UI3, image::UI3, itkRegionOfInterestImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::SC3, image::SC3, itkRegionOfInterestImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::SS3, image::SS3, itkRegionOfInterestImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(RegionOfInterestImageFilter, image::SI3, image::SI3, itkRegionOfInterestImageFilterSI3SI3);
  }
}
#endif
