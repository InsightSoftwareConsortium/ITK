/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkVoronoiSegmentationImageFilter.cxx
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
#include "itkVoronoiSegmentationImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace itktraits
{
  typedef itk::DefaultDynamicMeshTraits<double,2,2,double,float,double> 
  dynamicMeshTraitDouble;
}
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkVoronoiSegmentationImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT3(PointSet,double,2,itktraits::dynamicMeshTraitDouble,itkPointSetDouble);
    ITK_WRAP_OBJECT3(Mesh,double,2,itktraits::dynamicMeshTraitDouble,itkMeshDouble);
    ITK_WRAP_OBJECT3(VoronoiSegmentationImageFilterBase,
                     image::UC2, image::UC2, image::UC2,
                     itkVoronoiSegmentationImageFilterBaseUC2UC2UC2);
    ITK_WRAP_OBJECT3(VoronoiSegmentationImageFilter, image::UC2, image::UC2, image::UC2,
                     itkVoronoiSegmentationImageFilterUC2UC2UC2);
  }
}

#endif
