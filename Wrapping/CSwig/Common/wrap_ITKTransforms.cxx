/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKTransforms.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkAffineTransform.h"
#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkBSplineDeformableTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkIdentityTransform.h"
#include "itkKernelTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkVersorTransform.h"
#include "itkVolumeSplineKernelTransform.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

#define ITK_WRAP_TRANSFORM_1(x) \
  ITK_WRAP_OBJECT1(x, double, itk##x)
    
#define ITK_WRAP_TRANSFORM_2(x, d1) \
  ITK_WRAP_OBJECT2(x, double, d1, itk##x##d1)
    
#define ITK_WRAP_TRANSFORM_3(x, d1, d2) \
  ITK_WRAP_OBJECT3(x, double, d1, d2, itk##x##d1##d2)
    
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKTransforms);
  namespace wrappers
  {
    ITK_WRAP_TRANSFORM_1(VersorTransform);
    ITK_WRAP_TRANSFORM_2(AffineTransform, 2);
    ITK_WRAP_TRANSFORM_2(AffineTransform, 3);
    ITK_WRAP_TRANSFORM_2(AzimuthElevationToCartesianTransform, 2);
    ITK_WRAP_TRANSFORM_2(AzimuthElevationToCartesianTransform, 3);
    ITK_WRAP_TRANSFORM_2(ElasticBodyReciprocalSplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(ElasticBodyReciprocalSplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_2(ElasticBodySplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(ElasticBodySplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_2(IdentityTransform, 2);
    ITK_WRAP_TRANSFORM_2(IdentityTransform, 3);
    ITK_WRAP_TRANSFORM_2(KernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(KernelTransform, 3);
    ITK_WRAP_TRANSFORM_2(ThinPlateR2LogRSplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(ThinPlateR2LogRSplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_2(ThinPlateSplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(ThinPlateSplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_2(VolumeSplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(VolumeSplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_3(BSplineDeformableTransform, 2, 3);
    ITK_WRAP_TRANSFORM_3(BSplineDeformableTransform, 3, 3);
  }
}
#endif
