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
#include "itkCenteredRigid2DTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkEuler2DTransform.h"
#include "itkIdentityTransform.h"
#include "itkKernelTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRigid2DTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DTransform.h"
#include "itkScaleTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkTransform.h"
#include "itkTranslationTransform.h"
#include "itkVersorRigid3DTransform.h"
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
    ITK_WRAP_TRANSFORM_1(CenteredRigid2DTransform);
    ITK_WRAP_TRANSFORM_1(Euler2DTransform);
    ITK_WRAP_TRANSFORM_1(QuaternionRigidTransform);
    ITK_WRAP_TRANSFORM_1(Rigid2DTransform);
    ITK_WRAP_TRANSFORM_1(Rigid3DPerspectiveTransform);
    ITK_WRAP_TRANSFORM_1(Rigid3DTransform);
    ITK_WRAP_TRANSFORM_1(Similarity2DTransform);
    ITK_WRAP_TRANSFORM_1(VersorRigid3DTransform);
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
    ITK_WRAP_TRANSFORM_2(ScaleTransform, 2);
    ITK_WRAP_TRANSFORM_2(ScaleTransform, 3);
    ITK_WRAP_TRANSFORM_2(ThinPlateR2LogRSplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(ThinPlateR2LogRSplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_2(ThinPlateSplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(ThinPlateSplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_2(TranslationTransform, 2);
    ITK_WRAP_TRANSFORM_2(TranslationTransform, 3);  
    ITK_WRAP_TRANSFORM_2(VolumeSplineKernelTransform, 2);
    ITK_WRAP_TRANSFORM_2(VolumeSplineKernelTransform, 3);
    ITK_WRAP_TRANSFORM_3(BSplineDeformableTransform, 2, 3);
    ITK_WRAP_TRANSFORM_3(BSplineDeformableTransform, 3, 3);
    ITK_WRAP_TRANSFORM_3(Transform, 2, 2);
    ITK_WRAP_TRANSFORM_3(Transform, 3, 2);
    ITK_WRAP_TRANSFORM_3(Transform, 3, 3);
  }
}
#endif
