/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKCommonB.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE_NAME(ITK_WRAP_PACKAGE);
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(ITKKernelDeformableTransforms),
    ITK_WRAP_GROUP(ITKRigidTransforms),
    ITK_WRAP_GROUP(ITKSimilarityTransforms),
    ITK_WRAP_GROUP(itkAffineTransform),
    ITK_WRAP_GROUP(itkAzimuthElevationToCartesianTransform),
    ITK_WRAP_GROUP(itkBSplineDeformableTransform),
    ITK_WRAP_GROUP(itkIdentityTransform),
    ITK_WRAP_GROUP(itkScaleTransform),
    ITK_WRAP_GROUP(itkTranslationTransform),
    ITK_WRAP_GROUP(itkTransform),
    ITK_WRAP_GROUP(itkMatrixOffsetTransformBase),
    ITK_WRAP_GROUP(itkVersorTransformGroup)
  };
}
#endif
