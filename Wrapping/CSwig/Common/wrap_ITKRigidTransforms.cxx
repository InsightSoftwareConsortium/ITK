/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKRigidTransforms.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCenteredRigid2DTransform.h"
#include "itkEuler2DTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRigid2DTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DTransform.h"
#include "itkVersorRigid3DTransform.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

#define ITK_WRAP_TRANSFORM_1(x) \
  ITK_WRAP_OBJECT1(x, double, itk##x)
    
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKRigidTransforms);
  namespace wrappers
  {
    ITK_WRAP_TRANSFORM_1(CenteredRigid2DTransform);
    ITK_WRAP_TRANSFORM_1(Euler2DTransform);
    ITK_WRAP_TRANSFORM_1(QuaternionRigidTransform);
    ITK_WRAP_TRANSFORM_1(Rigid2DTransform);
    ITK_WRAP_TRANSFORM_1(Rigid3DPerspectiveTransform);
    ITK_WRAP_TRANSFORM_1(Rigid3DTransform);
    ITK_WRAP_TRANSFORM_1(VersorRigid3DTransform);
  }
}
#endif
