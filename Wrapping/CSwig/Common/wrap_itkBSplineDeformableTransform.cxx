/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkBSplineDeformableTransform.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBSplineDeformableTransform.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

#define ITK_WRAP_TRANSFORM_3(x, d1, d2) \
  ITK_WRAP_OBJECT3(x, double, d1, d2, itk##x##d1##d2)
    
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkBSplineDeformableTransform);
  namespace wrappers
  {
    ITK_WRAP_TRANSFORM_3(BSplineDeformableTransform, 2, 3);
    ITK_WRAP_TRANSFORM_3(BSplineDeformableTransform, 3, 3);
  }
}
#endif
