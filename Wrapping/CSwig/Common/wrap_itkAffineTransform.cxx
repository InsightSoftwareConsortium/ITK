/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkAffineTransform.cxx
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

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

#define ITK_WRAP_TRANSFORM_2(x, d1) \
  ITK_WRAP_OBJECT2(x, double, d1, itk##x##d1)
    
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkAffineTransform);
  namespace wrappers
  {
    ITK_WRAP_TRANSFORM_2(AffineTransform, 2);
    ITK_WRAP_TRANSFORM_2(AffineTransform, 3);
  }
}
#endif
