/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKCostFunctions.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCostFunction.h"
#include "itkSingleValuedCostFunction.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKCostFunctions);
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_OBJECT_TYPEDEF(CostFunction);
      ITK_WRAP_OBJECT_TYPEDEF(SingleValuedCostFunction);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_OBJECT_SIZEOF(CostFunction);
  ITK_WRAP_OBJECT_SIZEOF(SingleValuedCostFunction);
}

#endif
