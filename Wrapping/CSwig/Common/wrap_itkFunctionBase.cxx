/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkFunctionBase.cxx
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
#include "itkFunctionBase.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFunctionBase);
  namespace wrappers
  {
    // wrap FunctionBase 
    ITK_WRAP_OBJECT2(FunctionBase, 
                itk::Point< float, 2>, double, itkFunctionBaseF2D);
    ITK_WRAP_OBJECT2(FunctionBase, 
                itk::Point< float, 3>, double, itkFunctionBaseF3D);
  }
}

#endif
