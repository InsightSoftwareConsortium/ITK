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
    namespace point
    {
    typedef ::itk::Point< float,  2 > F2;
    typedef ::itk::Point< float,  3 > F3;
    typedef ::itk::Point< double, 2 > D2;
    typedef ::itk::Point< double, 3 > D3;
    }
    // wrap FunctionBase 
    ITK_WRAP_OBJECT2(FunctionBase, point::F2, double, itkFunctionBasePF2D);
    ITK_WRAP_OBJECT2(FunctionBase, point::F3, double, itkFunctionBasePF3D);
    ITK_WRAP_OBJECT2(FunctionBase, point::D2, double, itkFunctionBasePD2D);
    ITK_WRAP_OBJECT2(FunctionBase, point::D3, double, itkFunctionBasePD3D);

    ITK_WRAP_OBJECT2(FunctionBase, image::F2, double, itkFunctionBaseIF2D);
    ITK_WRAP_OBJECT2(FunctionBase, image::F3, double, itkFunctionBaseIF3D);
    ITK_WRAP_OBJECT2(FunctionBase, image::US2, double, itkFunctionBaseIUS2D);
    ITK_WRAP_OBJECT2(FunctionBase, image::US3, double, itkFunctionBaseIUS3D);
  }
}

#endif
