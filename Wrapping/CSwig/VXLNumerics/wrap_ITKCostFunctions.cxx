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

ITK_WRAP_CONFIG_GROUP(ITKCostFunctions);

ITK_WRAP_OBJECT(CostFunction);
ITK_WRAP_OBJECT(SingleValuedCostFunction);

#endif
