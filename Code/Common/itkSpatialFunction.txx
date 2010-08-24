/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialFunction_txx
#define __itkSpatialFunction_txx

#include "itkSpatialFunction.h"

namespace itk
{
template< typename TFunctionValue, unsigned int VImageDimension, typename TInput >
SpatialFunction< TFunctionValue, VImageDimension, TInput >
::SpatialFunction()
{}

template< typename TFunctionValue, unsigned int VImageDimension, typename TInput >
SpatialFunction< TFunctionValue, VImageDimension, TInput >
::~SpatialFunction()
{}

template< typename TFunctionValue, unsigned int VImageDimension, typename TInput >
void
SpatialFunction< TFunctionValue, VImageDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
