/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInteriorExteriorSpatialFunction_txx
#define __itkInteriorExteriorSpatialFunction_txx

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{
template< unsigned int VDimension, typename TInput >
InteriorExteriorSpatialFunction< VDimension, TInput >
::InteriorExteriorSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
InteriorExteriorSpatialFunction< VDimension, TInput >
::~InteriorExteriorSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
void
InteriorExteriorSpatialFunction< VDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
