/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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

template <unsigned int VImageDimension,typename TInput>
InteriorExteriorSpatialFunction<VImageDimension,TInput>
::InteriorExteriorSpatialFunction()
{

}

template <unsigned int VImageDimension,typename TInput>
InteriorExteriorSpatialFunction<VImageDimension,TInput>
::~InteriorExteriorSpatialFunction()
{

}

template <unsigned int VImageDimension,typename TInput>
void
InteriorExteriorSpatialFunction<VImageDimension,TInput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
