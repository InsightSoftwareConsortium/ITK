/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTorusInteriorExteriorSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTorusInteriorExteriorSpatialFunction_txx
#define __itkTorusInteriorExteriorSpatialFunction_txx

#include "itkTorusInteriorExteriorSpatialFunction.h"

namespace itk
{

template <unsigned int VDimension, typename TInput>
TorusInteriorExteriorSpatialFunction<VDimension, TInput>
::TorusInteriorExteriorSpatialFunction()
{
  m_Origin.Fill(0.0);

  // These default values are not picked for any particular reason
  m_MajorRadius = 3;
  m_MinorRadius = 1;
}

template <unsigned int VDimension, typename TInput>
TorusInteriorExteriorSpatialFunction<VDimension, TInput>
::~TorusInteriorExteriorSpatialFunction()
{

}

template <unsigned int VDimension, typename TInput>
typename TorusInteriorExteriorSpatialFunction<VDimension, TInput>::OutputType
TorusInteriorExteriorSpatialFunction<VDimension, TInput>
::Evaluate(const InputType& position) const
{
  double x = position[0] - m_Origin[0];
  double y = position[1] - m_Origin[1];
  double z = position[2] - m_Origin[2];

  double k = pow(m_MajorRadius - sqrt(x*x + y*y), 2.0) + z*z;

  if( k <= (m_MinorRadius * m_MinorRadius) )
    return true;
  else
    return false;
}

template <unsigned int VDimension, typename TInput>
void
TorusInteriorExteriorSpatialFunction<VDimension, TInput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i;

  os << indent << "Origin: [";
  for (i=0; i < VDimension - 1; i++)
    {
    os << m_Origin[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Major radius: " << m_MajorRadius << std::endl;

  os << indent << "Minor radius: " << m_MinorRadius << std::endl;
}

} // end namespace itk

#endif
