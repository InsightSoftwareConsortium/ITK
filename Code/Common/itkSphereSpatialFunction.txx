/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSpatialFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSphereSpatialFunction_txx
#define __itkSphereSpatialFunction_txx

#include "itkSphereSpatialFunction.h"

namespace itk
{

template <unsigned int VImageDimension,typename TInput>
SphereSpatialFunction<VImageDimension,TInput>
::SphereSpatialFunction()
{
  m_Radius = 1.0;

  m_Center.Fill(0.0);
}

template <unsigned int VImageDimension,typename TInput>
SphereSpatialFunction<VImageDimension,TInput>
::~SphereSpatialFunction()
{

}

template <unsigned int VImageDimension,typename TInput>
typename SphereSpatialFunction<VImageDimension,TInput>::OutputType
SphereSpatialFunction<VImageDimension,TInput>
::Evaluate(const InputType& position) const
{
  double acc = 0;

  for(unsigned int i = 0; i < VImageDimension; i++)
    {
    acc += (position[i] - m_Center[i]) * (position[i] - m_Center[i]);
    }

  acc -= m_Radius*m_Radius;

  if(acc <= 0) // inside the sphere
    {
    return 1;
    }
  else
    {
    return 0; // outside the sphere
    }
}

template <unsigned int VImageDimension,typename TInput>
void
SphereSpatialFunction<VImageDimension,TInput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i;
  os << indent << "Center: [";
  for (i=0; i < VImageDimension - 1; i++)
    {
    os << m_Center[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Radius: " << m_Radius << std::endl;
}

} // end namespace itk

#endif
