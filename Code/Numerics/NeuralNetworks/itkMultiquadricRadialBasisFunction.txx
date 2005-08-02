/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiquadricRadialBasisFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMultiquadricRadialBasisFunction_txx
#define __itkMultiquadricRadialBasisFunction_txx

#include "itkMultiquadricRadialBasisFunction.h"
#include <math.h>

namespace itk
{
namespace Statistics
{

template<class ScalarType>
MultiquadricRadialBasisFunction<ScalarType>
::MultiquadricRadialBasisFunction()
{
  m_Center=0.0;
  m_Radius=1;
}

template<class ScalarType>
MultiquadricRadialBasisFunction<ScalarType>
::~MultiquadricRadialBasisFunction()
{
}

template<class ScalarType>
ScalarType
MultiquadricRadialBasisFunction<ScalarType>
::Evaluate(const ScalarType& input) const
{
  const ScalarType val = pow((input*input)+(m_Radius*m_Radius),0.5);
  return val;
}

/** Print the object */
template<class ScalarType>
void  
MultiquadricRadialBasisFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{
  os << indent << "MultiquadricRadialBasisFunction(" << this << ")" << std::endl; 
  os << indent << "m_Center = " << m_Center << std::endl;
  os << indent << "m_Radius = " << m_Radius << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
