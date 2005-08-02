/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHardLimitTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkHardLimitTransferFunction_txx
#define __itkHardLimitTransferFunction_txx

#include "itkHardLimitTransferFunction.h"

namespace itk
{
namespace Statistics
{

template<class ScalarType>
HardLimitTransferFunction< ScalarType>
::HardLimitTransferFunction()
{
}

template<class ScalarType>
HardLimitTransferFunction<ScalarType>
::~HardLimitTransferFunction()
{
}

template<class ScalarType>
ScalarType
HardLimitTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return (input >= 0);
}

template<class ScalarType>
ScalarType
HardLimitTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  return 0;
}

/** Print the object */
template<class ScalarType>
void  
HardLimitTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "HardLimitTransferFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk


#endif
