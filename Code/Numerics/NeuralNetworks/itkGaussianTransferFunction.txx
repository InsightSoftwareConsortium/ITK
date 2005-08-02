/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianTransferFunction_txx
#define __itkGaussianTransferFunction_txx

#include "itkGaussianTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class ScalarType>
GaussianTransferFunction<ScalarType>
::GaussianTransferFunction()
{
}

/** Destructor */
template<class ScalarType>
GaussianTransferFunction<ScalarType>
::~GaussianTransferFunction()
{
}

/** Evaluate function */
template<class ScalarType>
ScalarType
GaussianTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return static_cast<ScalarType>((exp(-1 * input * input)));
}

/** Evaluate derivatives function */
template<class ScalarType>
ScalarType
GaussianTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  return static_cast<ScalarType>(-2 * Evaluate(input) * input);
}

/** Print the object */
template<class ScalarType>
void  
GaussianTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "GaussianTransferFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
