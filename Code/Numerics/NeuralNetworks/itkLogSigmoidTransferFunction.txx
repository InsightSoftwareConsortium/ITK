/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLogSigmoidTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLogSigmoidTransferFunction_txx
#define __itkLogSigmoidTransferFunction_txx

#include "itkLogSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class ScalarType>
LogSigmoidTransferFunction<ScalarType>
::LogSigmoidTransferFunction()
{
}

/** Destructor */
template<class ScalarType>
LogSigmoidTransferFunction<ScalarType>
::~LogSigmoidTransferFunction()
{
}

/** Evaluate */
template<class ScalarType>
ScalarType
LogSigmoidTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  const ScalarType v = 1.0 / (1.0 + exp(-input));
  return v;
}

/** Evaluate derivatives */
template<class ScalarType>
ScalarType
LogSigmoidTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return f * (1 - f);
}

/** Print the object */
template<class ScalarType>
void  
LogSigmoidTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "LogSigmoidTransferFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
