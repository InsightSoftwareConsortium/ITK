/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSigmoidTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSigmoidTransferFunction_txx
#define __itkSigmoidTransferFunction_txx

#include "itkSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class ScalarType>
SigmoidTransferFunction< ScalarType>
::SigmoidTransferFunction()
{
  m_Alpha = 1.0; 
  m_Beta = 0.0; 
  m_OutputMinimum = NumericTraits<ScalarType>::min();
  m_OutputMaximum = NumericTraits<ScalarType>::max();
}

/** Destructor */
template<class ScalarType>
SigmoidTransferFunction<ScalarType>
::~SigmoidTransferFunction()
{
}

/** Evaluate */
template<class ScalarType>
ScalarType
SigmoidTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  const ScalarType x = (static_cast<ScalarType>(input) - m_Beta) / m_Alpha;
  const ScalarType e = 1.0 / (1.0 + exp(-x));
  const ScalarType v = (m_OutputMaximum - m_OutputMinimum) * e
                     + m_OutputMinimum;
  return v;
}

/** Evaluate Derivatives */
template<class ScalarType>
ScalarType
SigmoidTransferFunction< ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return f * (1 - f);
}

/** Print the object */
template<class ScalarType>
void  
SigmoidTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "SigmoidTransferFunction(" << this << ")" << std::endl; 
  os << indent << "m_Alpha = " << m_Alpha << std::endl;
  os << indent << "m_Beta = " << m_Beta << std::endl;
  os << indent << "m_OutputMinimum = " << m_OutputMinimum << std::endl;
  os << indent << "m_OutputMaximum = " << m_OutputMaximum << std::endl;
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace Statistics
} // end namespace itk

#endif
