/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricSigmoidTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricSigmoidTransferFunction_txx
#define __itkSymmetricSigmoidTransferFunction_txx

#include "itkSymmetricSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{

template<class ScalarType>
SymmetricSigmoidTransferFunction<ScalarType>
::SymmetricSigmoidTransferFunction()
{
  m_Range = 15.0;
  m_Offset = 0.1;
}

template<class ScalarType>
SymmetricSigmoidTransferFunction<ScalarType>
::~SymmetricSigmoidTransferFunction()
{
}

template<class ScalarType>
ScalarType
SymmetricSigmoidTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  ScalarType val;
  if(input<-m_Range)
    {
    val=-0.5;
    }
  else if(input>m_Range)
    {
    val=0.5;
    }
  else
    {
    val= (ScalarType)1.0/(1.0+exp(-input))-0.5;
    }
  return val;
}

/** Evaluate derivatives */
template<class ScalarType>
ScalarType
SymmetricSigmoidTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return (m_Offset+(0.25-f*f));
}


/** Print the object */
template<class ScalarType>
void  
SymmetricSigmoidTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "SumInputFunction(" << this << ")" << std::endl; 
  os << indent << "m_Range = " << m_Range << std::endl;
  os << indent << "m_Offset = " << m_Offset << std::endl;
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace Statistics
} // end namespace itk


#endif
