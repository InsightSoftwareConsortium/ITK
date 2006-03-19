/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTanSigmoidTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTanSigmoidTransferFunction_txx
#define __itkTanSigmoidTransferFunction_txx

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class ScalarType>
TanSigmoidTransferFunction<ScalarType>
::TanSigmoidTransferFunction()
{
}

/** Destructor */
template<class ScalarType>
TanSigmoidTransferFunction<ScalarType> 
::~TanSigmoidTransferFunction()
{
}

/** Evaluate */
template<class ScalarType>
ScalarType
TanSigmoidTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return static_cast<ScalarType>((2
                                / (1 + vcl_exp(-2 * static_cast<ScalarType>(input))))
                               - 1);
}

/** Evaluate derivatives */
template<class ScalarType>
ScalarType
TanSigmoidTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input); 
  return 1 - (f * f);
}

/** Print the object */
template<class ScalarType>
void  
TanSigmoidTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "TanSigmoidTransferFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace Statistics
} // end namespace itk

#endif
