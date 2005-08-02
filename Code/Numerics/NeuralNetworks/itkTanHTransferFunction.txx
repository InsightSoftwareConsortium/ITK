/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTanHTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTanHTransferFunction_txx
#define __itkTanHTransferFunction_txx

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class ScalarType>
TanHTransferFunction<ScalarType>
::TanHTransferFunction()
{
}

/** Destructor */
template<class ScalarType>
TanHTransferFunction<ScalarType> 
::~TanHTransferFunction()
{
}

/** Evaluate */
template<class ScalarType>
ScalarType
TanHTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  ScalarType x = exp(input);
  ScalarType y = exp(-input);
  return static_cast<ScalarType>((float) (x - y) / (x + y));
}

/** Evaluate derivatives */
template<class ScalarType>
ScalarType
TanHTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input); 
  return 1 - pow(f, 2);
}

/** Print the object */
template<class ScalarType>
void  
TanHTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "TanHTransferFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace Statistics
} // end namespace itk

#endif
