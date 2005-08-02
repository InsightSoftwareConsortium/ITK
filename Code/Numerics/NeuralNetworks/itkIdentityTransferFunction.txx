/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIdentityTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkIdentityTransferFunction_txx
#define __itkIdentityTransferFunction_txx

#include "itkIdentityTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class ScalarType>
IdentityTransferFunction<ScalarType>
::IdentityTransferFunction()
{
}

/** Destructor */
template<class ScalarType>
IdentityTransferFunction<ScalarType>
::~IdentityTransferFunction()
{
}

template<class ScalarType>
ScalarType
IdentityTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return input;
}

template<class ScalarType>
ScalarType
IdentityTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  return 1;
}

/** Print the object */
template<class ScalarType>
void  
IdentityTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "IdentityTransferFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
