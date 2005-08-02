/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSignedHardLimitTransferFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSignedHardLimitTransferFunction_txx
#define __itkSignedHardLimitTransferFunction_txx

#include "itkSignedHardLimitTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class ScalarType>
SignedHardLimitTransferFunction<ScalarType>
::SignedHardLimitTransferFunction()
{
}

/** Destructor */
template<class ScalarType>
SignedHardLimitTransferFunction<ScalarType>
::~SignedHardLimitTransferFunction()
{
}

/** Evaluate */
template<class ScalarType>
ScalarType
SignedHardLimitTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return 2 * (input >= 0) - 1;
}

/** Evaluate derivative */
template<class ScalarType>
ScalarType
SignedHardLimitTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  return 0;
}

/** Print the object */
template<class ScalarType>
void  
SignedHardLimitTransferFunction< ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "SignedHardLimitTransferFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace Statistics
} // end namespace itk

#endif
