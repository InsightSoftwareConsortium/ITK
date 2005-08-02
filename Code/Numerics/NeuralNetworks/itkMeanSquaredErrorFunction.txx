/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaredErrorFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeanSquaredErrorFunction_txx
#define __itkMeanSquaredErrorFunction_txx

#include "itkMeanSquaredErrorFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class TVector, class ScalarType>
MeanSquaredErrorFunction<TVector,ScalarType>
::MeanSquaredErrorFunction()
{
}

/** Destructor*/
template<class TVector, class ScalarType>
MeanSquaredErrorFunction <TVector,ScalarType>
::~MeanSquaredErrorFunction()
{
}

/** Evaluate */
template<class TVector, class ScalarType>
ScalarType
MeanSquaredErrorFunction <TVector,ScalarType>
::Evaluate(const TVector& errors)  const
{
  vnl_vector <ScalarType> temp(errors.GetVnlVector());
  return (temp.squared_magnitude() / temp.size());
}

/** Evaluate derivatives */
template<class TVector, class ScalarType>
typename MeanSquaredErrorFunction <TVector,ScalarType>
::ErrorVectorType
MeanSquaredErrorFunction <TVector,ScalarType>
::EvaluateDerivative(const TVector& errors)  const
{
  ScalarType m = static_cast<ScalarType>(2) / errors.GetVectorDimension();
  return (errors * m);
}

/** Print the object */
template<class TVector, class ScalarType>
void  
MeanSquaredErrorFunction <TVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "MeanSquaredErrorFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
