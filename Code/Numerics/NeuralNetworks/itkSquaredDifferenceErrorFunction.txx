/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSquaredDifferenceErrorFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSquaredDifferenceErrorFunction_txx
#define __itkSquaredDifferenceErrorFunction_txx

#include "itkSquaredDifferenceErrorFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class TVector, class ScalarType>
SquaredDifferenceErrorFunction<TVector,ScalarType>
::SquaredDifferenceErrorFunction()
{
}

/** Destructor */
template<class TVector, class ScalarType>
SquaredDifferenceErrorFunction <TVector,ScalarType>
::~SquaredDifferenceErrorFunction()
{
}

/** Evaluate */
template<class TVector, class ScalarType>
ScalarType
SquaredDifferenceErrorFunction <TVector,ScalarType>
::Evaluate(const TVector& errors)  const
{
  vnl_vector <ScalarType> temp(errors.GetVnlVector());
  return (temp.squared_magnitude() / 2);
}

/** Evaluate derivatives */
template<class TVector, class ScalarType>
typename SquaredDifferenceErrorFunction <TVector,ScalarType>
::ErrorVectorType
SquaredDifferenceErrorFunction <TVector,ScalarType>
::EvaluateDerivative(const TVector& errors)  const
{
  TVector diff;
  for(unsigned int i=0; i<errors.GetVectorDimension(); i++)
    {
    if ((-0.1 < errors[i]) && (errors[i] < 0.1))
      {
      diff[i]=0;
      }
    else
      {
      diff[i]=errors[i];      
      }
    }
  return (diff); //(errors);
}

/** Print the object */
template<class TVector, class ScalarType>
void  
SquaredDifferenceErrorFunction <TVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "SquaredDifferenceErrorFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace Statistics
} // end namespace itk

#endif
