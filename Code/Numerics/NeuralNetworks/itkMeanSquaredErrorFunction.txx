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
template<class TMeasurementVector, class ScalarType>
MeanSquaredErrorFunction<TMeasurementVector,ScalarType>
::MeanSquaredErrorFunction()
{
}

/** Destructor */
template<class TMeasurementVector, class ScalarType>
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::~MeanSquaredErrorFunction()
{
}

/** Evaluate */
template<class TMeasurementVector, class ScalarType>
ScalarType
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::Evaluate(const TMeasurementVector& errors)  const
{
  vnl_vector <ScalarType> temp(errors.Size());
  for(unsigned int i=0; i<errors.Size(); i++)
    {
    temp[i]=errors[i];
    }
  return (temp.squared_magnitude() / temp.size());
}

/** Evaluate derivatives */
template<class TMeasurementVector, class ScalarType>
typename MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::InternalVectorType
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::EvaluateDerivative(const TMeasurementVector& errors)  const
{
  ScalarType m = static_cast<ScalarType>(2) / errors.Size();
  InternalVectorType temp(errors.Size());
  for(unsigned int i=0; i<errors.Size(); i++)
  {
     temp[i]=errors[i]*m;
  }
  return temp;
}

/** Print the object */
template<class TMeasurementVector, class ScalarType>
void  
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "MeanSquaredErrorFunction(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
