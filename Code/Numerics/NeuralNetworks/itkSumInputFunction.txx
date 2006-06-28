/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSumInputFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSumInputFunction_txx
#define __itkSumInputFunction_txx

#include "itkSumInputFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class TVector, class ScalarType>
SumInputFunction<TVector,ScalarType>
::SumInputFunction()
{
  m_Size = 0;
}

/** Destructor */
template<class TVector, class ScalarType>
SumInputFunction<TVector,ScalarType>
::~SumInputFunction()
{
}

/** Set the size */
template<class TVector, class ScalarType>
void
SumInputFunction<TVector,ScalarType>
::SetSize(unsigned int n)
{
  m_Size = n;
  this->Modified();
}

/** Evaluate */
template<class TVector, class ScalarType>
ScalarType
SumInputFunction<TVector,ScalarType>
::Evaluate(const TVector& input)  const
{
  vnl_vector<ScalarType> temp(input, m_Size);
  return temp.sum();
}

/** Print the object */
template<class TVector, class ScalarType>
void  
SumInputFunction <TVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "SumInputFunction(" << this << ")" << std::endl;
  os << indent << "m_Size = " << m_Size << std::endl;
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace Statistics
} // end namespace itk

#endif
